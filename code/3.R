# ======================================================
# Final No-Warning Version: PDF-Only Rendering
# ======================================================

library(tidyverse)
library(igraph)
library(scales)
library(readr)
library(viridisLite)
library(here)

# 🚫 Disable RStudio Plots Pane (avoid redraw incomplete)
options(device = function(...) { pdf(...) })

# ------------------------------
# Output PDF
# ------------------------------
dir.create("Plots", showWarnings = FALSE)
pdf("Plots/3_full_network_analysis.pdf", width = 11, height = 8.5)

cat("=== PDF output started ===\n\n")

# ------------------------------
# Data paths
# ------------------------------
files <- list(
  fat      = here("data", "Fat_Supply_Quantity_Data.csv"),
  kcal     = here("data", "Food_Supply_kcal_Data.csv"),
  quantity = here("data", "Food_Supply_Quantity_kg_Data.csv"),
  protein  = here("data", "Protein_Supply_Quantity_Data.csv")
)

similarity_method <- "pearson"
threshold_percentile <- 0.8
min_avg_degree_target <- 1.0
relax_steps <- c(0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.1, 0)
set.seed(1234)

smart_read <- function(path) {
  r1 <- try(read_csv(path, show_col_types = FALSE), silent = TRUE)
  if (!inherits(r1, "try-error")) return(r1)
  r2 <- try(read_csv2(path, show_col_types = FALSE), silent = TRUE)
  if (!inherits(r2, "try-error")) return(r2)
  r3 <- try(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
  if (!inherits(r3, "try-error")) return(as_tibble(r3))
  stop("Cannot read file: ", path)
}

build_network_verbose <- function(file_path, similarity_method,
                                  threshold_percentile,
                                  min_avg_degree_target,
                                  relax_steps) {
  
  df <- smart_read(file_path)
  countries <- as.character(df$Country)
  df <- df %>% distinct(Country, .keep_all = TRUE)
  
  meta_cols <- c("Country","Obesity","Undernourished","Confirmed","Deaths",
                 "Recovered","Active","Population","Unit (all except Population)")
  
  nutrient_cols <- setdiff(colnames(df), meta_cols)
  
  nutrient_data <- df %>% select(all_of(nutrient_cols)) %>%
    mutate(across(everything(), as.numeric))
  
  const_cols <- sapply(nutrient_data, function(x) all(is.na(x)) || sd(x, na.rm=TRUE)==0)
  nutrient_data <- nutrient_data %>% select(-all_of(which(const_cols)))
  
  nutrient_data <- nutrient_data %>% mutate(across(everything(),
                                                   ~ ifelse(is.na(.), mean(., na.rm=TRUE), .)))
  
  nutrient_scaled <- scale(nutrient_data)
  
  sim_mat <- cor(t(nutrient_scaled), use="pairwise.complete.obs")
  sim_mat[is.na(sim_mat)] <- 0
  rownames(sim_mat) <- colnames(sim_mat) <- countries
  
  upper_vals <- sim_mat[upper.tri(sim_mat)]
  
  for (p in relax_steps) {
    thr <- quantile(upper_vals, probs=p, na.rm=TRUE)
    adj_mat <- sim_mat
    adj_mat[adj_mat < thr] <- 0
    g <- graph_from_adjacency_matrix(adj_mat, mode="undirected", weighted=TRUE)
    if (mean(degree(g)) >= min_avg_degree_target) break
  }
  
  list(graph = g, sim_matrix = sim_mat, countries = countries)
}

# Build networks
networks <- list()
for (nm in names(files)) {
  networks[[nm]] <- build_network_verbose(files[[nm]], similarity_method,
                                          threshold_percentile,
                                          min_avg_degree_target,
                                          relax_steps)
}

# Structural metrics
get_structural_metrics <- function(g) {
  comps <- components(g)
  giant <- induced_subgraph(g, which(comps$membership == which.max(comps$csize)))
  
  list(
    density = graph.density(g),
    avg_degree = mean(degree(g)),
    transitivity = transitivity(g),
    avg_path_length = if (vcount(giant)>1) mean_distance(giant) else NA,
    degree_sd = sd(degree(g)),
    membership = membership(cluster_louvain(g)),
    community_sizes = sizes(cluster_louvain(g)),
    modularity = modularity(cluster_louvain(g))
  )
}

struct_metrics <- map(networks, ~ get_structural_metrics(.x$graph))

struct_table <- tibble()
for (nm in names(struct_metrics)) {
  s <- struct_metrics[[nm]]
  struct_table <- bind_rows(struct_table,
                            tibble(
                              dimension = nm,
                              network_density = s$density,
                              average_degree = s$avg_degree,
                              transitivity = s$transitivity,
                              avg_path_length = s$avg_path_length,
                              degree_sd = s$degree_sd,
                              modularity = s$modularity,
                              n_communities = length(s$community_sizes)
                            ))
}

# === Plot 1: Structural Metrics ===
metrics_long <- struct_table %>%
  pivot_longer(-dimension, names_to="metric", values_to="value")

print(
  ggplot(metrics_long, aes(x=dimension, y=value, fill=dimension)) +
    geom_col(show.legend=FALSE) +
    facet_wrap(~metric, scales="free_y") +
    theme_minimal() +
    labs(title="Network Structural Metrics Across Dimensions")
)

# === Plot 2: Community Size Comparison ===
comm_size_df <- map_dfr(names(struct_metrics), function(nm) {
  tibble(
    dimension = nm,
    community = seq_along(struct_metrics[[nm]]$community_sizes),
    size = as.integer(struct_metrics[[nm]]$community_sizes)
  )
})

print(
  ggplot(comm_size_df, aes(x=factor(community), y=size, fill=dimension)) +
    geom_col(position="dodge") +
    facet_wrap(~dimension) +
    theme_minimal() +
    labs(title="Community Size Distribution")
)

# === Plot 3–6 Networks ===
par(mfrow=c(2,2), mar=c(1,1,3,1))
for (nm in names(networks)) {
  g <- networks[[nm]]$graph
  mem <- struct_metrics[[nm]]$membership
  V(g)$color <- scales::hue_pal()(length(unique(mem)))[as.factor(mem)]
  plot(g, vertex.size=4, vertex.label=NA, main=toupper(nm), layout=layout_with_fr)
}
par(mfrow=c(1,1))

# Close PDF
dev.off()
cat("\n=== PDF successfully saved ===\n")