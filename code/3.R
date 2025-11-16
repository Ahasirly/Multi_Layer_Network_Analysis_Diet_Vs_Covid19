# === Simplified version: Dietary network comparison (fat / kcal / quantity / protein) ===
# Run directly in RStudio; results will be displayed in console and Plots panel

library(tidyverse)
library(igraph)
library(scales)
library(readr)
library(viridisLite)

# ------------------------------
# Configuration parameters (adjustable)
# ------------------------------
data_dir <- "C:/Users/grizz/Downloads/archive (3)"
files <- list(
  fat = file.path(data_dir, "Fat_Supply_Quantity_Data.csv"),
  kcal = file.path(data_dir, "Food_Supply_kcal_Data.csv"),
  quantity = file.path(data_dir, "Food_Supply_Quantity_kg_Data.csv"),
  protein = file.path(data_dir, "Protein_Supply_Quantity_Data.csv")
)

similarity_method <- "pearson"  # Alternative: "cosine"
threshold_percentile <- 0.8
min_avg_degree_target <- 1.0
relax_steps <- c(0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.1, 0)
set.seed(1234)  # Ensure reproducibility

# ------------------------------
# Utility function: Robust data reading
# ------------------------------
smart_read <- function(path) {
  r1 <- try(read_csv(path, show_col_types = FALSE), silent = TRUE)
  if (!inherits(r1, "try-error")) return(r1)
  r2 <- try(read_csv2(path, show_col_types = FALSE), silent = TRUE)
  if (!inherits(r2, "try-error")) return(r2)
  r3 <- try(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
  if (!inherits(r3, "try-error")) return(as_tibble(r3))
  stop("Cannot read file: ", path)
}

# ------------------------------
# Network construction function (with detailed logging)
# ------------------------------
build_network_verbose <- function(file_path, similarity_method = "pearson",
                                  threshold_percentile, min_avg_degree_target, relax_steps) {
  similarity_method <- match.arg(similarity_method)
  cat("-> Reading:", basename(file_path), "\n")
  df <- smart_read(file_path)
  cat("   Data size: rows=", nrow(df), " cols=", ncol(df), "\n")
  
  if (!"Country" %in% colnames(df)) stop("Missing 'Country' column")
  countries <- as.character(df$Country)
  if (any(duplicated(countries))) {
    warning("Duplicated country names; keeping first occurrence")
    df <- df %>% distinct(Country, .keep_all = TRUE)
    countries <- as.character(df$Country)
  }
  
  # Filter nutrient columns
  meta_cols <- c("Country","Obesity","Undernourished","Confirmed","Deaths","Recovered","Active","Population","Unit (all except Population)")
  nutrient_cols <- setdiff(colnames(df), meta_cols)
  cat("   Detected nutrient columns:", length(nutrient_cols), "\n")
  if (length(nutrient_cols) == 0) stop("No valid nutrient columns found")
  
  # Process nutrient data
  nutrient_data <- df %>% select(all_of(nutrient_cols)) %>% mutate(across(everything(), as.numeric))
  const_cols <- sapply(nutrient_data, function(x) all(is.na(x)) || (sd(x, na.rm = TRUE) == 0))
  if (any(const_cols)) {
    cat("   Removing constant/empty columns:", paste(names(nutrient_data)[const_cols], collapse = ", "), "\n")
    nutrient_data <- nutrient_data %>% select(-which(const_cols))
  }
  if (ncol(nutrient_data) == 0) stop("No usable nutrient columns remaining")
  
  # Impute missing values and standardize
  if (sum(is.na(nutrient_data)) > 0) {
    cat("   Imputing missing values with column means\n")
    nutrient_data <- nutrient_data %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  }
  nutrient_scaled <- scale(nutrient_data)
  
  # Calculate similarity matrix
  if (similarity_method == "pearson") {
    sim_mat <- cor(t(nutrient_scaled), use = "pairwise.complete.obs")
  } else {
    nm <- nutrient_scaled
    sim_mat <- tcrossprod(nm) / (sqrt(rowSums(nm^2) %*% t(rowSums(nm^2))))
  }
  sim_mat[is.na(sim_mat) | is.nan(sim_mat) | is.infinite(sim_mat)] <- 0
  rownames(sim_mat) <- colnames(sim_mat) <- countries
  
  # Automatically adjust threshold to ensure network validity
  upper_vals <- sim_mat[upper.tri(sim_mat)]
  cat("   Similarity matrix stats: min=", round(min(upper_vals, na.rm = TRUE), 4),
      " q25=", round(quantile(upper_vals,0.25,na.rm=TRUE),4),
      " median=", round(median(upper_vals,na.rm=TRUE),4),
      " q75=", round(quantile(upper_vals,0.75,na.rm=TRUE),4),
      " max=", round(max(upper_vals, na.rm=TRUE),4), "\n")
  
  chosen_threshold <- NULL
  final_adj <- NULL
  for (p in relax_steps) {
    thr <- quantile(upper_vals, probs = p, na.rm = TRUE)
    adj_mat <- sim_mat
    adj_mat[adj_mat < thr] <- 0
    g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE, diag = FALSE)
    vcnt <- vcount(g); ecnt <- ecount(g)
    avg_deg <- if (vcnt>0) mean(degree(g)) else 0
    cat(sprintf("    Trying threshold p=%.2f: nodes=%d edges=%d avg_degree=%.3f\n", p, vcnt, ecnt, avg_deg))
    if (vcnt == 0) next
    if (avg_deg >= min_avg_degree_target || p == tail(relax_steps, 1)) {
      chosen_threshold <- thr
      final_adj <- adj_mat
      break
    }
  }
  if (is.null(final_adj)) stop("Failed to construct valid network")
  
  graph <- graph_from_adjacency_matrix(final_adj, mode = "undirected", weighted = TRUE, diag = FALSE)
  cat("   Selected threshold:", round(chosen_threshold, 4), "nodes:", vcount(graph), "edges:", ecount(graph), "\n\n")
  list(graph = graph, sim_matrix = sim_mat, adj_matrix = final_adj, countries = countries)
}

# ------------------------------
# Build networks
# ------------------------------
cat("==== Starting network construction ====\n")
networks <- list()
for (nm in names(files)) {
  cat("Processing", nm, "dimension...\n")
  networks[[nm]] <- tryCatch({
    build_network_verbose(files[[nm]], similarity_method = similarity_method,
                          threshold_percentile = threshold_percentile,
                          min_avg_degree_target = min_avg_degree_target,
                          relax_steps = relax_steps)
  }, error = function(e) {
    warning(nm, " network construction failed: ", e$message)
    NULL
  })
}
networks <- networks[!sapply(networks, is.null)]
if (length(networks) == 0) stop("All network constructions failed. Check file paths and data.")

# ------------------------------
# Calculate structural metrics and communities
# ------------------------------
get_structural_metrics <- function(g) {
  dens <- graph.density(g)
  avg_deg <- mean(degree(g))
  trans <- transitivity(g, type = "global")
  comps <- components(g)
  giant_idx <- which.max(comps$csize)
  giant <- induced_subgraph(g, which(comps$membership == giant_idx))
  apl <- if (vcount(giant) > 1) mean_distance(giant, directed = FALSE) else NA
  deg_sd <- sd(degree(g))
  assort <- tryCatch(assortativity_degree(g), error = function(e) NA)
  btw_sd <- sd(betweenness(g, normalized = TRUE))
  eig_sd <- sd(eigen_centrality(g, directed = FALSE, scale = TRUE)$vector)
  comm <- cluster_louvain(as.undirected(g), weights = E(g)$weight)
  list(
    density = dens, avg_degree = avg_deg, transitivity = trans,
    avg_path_length = apl, degree_sd = deg_sd, degree_assortativity = assort,
    betweenness_sd = btw_sd, eigen_centrality_sd = eig_sd,
    modularity = modularity(comm), n_communities = length(comm),
    community_sizes = sizes(comm), membership = membership(comm)
  )
}

cat("\n==== Calculating structural metrics ====\n")
struct_metrics <- list()
for (nm in names(networks)) {
  cat("Calculating metrics for", nm, "...\n")
  struct_metrics[[nm]] <- tryCatch({
    get_structural_metrics(networks[[nm]]$graph)
  }, error = function(e) {
    warning(nm, " metric calculation failed: ", e$message)
    NULL
  })
}
struct_metrics <- struct_metrics[!sapply(struct_metrics, is.null)]

# ------------------------------
# Print structural metrics table
# ------------------------------
cat("\n==== Summary of structural metrics by dimension ====\n")
struct_table <- map_dfr(names(struct_metrics), function(nm) {
  s <- struct_metrics[[nm]]
  tibble(
    dimension = nm,
    network_density = round(s$density, 4),
    average_degree = round(s$avg_degree, 2),
    transitivity = round(s$transitivity, 4),
    average_path_length = round(s$avg_path_length, 2),
    degree_sd = round(s$degree_sd, 2),
    degree_assortativity = round(s$degree_assortativity, 4),
    betweenness_sd = round(s$betweenness_sd, 4),
    eigen_centrality_sd = round(s$eigen_centrality_sd, 4),
    modularity = round(s$modularity, 4),
    n_communities = s$n_communities
  )
})
print(struct_table, n=Inf)

# ------------------------------
# Community structure comparison (ARI index)
# ------------------------------
cat("\n==== Community structure consistency (ARI index, 0-1, higher=more similar) ====\n")
dims <- names(struct_metrics)
n <- length(dims)
ari_mat <- matrix(NA, n, n, dimnames = list(dims, dims))
for (i in seq_along(dims)) {
  for (j in seq_along(dims)) {
    mem_i <- struct_metrics[[dims[i]]]$membership
    mem_j <- struct_metrics[[dims[j]]]$membership
    common <- intersect(names(mem_i), names(mem_j))
    if (length(common) > 1) {
      ari_mat[i,j] <- round(compare(mem_i[common], mem_j[common], method = "adjusted.rand"), 3)
    }
  }
}
print(ari_mat)

# ------------------------------
# Edge structure comparison
# ------------------------------
edge_list_labels <- function(g) {
  el <- as_edgelist(g)
  if (nrow(el) == 0) return(character(0))
  apply(el, 1, function(x) paste(sort(x), collapse = "--"))
}

# Edge overlap rate
cat("\n==== Edge structure overlap rate (0-1, higher=more similar) ====\n")
edge_overlap_mat <- matrix(NA, n, n, dimnames = list(dims, dims))
for (i in seq_along(dims)) {
  for (j in seq_along(dims)) {
    e1 <- edge_list_labels(networks[[dims[i]]]$graph)
    e2 <- edge_list_labels(networks[[dims[j]]]$graph)
    if (length(e1) > 0 && length(e2) > 0) {
      edge_overlap_mat[i,j] <- round(mean(e1 %in% e2), 3)
    }
  }
}
print(edge_overlap_mat)

# Edge weight correlation
cat("\n==== Common edge weight correlation (-1 to 1, higher=more similar) ====\n")
weight_cor_mat <- matrix(NA, n, n, dimnames = list(dims, dims))
for (i in seq_along(dims)) {
  for (j in seq_along(dims)) {
    e1 <- edge_list_labels(networks[[dims[i]]]$graph)
    e2 <- edge_list_labels(networks[[dims[j]]]$graph)
    common <- intersect(e1, e2)
    if (length(common) >= 2) {
      get_weight <- function(sim_mat, e) {
        nodes <- strsplit(e, "--")[[1]]
        sim_mat[nodes[1], nodes[2]]
      }
      w1 <- sapply(common, function(e) get_weight(networks[[dims[i]]]$sim_matrix, e))
      w2 <- sapply(common, function(e) get_weight(networks[[dims[j]]]$sim_matrix, e))
      weight_cor_mat[i,j] <- round(cor(w1, w2, method = "spearman"), 3)
    }
  }
}
print(weight_cor_mat)

# ------------------------------
# Plotting (displayed in Plots panel)
# ------------------------------
cat("\n==== Generating visualizations (see Plots panel) ====\n")

# 1. Structural metrics comparison
metrics_long <- struct_table %>%
  pivot_longer(-dimension, names_to = "metric", values_to = "value")
print(ggplot(metrics_long, aes(x = dimension, y = value, fill = dimension)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~metric, scales = "free_y", ncol = 2) +
        labs(title = "Network structural metrics across nutritional dimensions", x = "Nutritional dimension", y = "Metric value") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# 2. Community size comparison
comm_size_df <- map_dfr(names(struct_metrics), function(nm) {
  s <- struct_metrics[[nm]]
  tibble(dimension = nm, community = seq_along(s$community_sizes), size = as.integer(s$community_sizes))
})
print(ggplot(comm_size_df, aes(x = factor(community), y = size, fill = dimension)) +
        geom_col(position = "dodge") +
        facet_wrap(~dimension, scales = "free_x") +
        labs(title = "Community size distribution by dimension", x = "Community ID", y = "Number of countries") +
        theme_minimal())

# 3. Network visualization (colored by community)
par(mfrow = c(2, 2), mar = c(1, 1, 3, 1))  # 2x2 layout, adjusted margins
for (nm in names(networks)) {
  g <- networks[[nm]]$graph
  mem <- struct_metrics[[nm]]$membership
  # Ensure matching community colors
  n_comm <- length(unique(mem))
  V(g)$color <- scales::hue_pal()(n_comm)[as.factor(mem)]
  # Plot network
  plot(g, 
       vertex.size = 4,        # Node size
       vertex.label = NA,      # Hide node labels
       main = paste(nm, "network"),  # Title
       layout = layout_with_fr,  # Layout algorithm
       edge.width = 0.5,       # Edge thickness
       margin = 0.1)           # Plot margin
}
par(mfrow = c(1, 1))  # Restore default layout
cat("\n==== Analysis complete ====\n")

