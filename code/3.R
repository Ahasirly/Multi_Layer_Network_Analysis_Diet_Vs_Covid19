# =======================
# Food Pattern Clustering and Network Visualization
# =======================

library(tidyverse)
library(igraph)
library(ggraph)
library(scales)

# === Plot output setup ===
dir.create("Plots", showWarnings = FALSE)
plot_file <- file.path("Plots", "3_clustering_visualizations.pdf")
pdf(plot_file, width = 10, height = 8, onefile = TRUE)
on.exit(dev.off(), add = TRUE)

# === 1. Load data ===
data <- read.csv("data/Food_Supply_kcal_Data.csv", check.names = FALSE)

# === 2. Select food-related columns (excluding health and COVID variables) ===
food_cols <- c("Alcoholic Beverages","Animal Products","Animal fats","Aquatic Products, Other",
               "Cereals - Excluding Beer","Eggs","Fish, Seafood","Fruits - Excluding Wine",
               "Meat","Milk - Excluding Butter","Miscellaneous","Offals","Oilcrops","Pulses",
               "Spices","Starchy Roots","Stimulants","Sugar Crops","Sugar & Sweeteners",
               "Treenuts","Vegetal Products","Vegetable Oils","Vegetables")

food_data <- data %>%
  select(Country, all_of(food_cols)) %>%
  drop_na()

# === 3. Standardise data (avoid dominance by large-magnitude variables) ===
food_scaled <- as.data.frame(scale(food_data[, -1]))
rownames(food_scaled) <- food_data$Country

# === 4. K-means clustering ===
set.seed(123)
k <- 3  # assume three clusters: meat-heavy, plant-forward, mixed
kmeans_result <- kmeans(food_scaled, centers = k, nstart = 25)
food_data$Cluster <- factor(kmeans_result$cluster)

# === 5. Compute country similarity matrix (scaled Euclidean distance) ===
sim_matrix <- as.matrix(1 - dist(food_scaled, method = "euclidean") / max(dist(food_scaled)))

# Keep only highly similar pairs (> 0.8) to simplify the graph
edges <- which(sim_matrix > 0.8, arr.ind = TRUE)
edges_df <- data.frame(
  from = rownames(sim_matrix)[edges[, 1]],
  to = rownames(sim_matrix)[edges[, 2]],
  weight = sim_matrix[edges],
  stringsAsFactors = FALSE
) %>% filter(from != to)

# === 6. Build igraph object ===
nodes_df <- data.frame(
  name = food_data$Country,
  Cluster = food_data$Cluster
)

g <- graph_from_data_frame(d = edges_df, vertices = nodes_df, directed = FALSE)

# === 7. Plot country similarity network ===
set.seed(42)
p <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.2, color = "grey60") +
  geom_node_point(aes(color = Cluster), size = 6) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = c("#E64B35", "#4DBBD5", "#00A087")) +
  theme_void() +
  labs(title = "Food Pattern Clustering: Global Dietary Similarity Network",
       subtitle = "Nodes = Countries | Colors = Dietary Pattern Clusters",
       color = "Cluster (Diet Type)", edge_width = "Similarity")

print(p)

# === 8. Compare pandemic indicators across clusters ===
compare_vars <- c("Obesity","Undernourished","Confirmed","Deaths","Recovered")
compare_df <- data %>%
  select(Country, all_of(compare_vars)) %>%
  mutate(across(all_of(compare_vars), ~ as.numeric(gsub("%","",as.character(.x))))) %>%
  inner_join(food_data %>% select(Country, Cluster), by = "Country") %>%
  group_by(Cluster) %>%
  summarise(across(all_of(compare_vars), ~ mean(.x, na.rm = TRUE)))


print(compare_df)



library(ggplot2)
library(tidyr)

# Prepare long format for plotting
compare_long <- compare_df %>%
  pivot_longer(cols = -Cluster, names_to = "Variable", values_to = "Value")

# Make cluster labels more descriptive
compare_long <- compare_long %>%
  mutate(Cluster = recode_factor(Cluster,
                                 `2` = "High Meat/Fat",
                                 `3` = "Plant-based",
                                 `1` = "Mixed Diet"))

# Plot comparison
ggplot(compare_long, aes(x = Variable, y = Value, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Clusters by Health and COVID-19 Indicators",
    x = "Variable",
    y = "Average Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


library(dplyr)

cluster_food_means <- food_data %>%
  group_by(Cluster) %>%
  summarise(across(all_of(food_cols), ~ mean(as.numeric(.x), na.rm = TRUE)))

print(cluster_food_means)

