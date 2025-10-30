# =============================================
# 👇 一次性完整可运行脚本
# =============================================

library(tidyverse)
library(igraph)
library(ggraph)

# === Plot output setup ===
dir.create("Plots", showWarnings = FALSE)
plot_file <- file.path("Plots", "2_food_covid_networks.pdf")
pdf(plot_file, width = 10, height = 8, onefile = TRUE)
on.exit(dev.off(), add = TRUE)

# === 读入数据 ===
data <- read.csv("data/Food_Supply_kcal_Data.csv", check.names = FALSE)

# === 选出食物类列和目标列 ===
valid_food_cols <- c("Alcoholic Beverages","Animal Products","Animal fats","Aquatic Products, Other",
                     "Cereals - Excluding Beer","Eggs","Fish, Seafood","Fruits - Excluding Wine",
                     "Meat","Milk - Excluding Butter","Miscellaneous","Offals","Oilcrops","Pulses",
                     "Spices","Starchy Roots","Stimulants","Sugar Crops","Sugar & Sweeteners",
                     "Treenuts","Vegetal Products","Vegetable Oils","Vegetables")

infection_col <- "Confirmed"   # 感染率
recovery_col <- "Recovered"    # 治愈率

# === 计算相关系数 ===
cor_matrix_inf <- sapply(valid_food_cols, function(col) cor(data[[col]], data[[infection_col]], use = "complete.obs"))
cor_matrix_rec <- sapply(valid_food_cols, function(col) cor(data[[col]], data[[recovery_col]], use = "complete.obs"))

# === 构建边表（感染率、治愈率） ===
edges1 <- data.frame(
  from = valid_food_cols,
  to = "Infection_Rate",
  Correlation = as.numeric(cor_matrix_inf),
  weight = abs(as.numeric(cor_matrix_inf))
)

edges2 <- data.frame(
  from = valid_food_cols,
  to = "Recovery_Rate",
  Correlation = as.numeric(cor_matrix_rec),
  weight = abs(as.numeric(cor_matrix_rec))
)

# === 清洗函数 ===
clean_edges <- function(edges, valid_food_cols) {
  edges %>%
    mutate(
      from = as.character(from),
      to   = as.character(to),
      from = trimws(from),
      to   = trimws(to),
      from = gsub("[[:cntrl:]]", "", from),
      to   = gsub("[[:cntrl:]]", "", to),
      from = enc2utf8(from),
      to   = enc2utf8(to),
      Correlation = as.numeric(Correlation),
      weight = as.numeric(weight)
    )
}

edges1 <- clean_edges(edges1, valid_food_cols)
edges2 <- clean_edges(edges2, valid_food_cols)

# === 构建节点表 ===
build_nodes <- function(edges, valid_food_cols) {
  vertices <- unique(c(edges$from, edges$to))
  valid_food_cols_clean <- trimws(enc2utf8(as.character(valid_food_cols)))
  data.frame(
    name = vertices,
    type = ifelse(vertices %in% valid_food_cols_clean, "Food", "Rate"),
    stringsAsFactors = FALSE
  )
}

nodes1 <- build_nodes(edges1, valid_food_cols)
nodes2 <- build_nodes(edges2, valid_food_cols)

# === 构建图 ===
g1 <- graph_from_data_frame(d = edges1, vertices = nodes1, directed = FALSE)
g2 <- graph_from_data_frame(d = edges2, vertices = nodes2, directed = FALSE)

# === 画图函数 ===
plot_network <- function(graph, title) {
  ggraph(graph, layout = "fr") +
    geom_edge_link(aes(width = weight, color = Correlation), alpha = 0.8) +
    geom_node_point(aes(color = type), size = 6) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_color_manual(values = c("Food" = "#2E8B57", "Rate" = "#4682B4")) +
    scale_edge_color_gradient2(low = "#2166AC", mid = "grey80", high = "#B2182B", midpoint = 0) +
    theme_void() +
    labs(title = title, color = "Node Type", edge_color = "Correlation")
}

# === 绘制两张图 ===
set.seed(42)
p1 <- plot_network(g1, "Food vs Infection Rate (Network)")
p2 <- plot_network(g2, "Food vs Recovery Rate (Network)")

# === 显示 ===
print(p1)
print(p2)
