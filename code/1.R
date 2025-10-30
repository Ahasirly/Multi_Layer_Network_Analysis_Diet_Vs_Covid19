# === 安装与加载必要包 ===
library(tidyverse)
library(igraph)
library(scales)
library(readr)

# === Plot output setup ===
dir.create("Plots", showWarnings = FALSE)
plot_file <- file.path("Plots", "1_network_analysis.pdf")
pdf(plot_file, width = 10, height = 8, onefile = TRUE)
on.exit(dev.off(), add = TRUE)

# === 导入数据 ===
data <- read_csv("data/Food_Supply_kcal_Data.csv")

# === 数据预处理 ===
numeric_data <- data %>%
  select(-`Unit (all except Population)`, -Population, -Obesity, -Undernourished,
         -Confirmed, -Deaths, -Recovered, -Active) %>%
  column_to_rownames("Country")

mat <- as.matrix(numeric_data)
mat_norm <- scale(mat)

# =============================
# ① 基于皮尔逊相关系数的网络
# =============================
sim_cor <- cor(t(mat_norm))
threshold_cor <- 0.6
adj_cor <- sim_cor
adj_cor[adj_cor < threshold_cor] <- 0
g <- graph_from_adjacency_matrix(adj_cor, mode = "undirected", weighted = TRUE, diag = FALSE)

deg <- degree(g)
btw <- betweenness(g)
comm <- cluster_louvain(g)

cat("==== 基于皮尔逊相关的网络 ====\n")
cat("平均度数：", mean(deg), "\n")
cat("社区数量：", length(unique(membership(comm))), "\n")
cat("中介性最高的10个国家：\n")
print(sort(btw, decreasing = TRUE)[1:10])

# --- 可视化 ---
plot(
  g,
  vertex.size = rescale(deg, to = c(5, 15)),
  vertex.color = membership(comm),
  vertex.label = NA,
  edge.width = rescale(E(g)$weight, to = c(0.2, 2)),
  main = "Global Dietary Network (Pearson similarity > 0.6)"
)

# --- 输出每个社区的国家名称 ---
country_clusters <- data.frame(
  Country = names(membership(comm)),
  Community = membership(comm)
)

cat("\n各社区国家（皮尔逊相似度网络）：\n")
for (c in sort(unique(country_clusters$Community))) {
  cat(paste0("\n【社区 ", c, "】\n"))
  cat(paste(country_clusters$Country[country_clusters$Community == c], collapse = ", "))
  cat("\n")
}

# =============================
# ② 合并肥胖率和营养不良率信息
# =============================
obesity_data <- data %>%
  select(Country, Obesity, Undernourished) %>%
  mutate(
    Obesity = parse_number(as.character(Obesity)),
    Undernourished = parse_number(as.character(Undernourished))
  )

country_clusters_clean <- country_clusters %>%
  left_join(obesity_data, by = "Country")

# =============================
# ③ 前5个社区的健康对比分析
# =============================
top5_clusters <- sort(unique(country_clusters_clean$Community))[1:5]

community_health <- country_clusters_clean %>%
  filter(Community %in% top5_clusters) %>%
  group_by(Community) %>%
  summarise(
    mean_obesity = mean(Obesity, na.rm = TRUE),
    mean_undernourished = mean(Undernourished, na.rm = TRUE),
    n_countries = n()
  ) %>%
  arrange(Community)

cat("\n===== 前5个社区的健康指标对比 =====\n")
print(community_health)

# --- 可视化 ---
community_health_long <- community_health %>%
  pivot_longer(cols = c(mean_obesity, mean_undernourished),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         mean_obesity = "Mean Obesity (%)",
                         mean_undernourished = "Mean Undernourishment (%)"))

ggplot(community_health_long, aes(x = factor(Community), y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = ifelse(is.na(value), "NA", round(value, 2))),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  labs(
    title = "Top 5 Communities: Mean Obesity vs Undernourishment",
    x = "Community",
    y = "Average Percentage (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  ylim(0, max(community_health_long$value, na.rm = TRUE) * 1.2)

# =============================
# ④ 新冠指标分析（原数据单位为 % → 转换为每百人）
# =============================
covid_cols <- c("Country", "Confirmed", "Deaths", "Recovered", "Active", "Population")

covid_data <- data %>%
  select(all_of(covid_cols)) %>%
  mutate(across(c(Confirmed, Deaths, Recovered, Active), as.numeric))

# 检查单位
if ("% " %in% unique(trimws(data$`Unit (all except Population)`)) ||
    "%" %in% unique(trimws(data$`Unit (all except Population)`))) {
  cat("检测到单位为 % —— 转换为每百人指标。\n")
  covid_data <- covid_data %>%
    mutate(
      infection_rate = Confirmed,   # 百分比即每百人
      death_rate = Deaths,
      recovery_rate = Recovered,
      active_rate = Active
    )
} else {
  cat("未检测到百分比单位 —— 按病例数/人口计算。\n")
  covid_data <- covid_data %>%
    mutate(across(c(Population), as.numeric)) %>%
    mutate(
      infection_rate = Confirmed / Population * 100,
      death_rate = Deaths / Population * 100,
      recovery_rate = Recovered / Population * 100,
      active_rate = Active / Population * 100
    )
}

# 合并社区信息
covid_merged <- covid_data %>%
  left_join(country_clusters_clean %>% select(Country, Community), by = "Country")

top5_clusters <- sort(unique(covid_merged$Community))[1:5]

covid_by_comm <- covid_merged %>%
  filter(Community %in% top5_clusters) %>%
  group_by(Community) %>%
  summarise(
    avg_infection = mean(infection_rate, na.rm = TRUE),
    avg_death = mean(death_rate, na.rm = TRUE),
    avg_recovery = mean(recovery_rate, na.rm = TRUE),
    avg_active = mean(active_rate, na.rm = TRUE),
    n_countries = n()
  ) %>%
  arrange(Community)

cat("\n===== 前5个社区的新冠疫情指标对比（每百人） =====\n")
print(covid_by_comm)

# === 可视化 ===
covid_long <- covid_by_comm %>%
  pivot_longer(
    cols = c(avg_infection, avg_death, avg_recovery, avg_active),
    names_to = "metric", values_to = "value"
  ) %>%
  mutate(metric = recode(metric,
                         avg_infection = "Infection rate",
                         avg_death = "Death rate",
                         avg_recovery = "Recovery rate",
                         avg_active = "Active case rate"))

ggplot(covid_long, aes(x = factor(Community), y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = ifelse(is.na(value), "NA", round(value, 2))),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Top 5 Communities: Average Infection / Death / Recovery / Active Rates",
    x = "Community",
    y = "Cases per 100 people (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  ylim(0, max(covid_long$value, na.rm = TRUE) * 1.2)


# 饮食习惯共性

