



library(tidyverse)
library(igraph)
library(ggraph)
library(scales)

# === 1. 读取四个表 ===
file_path <- "C:/Users/grizz/Downloads/archive (3)/"
files <- c("Food_Supply_kcal_Data.csv",
           "Fat_Supply_Quantity_Data.csv",
           "Food_Supply_Quantity_kg_Data.csv",
           "Protein_Supply_Quantity_Data.csv")

data_list <- lapply(files, function(f) {
  read.csv(paste0(file_path, f), check.names = FALSE)
})
names(data_list) <- c("kcal", "fat", "kg", "protein")

# === 2. 定义食物列 ===
food_cols <- c("Alcoholic Beverages","Animal Products","Animal fats","Aquatic Products, Other",
               "Cereals - Excluding Beer","Eggs","Fish, Seafood","Fruits - Excluding Wine",
               "Meat","Milk - Excluding Butter","Miscellaneous","Offals","Oilcrops","Pulses",
               "Spices","Starchy Roots","Stimulants","Sugar Crops","Sugar & Sweeteners",
               "Treenuts","Vegetal Products","Vegetable Oils","Vegetables")

# === 3. 取低 COVID 国家（示例用 kcal 表） ===
low_covid_countries <- data_list$kcal %>%
  mutate(Confirmed = as.numeric(as.character(Confirmed)),
         Population = as.numeric(as.character(Population)),
         Confirmed_rate = Confirmed / Population) %>%
  filter(!is.na(Confirmed_rate)) %>%
  arrange(Confirmed_rate) %>%
  slice_head(n = 20) %>%
  pull(Country)

# === 4. 合并四张表的食物数据 ===
combined_food <- lapply(data_list, function(df) {
  df %>%
    filter(Country %in% low_covid_countries) %>%
    select(Country, all_of(food_cols)) %>%
    mutate(across(all_of(food_cols), ~ as.numeric(as.character(.x))))
})

# 用平均值合并四表
combined_mat <- Reduce(function(x, y) {
  x[-1] <- x[-1] + y[-1]
  x
}, combined_food)
combined_mat[-1] <- combined_mat[-1] / length(combined_food)

# === 5. 构建国家-食物边 ===
edges_list <- combined_mat %>%
  pivot_longer(cols = -Country, names_to = "Food", values_to = "Amount") %>%
  filter(!is.na(Amount) & Amount > 0) %>%
  mutate(Amount_scaled = scales::rescale(Amount, to = c(0.1, 1)))  # 标准化边权重

# === 6. 构建 igraph 网络 ===
nodes <- tibble(name = c(unique(edges_list$Country), unique(edges_list$Food)),
                type = c(rep("Country", length(unique(edges_list$Country))),
                         rep("Food", length(unique(edges_list$Food)))))

edges <- edges_list %>%
  rename(from = Country, to = Food, weight = Amount_scaled)

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

# === 7. 节点颜色和形状 ===
V(g)$color <- ifelse(V(g)$type == "Country", "#4DBBD5", "#E64B35")
V(g)$size <- ifelse(V(g)$type == "Country", 6, 4)

# === 8. 绘制网络 ===
set.seed(42)
ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.4, color = "grey50") +
  geom_node_point(aes(color = color, size = size)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_size_identity() +
  scale_color_identity() +
  theme_void() +
  labs(title = "Low-COVID Countries and Food Intake Network",
       subtitle = "Edges = Normalized Food Intake Amount",
       edge_width = "Scaled Amount")





library(tidyverse)
library(scales)

# === 1. 使用之前的 combined_mat ===
# combined_mat 已经是低 COVID 国家平均四表数据

combined_mat_numeric <- combined_mat %>%
  mutate(across(-Country, ~ as.numeric(.x)))

# 然后计算平均值
food_means <- combined_mat_numeric %>%
  select(-Country) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Food", values_to = "Avg_Intake") %>%
  arrange(desc(Avg_Intake))

food_means

# === 3. 计算标准差，找出波动小的食物 ===
food_sd <- combined_mat %>%
  select(-Country) %>%
  summarise(across(everything(), sd, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Food", values_to = "SD_Intake")

# === 4. 合并均值和标准差 ===
food_summary <- food_means %>%
  left_join(food_sd, by = "Food") %>%
  mutate(Common = ifelse(Avg_Intake > mean(Avg_Intake) & SD_Intake < median(SD_Intake), "Yes", "No"))

# 查看共性食物
food_summary %>% filter(Common == "Yes") %>% arrange(desc(Avg_Intake))

# === 5. 绘图：平均摄入量条形图 ===
ggplot(food_summary, aes(x = reorder(Food, Avg_Intake), y = Avg_Intake, fill = Common)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Yes" = "#4DBBD5", "No" = "#E64B35")) +
  labs(title = "Low-COVID Countries: Common High-Intake Foods",
       x = "Food Category", y = "Average Intake", fill = "Common") +
  theme_minimal(base_size = 14)

