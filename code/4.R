# =============================================
# Low-COVID Countries Food Network + Bar Chart
# Clean, stable, and fully functional version
# =============================================

library(tidyverse)
library(igraph)
library(ggraph)
library(scales)

# === Create plot output directory ===
dir.create("Plots", showWarnings = FALSE)

# === 1. Load supply tables ===
files <- c(
  "Food_Supply_kcal_Data.csv",
  "Fat_Supply_Quantity_Data.csv",
  "Food_Supply_Quantity_kg_Data.csv",
  "Protein_Supply_Quantity_Data.csv"
)

data_list <- lapply(files, function(f) {
  read.csv(file.path("data", f), check.names = FALSE)
})
names(data_list) <- c("kcal", "fat", "kg", "protein")

# === 2. Food categories ===
food_cols <- c(
  "Alcoholic Beverages","Animal Products","Animal fats","Aquatic Products, Other",
  "Cereals - Excluding Beer","Eggs","Fish, Seafood","Fruits - Excluding Wine",
  "Meat","Milk - Excluding Butter","Miscellaneous","Offals","Oilcrops","Pulses",
  "Spices","Starchy Roots","Stimulants","Sugar Crops","Sugar & Sweeteners",
  "Treenuts","Vegetal Products","Vegetable Oils","Vegetables"
)

# === 3. Identify low-COVID countries ===
low_covid_countries <- data_list$kcal %>%
  mutate(
    Confirmed = as.numeric(as.character(Confirmed)),
    Population = as.numeric(as.character(Population)),
    Confirmed_rate = Confirmed / Population
  ) %>%
  filter(!is.na(Confirmed_rate)) %>%
  arrange(Confirmed_rate) %>%
  slice_head(n = 20) %>%
  pull(Country)

# === 4. Combine food data across 4 tables ===
combined_food <- lapply(data_list, function(df) {
  df %>%
    filter(Country %in% low_covid_countries) %>%
    select(Country, all_of(food_cols)) %>%
    mutate(across(all_of(food_cols), ~ as.numeric(as.character(.x))))
})

combined_mat <- Reduce(function(x, y) {
  x[-1] <- x[-1] + y[-1]
  x
}, combined_food)

combined_mat[-1] <- combined_mat[-1] / length(combined_food)

# === 5. Build edges ===
edges_list <- combined_mat %>%
  pivot_longer(cols = -Country, names_to = "Food", values_to = "Amount") %>%
  filter(!is.na(Amount) & Amount > 0) %>%
  mutate(Amount_scaled = rescale(Amount, to = c(0.1, 1)))

# === 6. Construct network ===
nodes <- tibble(
  name = c(unique(edges_list$Country), unique(edges_list$Food)),
  type = c(
    rep("Country", length(unique(edges_list$Country))),
    rep("Food", length(unique(edges_list$Food)))
  )
)

edges <- edges_list %>%
  rename(from = Country, to = Food, weight = Amount_scaled)

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

# Node aesthetics
V(g)$color <- ifelse(V(g)$type == "Country", "#4DBBD5", "#E64B35")
V(g)$size  <- ifelse(V(g)$type == "Country", 6, 4)

# === 7. Draw network plot ===
set.seed(42)
p_network <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.4, color = "grey50") +
  geom_node_point(aes(color = color, size = size)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_size_identity() +
  scale_color_identity() +
  theme_void() +
  labs(
    title = "Low-COVID Countries and Food Intake Network",
    subtitle = "Edges = Normalized Food Intake Amount"
  )

# Display network in RStudio
print(p_network)

# Save to PDF
ggsave("Plots/4_low_covid_food_network.pdf", p_network, width = 10, height = 8)



# =============================================
# BAR CHART OF COMMON FOODS
# =============================================

combined_mat_numeric <- combined_mat %>%
  mutate(across(-Country, ~ as.numeric(.x)))

# Mean intake
food_means <- combined_mat_numeric %>%
  select(-Country) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Food", values_to = "Avg_Intake")

# Standard deviation
food_sd <- combined_mat_numeric %>%
  select(-Country) %>%
  summarise(across(everything(), sd, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Food", values_to = "SD_Intake")

# Combine
food_summary <- food_means %>%
  left_join(food_sd, by = "Food") %>%
  mutate(
    Common = ifelse(
      Avg_Intake > mean(Avg_Intake) &
        SD_Intake < median(SD_Intake),
      "Yes", "No"
    )
  )

# Bar chart
p_bar <- ggplot(food_summary, aes(x = reorder(Food, Avg_Intake), y = Avg_Intake, fill = Common)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Yes" = "#4DBBD5", "No" = "#E64B35")) +
  labs(
    title = "Low-COVID Countries: Common High-Intake Foods",
    x = "Food Category",
    y = "Average Intake",
    fill = "Common"
  ) +
  theme_minimal(base_size = 14)

# Display bar chart
print(p_bar)

# Save bar chart PDF
ggsave("Plots/4_low_covid_food_barchart.pdf", p_bar, width = 10, height = 8)