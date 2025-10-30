# === Load required packages ===
library(tidyverse)
library(igraph)
library(scales)
library(readr)

# === Plot output setup ===
dir.create("Plots", showWarnings = FALSE)
plot_file <- file.path("Plots", "1_network_analysis.pdf")
pdf(plot_file, width = 10, height = 8, onefile = TRUE)
on.exit(dev.off(), add = TRUE)

# === Load data ===
data <- read_csv("data/Food_Supply_kcal_Data.csv")

# === Prepare numeric matrix for similarity analysis ===
numeric_data <- data %>%
  select(-`Unit (all except Population)`, -Population, -Obesity, -Undernourished,
         -Confirmed, -Deaths, -Recovered, -Active) %>%
  column_to_rownames("Country")

mat <- as.matrix(numeric_data)
mat_norm <- scale(mat)

# =============================
# 1. Network based on Pearson correlations
# =============================
sim_cor <- cor(t(mat_norm))
threshold_cor <- 0.6
adj_cor <- sim_cor
adj_cor[adj_cor < threshold_cor] <- 0
g <- graph_from_adjacency_matrix(adj_cor, mode = "undirected", weighted = TRUE, diag = FALSE)

deg <- degree(g)
btw <- betweenness(g)
comm <- cluster_louvain(g)

cat("==== Network based on Pearson correlations ====\n")
cat("Average degree: ", mean(deg), "\n")
cat("Number of communities: ", length(unique(membership(comm))), "\n")
cat("Top 10 countries by betweenness:\n")
print(sort(btw, decreasing = TRUE)[1:10])

# --- Visualise network ---
plot(
  g,
  vertex.size = rescale(deg, to = c(5, 15)),
  vertex.color = membership(comm),
  vertex.label = NA,
  edge.width = rescale(E(g)$weight, to = c(0.2, 2)),
  main = "Global Dietary Network (Pearson similarity > 0.6)"
)

# --- Report countries in each community ---
country_clusters <- data.frame(
  Country = names(membership(comm)),
  Community = membership(comm)
)

cat("\nCountries by community (Pearson similarity network):\n")
for (c in sort(unique(country_clusters$Community))) {
  cat(paste0("\n[Community ", c, "]\n"))
  cat(paste(country_clusters$Country[country_clusters$Community == c], collapse = ", "))
  cat("\n")
}

# =============================
# 2. Attach obesity and undernourishment data
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
# 3. Compare health indicators in the first five communities
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

cat("\n===== Health indicators for the first five communities =====\n")
print(community_health)

# --- Visualise health comparison ---
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
# 4. COVID-19 indicator analysis (convert percentages to per-100 rates)
# =============================
covid_cols <- c("Country", "Confirmed", "Deaths", "Recovered", "Active", "Population")

covid_data <- data %>%
  select(all_of(covid_cols)) %>%
  mutate(across(c(Confirmed, Deaths, Recovered, Active), as.numeric))

# Determine whether percentage units are present
if ("% " %in% unique(trimws(data$`Unit (all except Population)`)) ||
    "%" %in% unique(trimws(data$`Unit (all except Population)`))) {
  cat("Detected percentage units: treating values as per-100 population metrics.\n")
  covid_data <- covid_data %>%
    mutate(
      infection_rate = Confirmed,   # percentages already represent per 100 people
      death_rate = Deaths,
      recovery_rate = Recovered,
      active_rate = Active
    )
} else {
  cat("No percentage units detected: computing rates per 100 people using population.\n")
  covid_data <- covid_data %>%
    mutate(across(c(Population), as.numeric)) %>%
    mutate(
      infection_rate = Confirmed / Population * 100,
      death_rate = Deaths / Population * 100,
      recovery_rate = Recovered / Population * 100,
      active_rate = Active / Population * 100
    )
}

# Merge community information with COVID metrics
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

cat("\n===== COVID-19 indicators for the first five communities (per 100 people) =====\n")
print(covid_by_comm)

# === Visualise COVID comparison ===
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


# Dietary pattern summary placeholder

