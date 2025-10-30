# Project Proposal: Network Analysis of Global Dietary Patterns and Health Outcomes

## Problem Description
This project investigates how national dietary supply patterns influence population health outcomes in the context of the COVID-19 pandemic. Existing studies typically focus on isolated nutrients or food groups, leaving open how interconnected dietary systems across countries shape collective resilience to infectious diseases. By applying network science, we will construct global dietary networks that connect countries and food categories according to similarity in food supply. This framework allows us to uncover structural patterns that explain why certain dietary systems are associated with lower infection rates, faster recovery, and greater resilience during health crises such as COVID-19.

## Research Questions
1. **Cross-country dietary clusters**  
   How do global dietary pattern clusters, derived from national food supply data, differ in their health and COVID-19 outcomes, and what does the network structure of cross-country dietary similarity reveal about shared nutritional risk and resilience pathways?  
   - Nodes: countries  
   - Edges: dietary similarity above a cosine similarity threshold

2. **Food categories and COVID outcomes**  
   Which food categories show the strongest correlations with national infection and recovery rates, and what does this reveal about the potential role of dietary composition in shaping population-level health resilience?  
   - Nodes: food categories and COVID metrics  
   - Edges: correlation strength between supply and health indicators

3. **Multidimensional dietary networks**  
   How do dietary similarity networks and their community structures differ when built from alternative nutritional dimensions (fat supply, protein supply, kcal supply, and food quantity), and what do these differences reveal about the stability and multidimensionality of global dietary-health relationships?  
   - Networks: four layers (kcal, fat, protein, quantity) to compare structural differences

4. **Low-COVID dietary signatures**  
   What common dietary components characterize countries with low COVID-19 infection rates across multiple nutritional supply dimensions, and how do these shared food patterns reflect potential protective dietary structures?  
   - Nodes: low-infection countries and food categories  
   - Edges: normalized consumption intensity

## Data Source
COVID-19 Healthy Diet Dataset, Kaggle (https://www.kaggle.com/datasets/mariaren/covid19-healthy-diet-dataset), which integrates national food supply statistics with infection, recovery, and mortality indicators.

## Metrics and Analysis
- Degree centrality: identify key countries or foods with many connections  
- Clustering coefficient: evaluate the tightness of dietary communities  
- Modularity (Louvain): detect dietary clusters in each layer  
- Betweenness centrality: find bridge countries or foods between communities  
- Average degree and density: compare structural differences across nutritional dimensions  
- All computations and visualisations performed in R with `igraph`, `ggraph`, and `tidyverse`

## Analysis Plan
1. **Data preparation**: clean missing values and normalize food supply quantities.  
2. **Network construction**: build networks for each research question using cosine similarity or correlation-based edges.  
3. **Metric computation**: calculate network-level (density, modularity) and node-level (degree, betweenness) measures.  
4. **Community analysis**: identify and compare dietary communities across datasets.  
5. **Visualisation**: use `ggraph` to visualise country-food relationships and community structures.  
6. **Cross-dimensional comparison**: analyze convergence and divergence across kcal, fat, protein, and quantity-based networks.

## Timeline and Team Responsibilities
- **Team members**  
  - Lucas Xu: Data processing and network construction (RQ1, RQ3, RQ4)  
  - Luke Yin: Network analysis and metric computation (RQ2, RQ3, RQ4)  
  - Shuyu Yan: Visualisation, report writing, presentation support (all RQs)
- **Schedule**  
  - Oct 16: Proposal submission  
  - Oct 17 – Nov 7: Data cleaning, network construction, metric computation  
  - Nov 8: Mid-project report submission  
  - Nov 9 – Nov 28: In-depth analysis and visualisation across all RQs  
  - Dec 2: Video presentation submission  
  - Dec 3 – Dec 5: Final report writing and submission

