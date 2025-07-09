# Descriptive statistics and graphs

# Load necessary libraries
library(tidyverse)
library(here)
library(ggplot2)
library(gt)

# load merged data
merged_data <- readRDS(here("data", "merged_data.rds"))

# descriptive statistics
summary_stats <- merged_data %>%
  select(population, total_revenue, total_expenditure,
         total_debt_outstanding, surplus_ratio, debt_total_ratio,
         total_revenue_per_capita, total_expenditure_per_capita,
         total_debt_per_capita, own_rev_ratio, RDF_pct_of_GF_spending,
         total_damage_adj, total_damage_percapita, climate_related_damage,
         climate_related_damage_percapita, senate_total, senate_dem,
         senate_rep, house_total, house_dem, house_rep, senate_dem_pct,
         house_dem_pct) %>%
  mutate(total_revenue_m = total_revenue / 1000,
         total_expenditure_m = total_expenditure / 1000,
         total_debt_outstanding_m = total_debt_outstanding / 1000,
         total_damage_adj_k = total_damage_adj / 1000,
         climate_related_damage_k = climate_related_damage / 1000,
         population_k = population / 1000
         ) %>%
  select(-total_revenue, -total_expenditure, -total_debt_outstanding,
         -total_damage_adj, -climate_related_damage, -population) %>%
  summarise(
    across(
      everything(),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        n = ~ sum(!is.na(.x))
      ),
      .names = "{.col}__{.fn}"
    )
  )

ordered_vars <- c(
  # Disaster damage
  "total_damage_adj_k", "climate_related_damage_k",
  "total_damage_percapita", "climate_related_damage_percapita",
  
  # Fiscal
  "population_k", "total_revenue_m", "total_expenditure_m",
  "total_debt_outstanding_m", "total_revenue_per_capita",
  "total_expenditure_per_capita", "total_debt_per_capita",
  "own_rev_ratio", "debt_total_ratio", "surplus_ratio",
  "RDF_pct_of_GF_spending",
  
  # Political
  "senate_total", "senate_dem", "senate_rep", "senate_dem_pct",
  "house_total", "house_dem", "house_rep", "house_dem_pct"
)

summary_stats_long <- summary_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_sep = "__",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(variable = factor(variable, levels = ordered_vars)) %>%
  arrange(variable)


variable_meta <- tibble::tibble(
  variable = c(
    # Disaster damage
    "total_damage_adj_k", "climate_related_damage_k",
    "total_damage_percapita", "climate_related_damage_percapita",
    
    # Fiscal
    "population_k", "total_revenue_m", "total_expenditure_m",
    "total_debt_outstanding_m", "total_revenue_per_capita",
    "total_expenditure_per_capita", "total_debt_per_capita",
    "own_rev_ratio", "debt_total_ratio", "surplus_ratio",
    "RDF_pct_of_GF_spending",
    
    # Political
    "senate_total", "senate_dem", "senate_rep", "senate_dem_pct",
    "house_total", "house_dem", "house_rep", "house_dem_pct"
  ),
  label = c(
    "Total Damage Adjusted (thousands)", "Climate-Related Damage (thousands)",
    "Total Damage Per Capita", "Climate-Related Damage Per Capita",
    "Population (thousands)", "Total Revenue (millions)", "Total Expenditure (millions)",
    "Total Debt Outstanding (millions)", "Total Revenue Per Capita",
    "Total Expenditure Per Capita", "Total Debt Per Capita", "Own-Source Revenue Ratio",
    "Debt to Total Revenue Ratio", "Surplus Ratio", "RDF as % of GF Spending",
    "Senate Total Members", "Senate Democrats (Count)", "Senate Republicans (Count)",
    "Senate Democrats (%)", "House Total Members", "House Democrats (Count)",
    "House Republicans (Count)", "House Democrats (%)"
  ),
  category = c(
    rep("Disaster Damage Variables", 4),
    rep("Fiscal Variables", 11),
    rep("Political Variables", 8)
  )
)

sum_stats_table <- summary_stats_long %>%
  left_join(variable_meta, by = "variable") %>%
  mutate(category = factor(category, levels = c("Disaster Damage Variables", "Fiscal Variables", "Political Variables"))) %>%
  arrange(category, variable) %>%
  select(category, label, mean, median, sd, min, max, n) %>%
  gt(groupname_col = "category") %>%
  tab_header(title = "Descriptive Statistics of Merged Data") %>%
  fmt_number(columns = c(mean, median, sd, min, max), decimals = 2) %>%
  fmt_number(columns = n, decimals = 0) %>%
  cols_label(
    label = "Variable",
    mean = "Mean",
    median = "Median",
    sd = "Std. Dev.",
    min = "Min",
    max = "Max",
    n = "N"
  ) %>%
  tab_options(
    table.font.size = "small",
    data_row.padding = px(3),
    table.width = pct(100)
  )

gtsave(
  data = sum_stats_table,
  filename = here::here("results", "descriptive_stats_table.png")
)


