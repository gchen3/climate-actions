# Load libraries
library(tidyverse)
library(here)
library(gt)
library(scales)
library(janitor)

# Load and clean SHELDUS data
sheldus <- read_csv(here("data", "sheldus", "direct_loss_aggregated_output_5008.csv"))

# Select and rename variables of interest
sheldus_clean <- sheldus %>%
  rename_with(tolower) %>%
  clean_names() %>%
  rename(
    state = statename,
    cropdmg_adj = cropdmg_adj_2023,
    propertydmg_adj = propertydmg_adj_2023
  ) %>%
  select(state, year, hazard, cropdmg, cropdmg_adj, propertydmg, propertydmg_adj,
         duration_days, cropdmgpercapita, propertydmgpercapita)

saveRDS(sheldus_clean, here::here("data", "sheldus_long.rds"))
write.csv(sheldus_clean, here::here("data", "sheldus_long.csv"))

# Convert long disaster damage to wide by type of hazard
sheldus_wide <- sheldus_clean %>%
  select(-duration_days) %>%
  pivot_wider(
    names_from = hazard,
    values_from = c(cropdmg, cropdmg_adj, cropdmgpercapita, propertydmg, propertydmg_adj, propertydmgpercapita),
    values_fill = 0
  ) %>%
  clean_names() %>%
  mutate(
    total_damage_adj = rowSums(select(., starts_with("propertydmg_adj_")), na.rm = TRUE) +
      rowSums(select(., starts_with("cropdmg_adj_")), na.rm = TRUE),
    total_damage_percapita = rowSums(select(., starts_with("propertydmgpercapita_")), na.rm = TRUE) +
      rowSums(select(., starts_with("cropdmgpercapita_")), na.rm = TRUE),
    total_damage = rowSums(select(., starts_with("propertydmg_")), na.rm = TRUE) +
      rowSums(select(., starts_with("cropdmg_")), na.rm = TRUE),
    climate_related_damage = rowSums(select(., # Drought",Flood","Heat","Hurricane/Tropical Storm","Wildfire","Coastal"
      starts_with("cropdmg_adj_drought"), 
      starts_with("cropdmg_adj_flood"), 
      starts_with("cropdmg_adj_heat"), 
      starts_with("cropdmg_adj_hurricane_tropical_storm"), 
      starts_with("cropdmg_adj_wildfire"), 
      starts_with("cropdmg_adj_coastal")), na.rm = TRUE
    ),
    climate_related_damage_percapita = rowSums(select(., 
      starts_with("cropdmgpercapita_drought"), 
      starts_with("cropdmgpercapita_flood"), 
      starts_with("cropdmgpercapita_heat"), 
      starts_with("cropdmgpercapita_hurricane_tropical_storm"), 
      starts_with("cropdmgpercapita_wildfire"), 
      starts_with("cropdmgpercapita_coastal")), na.rm = TRUE
    )
  )

saveRDS(sheldus_wide, here::here("data", "sheldus.rds"))
write.csv(sheldus_wide, here::here("data", "sheldus.csv"))
  
# Summarize by year
summary_by_year <- sheldus_clean %>%
  group_by(year) %>%
  summarize(
    total_property_dmg = sum(propertydmg_adj, na.rm = TRUE),
    total_crop_dmg = sum(cropdmg_adj, na.rm = TRUE),
    .groups = "drop"
  )

# Summarize by state
summary_by_state <- sheldus_clean %>%
  group_by(state) %>%
  summarize(
    total_property_dmg = sum(propertydmg_adj, na.rm = TRUE),
    total_crop_dmg = sum(cropdmg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_property_dmg))

# Summarize by hazard
summary_by_hazard <- sheldus_clean %>%
  group_by(hazard) %>%
  summarize(
    total_property_dmg = sum(propertydmg_adj, na.rm = TRUE),
    total_crop_dmg = sum(cropdmg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_property_dmg))

# Create summary tables
summary_by_year |>
  mutate(
    total_property_dmg = total_property_dmg / 1e6,  # Convert to millions
    total_crop_dmg = total_crop_dmg / 1e6           # Convert to millions
  ) |>
  gt() |>
  tab_header(title = "Annual Direct Damage Summary (Adjusted to 2023 Dollars)") |>
  fmt_currency(columns = starts_with("total"), currency = "USD") |>
  cols_label(
    total_property_dmg = "Total Property Damage (million)",
    total_crop_dmg = "Total Crop Damage (million)"
  )

summary_by_state |>
  mutate(
    total_property_dmg = total_property_dmg / 1e6,  # Convert to millions
    total_crop_dmg = total_crop_dmg / 1e6           # Convert to millions
  ) |>
  gt() |>
  tab_header(title = "Total Damage by State (Adjusted to 2023 Dollars)") |>
  fmt_currency(columns = starts_with("total"), currency = "USD") |>
  cols_label(
    total_property_dmg = "Total Property Damage (million)",
    total_crop_dmg = "Total Crop Damage (million)"
  ) |>
  arrange(desc(total_property_dmg))

summary_by_hazard |>
  mutate(
    total_property_dmg = total_property_dmg / 1e6,  # Convert to millions
    total_crop_dmg = total_crop_dmg / 1e6           # Convert to millions
  ) |>
  gt() |>
  tab_header(title = "Total Damage by Hazard Type (Adjusted to 2023 Dollars)") |>
  fmt_currency(columns = starts_with("total"), currency = "USD") |>
  cols_label(
    total_property_dmg = "Total Property Damage (million)",
    total_crop_dmg = "Total Crop Damage (million)"
  ) 

# Year-hazard damage plot
plot_hazard_trends <- sheldus_clean %>%
  mutate(total_dmg = propertydmg + cropdmg) %>%
  group_by(year, hazard) %>%
  summarize(total_dmg = sum(total_dmg, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = total_dmg, color = hazard)) +
  geom_line() +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Total Damage by Hazard Type Over Time",
    x = "Year",
    y = "Total Damage (2023 USD)",
    color = "Hazard Type"
  ) +
  theme_minimal()

plot_hazard_trends

# Identify top 6 hazard types by total damage
climate_related_hazards <- c(
  "Coastal",
  "Drought",
  "Flood",
  "Hail",
  "Heat",
  "Hurricane/Tropical Storm",
  "Severe Thunderstorm",
  "Tornado",
  "Wildfire",
  "Wind",
  "Winter Weather"
)

most_climate_related_hazards <- c(
  "Drought",
  "Flood",
  "Heat",
  "Hurricane/Tropical Storm",
  "Wildfire",
  "Coastal"
)

# Filter to top hazards and plot with facets
plot_top_hazards <- sheldus_clean %>%
  filter(hazard %in% most_climate_related_hazards) %>%
  mutate(total_dmg = propertydmg + cropdmg) %>%
  mutate(total_dmg = total_dmg / 1e6) %>%
  group_by(year, hazard) %>%
  summarize(total_dmg = sum(total_dmg, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = total_dmg)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ hazard, scales = "free_y") +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Most climate related hazards: Annual Damage Trends",
    subtitle = "Total direct damage (property + crop) adjusted to 2023 dollars",
    x = "Year",
    y = "Total Damage (2023 USD, in million)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

plot_top_hazards

# NOTE --------------------------------------------------------------------
# Data Source:
# SPATIAL HAZARD EVENTS AND LOSSES DATABASE FOR THE UNITED STATES (SHELDUS) Version 23.0
# Downloaded: July 3, 2025
#
# Key Variables:
# - cropdmg: Direct damage to crops in nominal U.S. dollars.
# - cropdmg_adj: Direct damage to crops in adjusted 2023 U.S. dollars.
# - CropDmgPerCapita: Crop damage per capita, adjusted to the most recent SHELDUS inflation base year.
#
# - propertydmg: Direct damage to property in nominal U.S. dollars (no decimals).
# - propertydmg_adj: Direct damage to property in adjusted 2023 U.S. dollars (two decimal places).
# - PropertyDmgPerCapita: Property damage per capita, adjusted to the most recent SHELDUS inflation base year.
#
# - duration_days: Duration of the event in days (integer).
#
# Hazard Categories (18 total):
#  1.  Avalanche
#  2.  Coastal
#  3.  Drought
#  4.  Earthquake
#  5.  Flood
#  6.  Fog
#  7.  Hail
#  8.  Heat
#  9.  Hurricane/Tropical Storm
# 10.  Landslide
# 11.  Lightning
# 12.  Severe Thunderstorm
# 13.  Tornado
# 14.  Tsunami/Seiche
# 15.  Volcano
# 16.  Wildfire
# 17.  Wind
# 18.  Winter Weather

