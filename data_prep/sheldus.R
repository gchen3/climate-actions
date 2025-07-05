# Load libraries
library(tidyverse)
library(here)
library(gt)
library(scales)

# Load and clean SHELDUS data
sheldus <- read_csv(here("data", "direct_loss_aggregated_output_5008.csv"))
names(sheldus) <- tolower(names(sheldus))

# Select and rename variables of interest
sheldus_clean <- sheldus %>%
  rename(
    state = statename,
    cropdmg_adj = `cropdmg(adj 2023)`,
    propertydmg_adj = `propertydmg(adj 2023)`
  ) %>%
  select(state, year, hazard, cropdmg = cropdmg_adj, propertydmg = propertydmg_adj)

# Summarize by year
summary_by_year <- sheldus_clean %>%
  group_by(year) %>%
  summarize(
    total_property_dmg = sum(propertydmg, na.rm = TRUE),
    total_crop_dmg = sum(cropdmg, na.rm = TRUE),
    .groups = "drop"
  )

# Summarize by state
summary_by_state <- sheldus_clean %>%
  group_by(state) %>%
  summarize(
    total_property_dmg = sum(propertydmg, na.rm = TRUE),
    total_crop_dmg = sum(cropdmg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_property_dmg))

# Summarize by hazard
summary_by_hazard <- sheldus_clean %>%
  group_by(hazard) %>%
  summarize(
    total_property_dmg = sum(propertydmg, na.rm = TRUE),
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

