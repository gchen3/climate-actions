library(tidyverse)
library(readr)
library(ggplot2)
library(here)
library(gt)
library(skimr)
library(devtools)
library(janitor)
library(readxl)

# Data source: https://www.ncsl.org/about-state-legislatures/state-partisan-composition
# Data source: https://github.com/psthomas/state-partisan-composition/tree/master

link_1 <- "https://raw.githubusercontent.com/psthomas/state-partisan-composition/refs/heads/master/data/outputs/state_partisan_composition_1934_2021.csv"
link_2 <- "https://raw.githubusercontent.com/psthomas/state-partisan-composition/refs/heads/master/data/outputs/state_partisan_composition_2009_2021.csv"

#download.file(link_1, here("data", "state_partisan_composition_1934_2021.csv"))
#download.file(link_2, here("data", "state_partisan_composition_2009_2021.csv"))

# Load and clean state partisan composition data
state_poli_1 <- read_csv(here("data", "state_partisan_composition_1934_2021.csv"))
state_poli_2 <- read_csv(here("data", "state_partisan_composition_2009_2021.csv"))

state_poli_compare <- state_poli_1 |>
  filter(year >= 2009) |>
  left_join(state_poli_2, by = c("state", "year")) |>
  mutate(diff = senate_total.x - senate_total.y,
         diff_house = house_total.x - house_total.y,
         diff_dem = senate_dem.x - senate_dem.y,
         diff_rep = senate_rep.x - senate_rep.y,
         diff_house_dem = house_dem.x - house_dem.y,
         diff_house_rep = house_rep.x - house_rep.y) |>
  select(state, year, diff, diff_house, diff_dem, diff_rep, diff_house_dem, diff_house_rep) |>
  glimpse()

state_poli_formerge <- state_poli_1 |>
  filter(year >= 2000) |>
  mutate(senate_dem_pct = senate_dem / senate_total,
         house_dem_pct = house_dem / house_total)

## update with 2022, 2023, 2024 date from PDF files
# Link: https://www.ncsl.org/about-state-legislatures/state-partisan-composition

filepath_1 <- here("data", "ncsl", "Legis_Control_2-2021.xlsx")
filepath_2 <- here("data", "ncsl", "Legis_Control_February_2022.xlsx")
filepath_3 <- here("data", "ncsl", "2023-State-Legislative-Partisan-Composition-2-28-23.xlsx")
filepath_4 <- here("data", "ncsl", "Legis-Control-2024-3-1-24.xlsx")

legis_Control_2021 <- read_excel(filepath_1, range = "A2:M52", skip = 1) |>
  clean_names() |> mutate(year = 2021) 
legis_Control_2022 <- read_excel(filepath_2, range = "A2:N52", skip = 1) |>
  clean_names() |> select(-x7) |> mutate(year = 2022) 
legis_Control_2023 <- read_excel(filepath_3, range = "A2:N52", skip = 1) |>
  clean_names() |> select(-x7) |> mutate(year = 2023)
legis_Control_2024 <- read_excel(filepath_4, range = "A2:N52", skip = 1) |>
  clean_names() |> select(-x7) |> mutate(year = 2024)

names(legis_Control_2021)
names(legis_Control_2022)
names(legis_Control_2023)
names(legis_Control_2024)

state_poli_update <- legis_Control_2021 |>
  bind_rows(legis_Control_2022) |>
  bind_rows(legis_Control_2023) |>
  bind_rows(legis_Control_2024) |>
  rename (senate_total = total_senate) |>
  mutate(house_total = as.integer(total_house)) |>
  select(state, year, senate_total, senate_dem, senate_rep, senate_other,
         house_total, house_dem, house_rep, house_other) |>
  mutate(senate_dem_pct = senate_dem / senate_total,
         house_dem_pct = house_dem / house_total)

state_poli_compare_2 <- state_poli_1 |>
  filter(year >= 2021) |>
  left_join(state_poli_update, by = c("state", "year")) |>
  mutate(diff = senate_total.x - senate_total.y,
         diff_house = house_total.x - house_total.y,
         diff_dem = senate_dem.x - senate_dem.y,
         diff_rep = senate_rep.x - senate_rep.y,
         diff_house_dem = house_dem.x - house_dem.y,
         diff_house_rep = house_rep.x - house_rep.y) |>
  select(state, year, diff, diff_house, diff_dem, diff_rep, diff_house_dem, diff_house_rep) |>
  glimpse()

state_poli_formerge_2 <- state_poli_formerge |>
  select(-house_other, -senate_other)
  bind_rows(state_poli_update |> select (-house_other, -senate_other))

# Save data
saveRDS(state_poli_formerge_2, here("data", "state_poli.rds"))
write_csv(state_poli_formerge_2, here("data", "state_poli.csv"))

