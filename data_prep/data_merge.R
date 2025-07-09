library(tidyverse)

state_fin <- readRDS(here("data", "state_fin.rds"))

sheldus <- readRDS(here::here("data", "sheldus.rds"))
state_poli <- readRDS(here("data", "state_poli.rds"))
state_fund <- readRDS(here("data", "state_fund.rds"))

state_fin_1 <- state_fin |>
  mutate(state = str_to_lower(state)) |>
  mutate(state = str_remove(state, " state government")) |>
  mutate(state = str_remove(state, " state govt")) |>
  mutate(state = str_remove(state, " city")) |>
  filter(state != "washington dc")

state_fin_1 |> select(state) |> unique() |> print(n=99)

sheldus_1 <- sheldus |>
  mutate(state = str_to_lower(state)) |>
  filter(state != "american samoa") |>
  filter(state != "commonwealth of the northern mariana islands") |>
  filter(state != "district of columbia") |>
  filter(state != "guam") |>
  filter(state != "puerto rico") |>
  filter(state != "virgin islands")

sheldus_1 |> select(state) |> unique() |> print(n=99)

state_poli_1 <- state_poli |>
  mutate(state = str_to_lower(state)) 

state_poli_1 |> select(state) |> unique() |> print(n=99)

state_fund_1 <- state_fund |>
  mutate(state = str_to_lower(state))

state_fund_1 |> select(state) |> unique() |> print(n=99)

# Merge datasets
merged_data <- state_fin_1 %>%
  left_join(sheldus_1, by = c("state", "year")) %>%
  left_join(state_poli_1, by = c("state", "year")) %>%
  left_join(state_fund_1, by = c("state", "year")) %>%
  select(-chg_total_nat_res_ratio, -chg_parks_recreation_ratio,
  -natural_res_total_exp_ratio, -parks_rec_total_exp_ratio) %>%
  filter(year >= 2000)

merged_data_fill <- merged_data %>%
  mutate(across(
    matches("^(cropdmg|cropdmg_adj|cropdmgpercapita|propertydmg|propertydmg_adj|propertydmgpercapita|total_damage|climate_related_damage|total_damage|climate_related_damage)_"),
    ~ replace_na(., 0)
  )) |>
  mutate(across(
    matches("^(total_damage|climate_related_damage)$"),
    ~ replace_na(., 0)
  ))
  

saveRDS(merged_data_fill, here("data", "merged_data.rds"))
write.csv(merged_data_fill, here("data", "merged_data.csv"), row.names = FALSE)

# Display the structure of the merged data
glimpse(merged_data_fill)

# Check for missing values in the merged data
missing_values <- merged_data_fill %>%
  filter(state != "nebraska") %>%  # Exclude Nebraska as it has no data
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  filter(missing_count > 0)