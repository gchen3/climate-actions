library(here)
library(tidyverse)
library(readr)
library(gt)
library(ggplot2)
library(scales)

state_fin <- read_csv(here("data", "StateData.csv"))

names(state_fin)

state_fin_2 <- state_fin %>%
  rename(
    state = Name,
    year = Year4) |>
  rename_with(tolower) |>
  rename_with(~ gsub("_+", "_", tolower(.x))) |>
  select(state, year, population, total_revenue, total_rev_own_sources, general_revenue, gen_rev_own_sources,
         total_taxes, property_tax, total_ig_revenue, total_fed_ig_revenue, fed_igr_natural_res, total_state_ig_revenue,
         chg_total_nat_res, chg_parks_recreation, total_expenditure, total_capital_outlays, general_expenditure,
         total_ig_expenditure, natural_res_total_exp, parks_rec_total_exp, total_debt_outstanding, total_long_term_debt_out,
         st_debt_end_of_year, public_welf_total_exp, public_welf_cash_asst, public_welf_direct_exp) 

state_fin_3 <- state_fin_2 |>
  mutate(surplus_ratio = (total_revenue - total_expenditure) / total_revenue,
         debt_LT_ratio = total_long_term_debt_out / total_revenue,
         debt_ST_ratio = st_debt_end_of_year / total_revenue,
         debt_total_ratio = total_debt_outstanding / total_revenue,
         total_revenue_per_capita = total_revenue / population,
         total_expenditure_per_capita = total_expenditure / population,
         total_debt_per_capita = total_debt_outstanding / population,
         own_rev_ratio = total_rev_own_sources / total_revenue,
         ig_revenue_ratio = total_ig_revenue / total_revenue,
         chg_total_nat_res_ratio = chg_total_nat_res / total_revenue,
         chg_parks_recreation_ratio = chg_parks_recreation / total_revenue,
         natural_res_total_exp_ratio = natural_res_total_exp / total_expenditure,
         parks_rec_total_exp_ratio = parks_rec_total_exp / total_expenditure)

state_fin_4 <- state_fin_3 |>
  select(state, year, population, total_revenue, total_expenditure, total_debt_outstanding,
         surplus_ratio, debt_LT_ratio, debt_ST_ratio, debt_total_ratio,
         total_revenue_per_capita, total_expenditure_per_capita, total_debt_per_capita,
         own_rev_ratio, ig_revenue_ratio, chg_total_nat_res_ratio, chg_parks_recreation_ratio,
         natural_res_total_exp_ratio, parks_rec_total_exp_ratio)

# Save data
saveRDS(state_fin_4, here("data", "state_fin.rds"))
write_csv(state_fin_4, here("data", "state_fin.csv"))
  
# Make descriptive plots
year_rev_exp <- state_fin_3 |>
  group_by(year) |>
  filter(year >= 2000) |>
  summarize(
    avg_revenue = mean(total_revenue_per_capita, na.rm = TRUE),
    avg_expenditure = mean(total_expenditure_per_capita , na.rm = TRUE)
  ) |>
  mutate(
    avg_revenue = avg_revenue * 1000,  # Convert to dollars
    avg_expenditure = avg_expenditure * 1000  # Convert to dollars
  ) |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_revenue, color = "Average Revenue"), size = 1.2) +
  geom_line(aes(y = avg_expenditure, color = "Average Expenditure"), size = 1.2) +
  scale_color_manual(
    values = c("Average Revenue" = "darkgreen", 
               "Average Expenditure" = "firebrick")
  ) +
  scale_y_continuous(
    limits = c(0, 15000),
    breaks = seq(0, 15000, by = 2000),
    labels = scales::label_dollar()
  ) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Average State Revenues and Expenditures per Capita Over Time",
    subtitle = "Amounts shown in nominal USD",
    x = "Year",
    y = "Amount (USD)",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

year_rev_exp

debt_ratio_plot <- state_fin_3 |>
  group_by(year) |>
  filter(year >= 2000) |>
  summarize(
    avg_debt_total_ratio = mean(debt_total_ratio, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = avg_debt_total_ratio)) +
  geom_line(color = "darkblue", size = 1.2) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::label_percent()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Average State Debt to Revenue Ratio Over Time",
    subtitle = "Debt as a percentage of total revenue",
    x = "Year",
    y = "Debt to Revenue Ratio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

debt_ratio_plot

surplus_ratio_plot <- state_fin_3 |>
  group_by(year) |>
  filter(year >= 2000) |>
  summarize(
    avg_surplus_ratio = mean(surplus_ratio, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = avg_surplus_ratio)) +
  geom_line(color = "darkgreen", size = 1.2) +
  scale_y_continuous(
    limits = c(-0.8, 0.5),
    breaks = seq(-0.8, 0.5, by = 0.1),
    labels = scales::label_percent()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Average State Surplus Ratio Over Time",
    subtitle = "Surplus as a percentage of total revenue",
    x = "Year",
    y = "Surplus Ratio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

surplus_ratio_plot

parks_rec_revenue_plot <- state_fin_3 |>
  mutate(chg_parks_recreation_per_capita = (chg_parks_recreation * 1000) / population) |>
  group_by(year) |>
  filter(year >= 2000) |>
  summarize(
    avg_chg_parks_recreation_per_capita = mean(chg_parks_recreation_per_capita, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = avg_chg_parks_recreation_per_capita)) +
  geom_line(color = "orange", size = 1.2) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, by = 1),
    labels = scales::label_dollar()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Average State Parks and Recreation Revenue per Capita Over Time",
    subtitle = "Parks and Recreation revenue per capita in nominal USD",
    x = "Year",
    y = "Parks and Recreation Revenue per Capita (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

parks_rec_revenue_plot

parks_rec_exp_plot <- state_fin_3 |>
  mutate(parks_rec_exp_per_capita = (parks_rec_total_exp * 1000) / population) |>
  group_by(year) |>
  filter(year >= 2000) |>
  summarize(
    avg_parks_rec_exp_per_capita = mean(parks_rec_exp_per_capita, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = avg_parks_rec_exp_per_capita)) +
  geom_line(color = "purple", size = 1.2) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = scales::label_dollar()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Average State Parks and Recreation Expenditure per Capita Over Time",
    subtitle = "Parks and Recreation expenditure per capita in nominal USD",
    x = "Year",
    y = "Parks and Recreation Expenditure per Capita (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )
parks_rec_exp_plot

natural_res_exp_plot <- state_fin_3 |>
  mutate(natural_res_exp_per_capita = (natural_res_total_exp * 1000) / population) |>
  group_by(year) |>
  filter(year >= 2000) |>
  summarize(
    avg_natural_res_exp_per_capita = mean(natural_res_exp_per_capita, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = avg_natural_res_exp_per_capita)) +
  geom_line(color = "blue", size = 1.2) +
  scale_y_continuous(
    limits = c(0, 200),
    breaks = seq(0, 200, by = 20),
    labels = scales::label_dollar()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Average State Natural Resources Expenditure per Capita Over Time",
    subtitle = "Natural Resources expenditure per capita in nominal USD",
    x = "Year",
    y = "Natural Resources Expenditure per Capita (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

natural_res_exp_plot

natural_res_fedaid_plot <- state_fin_3 |>
  mutate(natural_res_fedaid_per_capita = (fed_igr_natural_res * 1000) / population) |>
  group_by(year) |>
  filter(year >= 2000) |>
  summarize(
    avg_natural_res_fedaid_per_capita = mean(natural_res_fedaid_per_capita, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = avg_natural_res_fedaid_per_capita)) +
  geom_line(color = "darkred", size = 1.2) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 5),
    labels = scales::label_dollar()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Average State Natural Resources Federal Aid per Capita Over Time",
    subtitle = "Natural Resources Federal Aid per capita in nominal USD",
    x = "Year",
    y = "Natural Resources Revenue per Capita (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

natural_res_fedaid_plot  

public_welf_total_exp_plot <- state_fin_3 |>
  mutate(public_welf_total_exp_per_capita = (public_welf_total_exp * 1000) / population) |>
  group_by(year) |>
  filter(year >= 2000) |>
  summarize(
    avg_public_welf_total_exp_per_capita = mean(public_welf_total_exp_per_capita, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = avg_public_welf_total_exp_per_capita)) +
  geom_line(color = "darkgreen", size = 1.2) +
  scale_y_continuous(
    limits = c(0, 5000),
    breaks = seq(0, 5000, by = 500),
    labels = scales::label_dollar()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Average State Public Welfare Total Expenditure per Capita Over Time",
    subtitle = "Public Welfare total expenditure per capita in nominal USD",
    x = "Year",
    y = "Public Welfare Total Expenditure per Capita (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

public_welf_total_exp_plot

public_welf_cash_asst_plot <- state_fin_3 |>
  mutate(public_welf_cash_asst_per_capita = (public_welf_cash_asst * 1000) / population) |>
  group_by(year) |>
  filter(year >= 2000) |>
  summarize(
    avg_public_welf_cash_asst_per_capita = mean(public_welf_cash_asst_per_capita, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = avg_public_welf_cash_asst_per_capita)) +
  geom_line(color = "orange", size = 1.2) +
  scale_y_continuous(
    limits = c(0, 200),
    breaks = seq(0, 200, by = 20),
    labels = scales::label_dollar()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Average State Public Welfare Cash Assistance per Capita Over Time",
    subtitle = "Public Welfare cash assistance per capita in nominal USD",
    x = "Year",
    y = "Public Welfare Cash Assistance per Capita (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

public_welf_cash_asst_plot
  
  

  
  

  
