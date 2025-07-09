library(readxl)
library(tidyverse)
library(here)
library(janitor)

# Load and clean state fund RDF data
state_fund <- read_excel(
  here("data", "nasbo", "NASBO State Rainy Day Fund Balances Data.xlsx"),
  sheet = "RDF Balances (% of GF Spending)",
  range = "A1:AM51",
  skip = 1
)

names(state_fund)

state_fund_clean <- state_fund |>
  select(-"2025") |>
  pivot_longer(
    cols = -c(State),
    names_to = "year",
    values_to = "RDF_pct_of_GF_spending"
  ) |>
  rename(state = State) |>
  mutate(year = as.integer(year))


# load and clean state fund exp data --------------------------------------

# Dollar amounts in the dataset are reported in millions
state_fund_exp <- read_excel(
  here("data", "nasbo", "State Exp Report Data 1991-2024.xlsm"),
  sheet = "STATE EXP REPORT DATA"
) |>
  rename(state = "...2") |>
  select(ENVCP_GF, 
         ENVCP_FF, 
         ENVCP_OF, 
         ENVCP_BF, 
         ENVCP_TOT,
         GFTOT_CAPI,
         FFTOT_CAPI,
         OFTOT_CAPI,
         BFTOT_CAPI,
         TOTAL_CAPI,
         GFTOT_CAP,
         FFTOT_CAP,
         OFTOT_CAP,
         BFTOT_CAP,
         TOTAL_CAP) |>
  rename_with(tolower) |>
  mutate(across(everything(), ~ .x * 1e6)) |>  # Convert from millions to actual amounts
  mutate(envcp_tot_pct = (envcp_tot / total_capi) * 100) # Calculate Environmental Capital as a percentage of total fund 
  
# GFTOT_CAPI	General Funds Total Spending (non-capital and capital)
# FFTOT_CAPI	Federal FundsTotal Spending (non-capital and capital)
# OFTOT_CAPI	Other State Funds Total Spending (non-capital and capital)
# BFTOT_CAPI	Bond FundsTotal Spending (non-capital and capital)
# TOTAL_CAPI	Total Spending (non-capital and capital)
# GFTOT_CAP	General Funds Total Capital Spending
# FFTOT_CAP	Federal Funds Total Capital Spending
# OFTOT_CAP	Other State Funds Total Capital Spending
# BFTOT_CAP	Bond Funds Total Capital Spending
# TOTAL_CAP	Total Capital Spending
# ENVCP_GF	Enviromental Capital General Funds
# ENVCP_FF	Enviromental Capital Federal Funds
# ENVCP_OF	Enviromental Capital Other State Funds
# ENVCP_BF	Enviromental Capital Bond Funds
# ENVCP_TOT	Enviromental Capital Total Funds


# plot the data -----------------------------------------------------------

library(ggplot2)
RDF_year_plot <- state_fund_clean |>
  group_by(year) |>
  mutate(year = as.integer(year)) |>
  summarize(
    avg_rdf = mean(RDF_pct_of_GF_spending, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_rdf), color = "steelblue", size = 1.2) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, by = 2),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::percent_format(scale = 0.1)) +
  labs(
    title = "Average State Rainy Day Fund Balances Over Time",
    subtitle = "Balances as a % of General Fund Spending",
    x = "Year",
    y = "RDF Balance (% of GF Spending)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

# Save the plot
ggsave(
  here("results", "RDF_year_plot.png"),
  plot = RDF_year_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# RDF as GF spending in 2024 by state plot --------------------------------

state_fund_clean |>
  filter(year == "2024") |>
  mutate(RDF_pct_of_GF_spending = as.numeric(RDF_pct_of_GF_spending)) |>
  ggplot(aes(x = reorder(state, -RDF_pct_of_GF_spending), y = RDF_pct_of_GF_spending)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "State Rainy Day Fund Balances in 2024",
    subtitle = "Balances as a % of General Fund Spending",
    x = "state",
    y = "RDF Balance (% of GF Spending)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8,
                               margin = margin(t = 5))
  )

# Save the bar plot
ggsave(
  here("results", "RDF_2024_by_state.png"),
  width = 10,
  height = 6,
  dpi = 300
)

# Save the cleaned data
saveRDS(state_fund_clean, here("data", "state_fund.rds"))
write_csv(state_fund_clean, here("data", "state_fund.csv"))
