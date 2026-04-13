# climate-actions

## Project workflow

This project builds a state-year panel that combines disaster damage, state fiscal conditions, rainy day fund balances, and partisan composition.

### 1. Raw data sources

Raw inputs are stored under `data/`:

- `data/censusGFD`: state fiscal data
- `data/sheldus`: SHELDUS disaster damage data
- `data/ncsl`: state partisan composition data
- `data/nasbo`: rainy day fund data

### 2. Data preparation

Scripts in `data_prep/` clean each source and save standardized `.rds` and `.csv` files to `data/`.

- `data_prep/state_fin.R`
  - Reads Census GFD state finance data
  - Renames variables and computes fiscal ratios and per-capita measures
  - Writes `data/state_fin.rds` and `data/state_fin.csv`

- `data_prep/state_fund.R`
  - Reads NASBO rainy day fund data
  - Reshapes state-year RDF balances
  - Writes `data/state_fund.rds` and `data/state_fund.csv`
  - Also creates summary figures in `results/`

- `data_prep/state_poli.R`
  - Reads and updates state partisan composition data
  - Produces state-year legislative composition variables
  - Writes `data/state_poli.rds` and `data/state_poli.csv`

- `data_prep/sheldus_revised.R`
  - Reads SHELDUS disaster damage data
  - Creates a long `state-year-hazard` file
  - Creates a wide `state-year` damage file with hazard-specific columns and summary damage measures
  - Writes `data/sheldus_long.rds`, `data/sheldus_long.csv`, `data/sheldus.rds`, and `data/sheldus.csv`

Note: `data_prep/sheldus.R` is an older SHELDUS script. The current saved SHELDUS outputs appear to correspond to `data_prep/sheldus_revised.R`.

### 3. Data merge

`data_prep/data_merge.R` merges the cleaned datasets by `state` and `year`.

It:

- standardizes state names across sources
- removes non-state territories from the SHELDUS file before merging
- left-joins fiscal, SHELDUS, political, and rainy day fund data
- fills missing disaster-related values with zero

Outputs:

- `data/merged_data.rds`
- `data/merged_data.csv`

### 4. Analysis

`analysis/descriptive_stats.R` reads `data/merged_data.rds` and produces descriptive statistics for:

- disaster damage variables
- fiscal variables
- political variables

Outputs are saved in `results/`, including the descriptive statistics table image.

### 5. Results and presentation

The `results/` folder stores generated figures and tables from the prep and analysis scripts.

The `webpage/` folder contains a Quarto site for presenting the project.

## Recommended run order

Run the scripts in this order:

1. `data_prep/state_fin.R`
2. `data_prep/state_fund.R`
3. `data_prep/state_poli.R`
4. `data_prep/sheldus_revised.R`
5. `data_prep/data_merge.R`
6. `analysis/descriptive_stats.R`
