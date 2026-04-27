
library(tidyverse)
# Importing datasets

labour_costs <- read_csv("lc_lci_r2_a$defaultview_linear.csv")

hicp_data <- read_csv("tec00118_page_linear.csv")

hpi_data <- read_csv("prc_hpi_a$defaultview_linear.csv")

# --- 2. DATA CLEANING ---

# 2.1 Clean Labour Costs
labour_costs_clean <- labour_costs %>%
  select(geo, TIME_PERIOD, nace_r2, lcstruct, unit, OBS_VALUE) %>%
  rename(
    country = geo,
    year = TIME_PERIOD,
    industry = nace_r2,
    cost_type = lcstruct,
    wage_growth = OBS_VALUE
  )

# 2.2 Clean General Inflation (HICP)
hicp_clean <- hicp_data %>%
  select(geo, TIME_PERIOD, unit, OBS_VALUE) %>%
  rename(
    country = geo,
    year = TIME_PERIOD,
    inflation_rate = OBS_VALUE
  )

# 2.3 Clean House Price Index (HPI)
hpi_clean <- hpi_data %>%
  select(geo, TIME_PERIOD, purchase, unit, OBS_VALUE) %>%
  rename(
    country = geo,
    year = TIME_PERIOD,
    hpi_value = OBS_VALUE
  )

# --- 3. QUICK CHECK ---
# Print the first few rows to make sure it looks good
head(labour_costs_clean)
head(hicp_clean)
head(hpi_clean)
