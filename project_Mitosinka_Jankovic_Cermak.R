
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

# 3.1 Get a list of all unique countries from each dataset
countries_lc <- unique(labour_costs_clean$country)
countries_hicp <- unique(hicp_clean$country)
countries_hpi <- unique(hpi_clean$country)

common_countries <- intersect(intersect(countries_lc, countries_hicp), countries_hpi)
print(paste("Number of common countries:", length(common_countries)))
print(common_countries)


# 3.3 Delete the excess countries from our datasets
labour_costs_filtered <- labour_costs_clean %>%
  filter(country %in% common_countries)

hicp_filtered <- hicp_clean %>%
  filter(country %in% common_countries)

hpi_filtered <- hpi_clean %>%
  filter(country %in% common_countries)


unique(hpi_filtered$purchase)
unique(hpi_filtered$unit)

unique(labour_costs_filtered$industry)
unique(labour_costs_filtered$cost_type)

# --- 4. FINÁLNÍ FILTROVÁNÍ KATEGORIÍ ---

# 4.1 HPI: Chceme jen celkové nákupy (Total) a roční změnu
hpi_final <- hpi_filtered %>%
  filter(purchase == "Total", 
         unit == "Annual average rate of change") # nebo "Annual average index, 2015=100"

# 4.2 Labour: Chceme celou ekonomiku, jen platy a roční změnu
labour_final <- labour_costs_filtered %>%
  filter(industry %in% c("B-S", "Industry, construction and services (except activities of households as employers and extra-territorial organisations and bodies)"), 
         cost_type %in% c("D11", "Wages and salaries (total)"), 
         unit == "Percentage change on previous period")

# 4.3 HICP: Jen kontrola, abychom měli stejnou jednotku
hicp_final <- hicp_filtered %>%
  filter(unit == "Annual average rate of change") 

# Kontrola, zda máme všude zhruba podobný počet řádků!
nrow(hpi_final)
nrow(labour_final)
nrow(hicp_final)

setdiff(countries_hpi, common_countries)