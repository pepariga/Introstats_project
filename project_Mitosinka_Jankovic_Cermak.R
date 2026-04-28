
library(tidyverse)
# Importing datasets

labour_costs <- read_csv("lc_lci_r2_a$defaultview_linear.csv")

hicp_data <- read_csv("tec00118_page_linear.csv")

hpi_data <- read_csv("prc_hpi_a$defaultview_linear.csv")


# Cleaning Labour Costs
labour_costs_clean <- labour_costs %>%
  select(geo, TIME_PERIOD, nace_r2, lcstruct, unit, OBS_VALUE) %>%
  rename(
    country = geo,
    year = TIME_PERIOD,
    industry = nace_r2,
    cost_type = lcstruct,
    wage_growth = OBS_VALUE
  )

# Cleaning HICP
hicp_clean <- hicp_data %>%
  select(geo, TIME_PERIOD, unit, OBS_VALUE) %>%
  rename(
    country = geo,
    year = TIME_PERIOD,
    inflation_rate = OBS_VALUE
  )

# Cleaning HPI
hpi_clean <- hpi_data %>%
  select(geo, TIME_PERIOD, purchase, unit, OBS_VALUE) %>%
  rename(
    country = geo,
    year = TIME_PERIOD,
    hpi_value = OBS_VALUE
  )


# Get lists of unique countries from each dataset
countries_lc <- unique(labour_costs_clean$country)
countries_hicp <- unique(hicp_clean$country)
countries_hpi <- unique(hpi_clean$country)

common_countries <- intersect(intersect(countries_lc, countries_hicp), countries_hpi)
print(common_countries)


# Deleting excess countries from each dataset
labour_costs_filtered <- labour_costs_clean %>%
  filter(country %in% common_countries)

hicp_filtered <- hicp_clean %>%
  filter(country %in% common_countries)

hpi_filtered <- hpi_clean %>%
  filter(country %in% common_countries)


# Give us only total annual % change from last year
hpi_final <- hpi_filtered %>%
  filter(purchase == "Total", 
         unit == "Annual average rate of change") 

# We want the wages and salaries for the whole ekonomy in % difference acording to last year
labour_final <- labour_costs_filtered %>%
  filter(industry %in% c("B-S", "Industry, construction and services (except activities of households as employers and extra-territorial organisations and bodies)"), 
         cost_type %in% c("D11", "Wages and salaries (total)"), 
         unit == "Percentage change on previous period")

# Just cheking so we have also % change from last year
hicp_final <- hicp_filtered %>%
  filter(unit == "Annual average rate of change") 



# Preparing tables for unification
hpi_for_join <- hpi_final %>% select(country, year, hpi_value)
labour_for_join <- labour_final %>% select(country, year, wage_growth)
hicp_for_join <- hicp_final %>% select(country, year, inflation_rate)


# Merging tables together by country and year
final_data <- hpi_for_join %>%
  inner_join(labour_for_join, by = c("country", "year")) %>%
  inner_join(hicp_for_join, by = c("country", "year"))

# creating new column for affordability gap
final_data <- final_data %>%
  mutate(affordability_gap = hpi_value - wage_growth)


# Removing rows with empty fields
final_data_no_NA <- final_data %>%
  drop_na(hpi_value, wage_growth, inflation_rate, affordability_gap)

# Removing countries with different amount of years than 10 (only less than 10 is possible)
final_data_complete <- final_data_no_NA %>%
  group_by(country) %>%
  filter(n_distinct(year) == 10) %>% 
  ungroup() 

# Checking what countries were delete from each dataset
complete_countries <- unique(final_data_complete$country)
original_countries_HPI <- unique(hpi_data$geo)
original_countries_HICP <- unique(hicp_data$geo)
original_countries_labour_costs <- unique(labour_costs$geo)

# Printing number of deleted countries from each dataset
dropped_countries_HPI <- setdiff(original_countries_HPI, complete_countries)
length(dropped_countries_HPI)
dropped_countries_HICP <- setdiff(original_countries_HICP, complete_countries)
length(dropped_countries_HICP)
dropped_countries_labour_costs <- setdiff(original_countries_labour_costs, complete_countries)
length(dropped_countries_labour_costs)

# Print the total number of countries studied
length(complete_countries)


