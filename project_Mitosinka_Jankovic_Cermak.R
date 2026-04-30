
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
length(original_countries_HPI)
length(original_countries_HICP)
length(original_countries_labour_costs)

# Printing number of deleted countries from each dataset
dropped_countries_HPI <- setdiff(original_countries_HPI, complete_countries)
length(dropped_countries_HPI)
dropped_countries_HICP <- setdiff(original_countries_HICP, complete_countries)
length(dropped_countries_HICP)
dropped_countries_labour_costs <- setdiff(original_countries_labour_costs, complete_countries)
length(dropped_countries_labour_costs)

# Print the numbers of countries studied before and after removing countries containing empty fields
countries_before_NA <- unique(final_data$country)
print("Countries before removing empty fields")
length(countries_before_NA)
print(countries_before_NA)
print("Final amount + number of countries studied")
length(complete_countries)
print(complete_countries)

# Beginning of analysis
# Five number summary for each variable
print("Five number Summary")
summary_final <- final_data_complete %>%
  select(hpi_value, wage_growth, inflation_rate, affordability_gap) %>%
  summary()
print(summary_final)

# Mean and median for each country
print("Mean and median by country")
country_stats <- final_data_complete %>%
  group_by(country) %>%
  summarise(
    mean_HPI = mean(hpi_value),
    median_HPI = median(hpi_value),
    mean_wage = mean(wage_growth),
    median_wage = median(wage_growth),
    mean_inflation = mean(inflation_rate),
    median_inflation = median(inflation_rate)
  ) %>%
  arrange(desc(mean_HPI))

print(as.data.frame(country_stats))

# Boxplots for HPI, Wage growth, Inflation rate, Affordability gap
boxplot(final_data_complete$hpi_value, 
        final_data_complete$wage_growth, 
        final_data_complete$inflation_rate, 
        final_data_complete$affordability_gap,
        names = c("HPI\n(House Prices)", "Wage\nGrowth", "Inflation\nRate", "Affordability\nGap"),
        col = c("lightblue", "lightgreen", "pink", "orange"),
        main = "Distribution of Economic Indicators",
        ylab = "Percentage Change (%)")
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

# Frequency tables  
# HPI
hpi_freq <- final_data_complete %>%
  mutate(Category = case_when(
    hpi_value < 0 ~ "1. Negative (<0%)",
    hpi_value >= 0 & hpi_value <= 5 ~ "2. Moderate (0-5%)",
    hpi_value > 5 & hpi_value <= 10 ~ "3. High (5-10%)",
    hpi_value > 10 ~ "4. Extreme (>10%)"
  )) %>%
  count(Category, name = "Count") %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 1))

# Wage Growth
wage_freq <- final_data_complete %>%
  mutate(Category = case_when(
    wage_growth < 0 ~ "1. Negative (<0%)",
    wage_growth >= 0 & wage_growth <= 5 ~ "2. Moderate (0-5%)",
    wage_growth > 5 & wage_growth <= 10 ~ "3. High (5-10%)",
    wage_growth > 10 ~ "4. Extreme (>10%)"
  )) %>%
  count(Category, name = "Count") %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 1))

# Inflation Rate
inflation_freq_unified <- final_data_complete %>%
  mutate(Category = case_when(
    inflation_rate < 0 ~ "1. Negative (<0%)",
    inflation_rate >= 0 & inflation_rate <= 5 ~ "2. Moderate (0-5%)",
    inflation_rate > 5 & inflation_rate <= 10 ~ "3. High (5-10%)",
    inflation_rate > 10 ~ "4. Extreme (>10%)"
  )) %>%
  count(Category, name = "Count") %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 1))

# Affordability Gap
gap_freq <- final_data_complete %>%
  mutate(Category = case_when(
    affordability_gap < 0 ~ "1. Negative (<0%) - Affordable",
    affordability_gap >= 0 & affordability_gap <= 5 ~ "2. Moderate Gap (0-5%)",
    affordability_gap > 5 & affordability_gap <= 10 ~ "3. High Gap (5-10%)",
    affordability_gap > 10 ~ "4. Extreme Gap (>10%)"
  )) %>%
  count(Category, name = "Count") %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 1))

# Frequency tables results
print("HPI (House prices) FREQUENCY")
print(as.data.frame(hpi_freq))

print("WAGE GROWTH FREQUENCY")
print(as.data.frame(wage_freq))

print("INFLATION FREQUENCY")
print(as.data.frame(inflation_freq_unified))

print("AFFORDABILITY GAP FREQUENCY")
print(as.data.frame(gap_freq))

# Calculate Volatility (Standard Deviation)
calculate_volatility <- function(data_vector) {
  return(sd(data_vector, na.rm = TRUE))
}
# COUNTRY RANKING BY AFFORDABILITY GAP - Ranking from worst (HPI outpaces wages) to best (Wages outpace HPI)
country_gap_ranking <- final_data_complete %>%
  group_by(country) %>%
  summarise(
    avg_gap = mean(affordability_gap),
    gap_volatility = calculate_volatility(affordability_gap)
  ) %>%
  arrange(desc(avg_gap))

print("--- Country Ranking by Affordability Gap (Mean) ---")
print(as.data.frame(country_gap_ranking))

# GLOBAL VOLATILITY COMPARISON - Comparing which economic indicator is the most unstable
global_volatility <- final_data_complete %>%
  summarise(
    HPI_SD = calculate_volatility(hpi_value),
    Wage_SD = calculate_volatility(wage_growth),
    Inflation_SD = calculate_volatility(inflation_rate),
    Gap_SD = calculate_volatility(affordability_gap)
  )

print("Global Volatility (Standard Deviation)")
print(global_volatility)