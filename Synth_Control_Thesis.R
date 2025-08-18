# ==============================================================================
# Title:    Analysis for Dissertation – "Between Demolition & Development:
#           The Impact of Slum Demolitions on Housing Prices 
#           in the Makkah Region"
# Author:   Almaha Alghamdi
# Date:     August 2025
# Purpose:  This script contains the analysis conducted for the dissertation.
#           It examines the effects of slum demolitions on housing prices in 
#           the Makkah region. 
#           This is the script for the Synthetic Control Method.
# ==============================================================================-

# Set working directory
setwd("C:/Users/Lenovo/Desktop/Thesis/Topic 5 - Demolitions Impact on Housing Prices")

# Download Packages 
library(tidyverse)
library(broom)
library(scales)
library(wooldridge)
library(dplyr)
library(fixest)
library(data.table)
library(leaflet)
library(rdrobust)
library(rddensity)
library(readxl)
library(purrr)
library(lubridate)
library(Synth)
library(leaflet.extras)
library(sf)
library(tmap)
library(tools)
library(ggmap)
library(ggspatial) 
library(osmdata)
library(gsynth)
library(gtable)
library(synthdid)
library(modelsummary)
library(MatchIt)

#--Starting with Combining Multiple Excel Sheets to become Main Dataset--#
# Set working director for files
file_path <- "C:/Users/Lenovo/Desktop/Thesis/Topic 5 - Demolitions Impact on Housing Prices/Transactions"

#---The dataset used in the following can be found here: https://en.aqarsas.sa/---# 

# Get a list of all Excel files in the directory
file_list <- list.files(file_path, pattern = "*.xlsx", 
                        full.names = TRUE)

# Read and combine all Excel files
combined_data <- file_list %>%
  map_df(~ read_excel(.))

# Rename columns (i.e translate to English)
combined_data <- combined_data %>%
  rename(
    transaction_id = `رقم الصفقة`,
    category = التصنيف,
    type = النوع,
    transaction_price = `سعر الصفقة (ريال)`,
    area_sqm = `المساحة (متر مربع)`,
    price_per_sqm = `سعر المتر المربع (ريال)`,
    transaction_date = `تاريخ الصفقة`,
    region = المنطقة,
    city = المدينة,
    neighborhood = الحي,
    plan = المخطط,
    plot = القطعة,
    longitude = lng,
    latitude = ltd
  )

# Remove unnecessary columns 
combined_data <- combined_data %>%
  select(
    -`location accuracy level`,
    -abnormality_decision,
    -abnormality_confidence )

# Adjust date format
combined_data <- combined_data %>%
  mutate(transaction_date = dmy(transaction_date))

# Creating vector map to translate categories and property types
# Translation for 'category'
category_translation <- c(
  "سكني" = "Residential"
)

# Translation for 'type'
type_translation <- c(
  "قطعة أرض" = "Land Parcel",
  "عمارة" = "Building",
  "شقة" = "Apartment",
  "فيلا" = "Villa",
  "معرض/محل" = "Shop/Store",
  "مركز تجاري" = "Shopping Center",
  "بيت" = "House",
  "إستراحة" = "Istiraha",
  "مرفق" = "Facility",
  "قصر" = "Palace",
  "وحدة عقارية" = "Property Unit",
  "وحدة سكنية" = "Residential Unit",
  "وحدة تجارية" = "Commercial Unit",
  "مكتب" = "Office",
  "محل تجاري" = "Commercial Store",
  "سطح" = "Rooftop",
  "شاليه" = "Chalet"
)

# Secondly, applying translations
# Translate category and type
combined_data <- combined_data %>%
  mutate(
    category = recode(category, !!!category_translation),
    type = recode(type, !!!type_translation)
  )

# Check if it worked properly
unique(combined_data$category)
unique(combined_data$type)

# Translations for 'region'
region_translation <- c(
  "مكة المكرمة" = "Makkah",
  "المدينة المنورة" = "Madinah",
  "الشرقية" = "Eastern Province",
  "الرياض" = "Riyadh",
  "القصيم" = "Qassim",
  "جيزان" = "Jazan",
  "عسير" = "Asir",
  "الجوف" = "Al-Jouf",
  "نجران" = "Najran",
  "تبوك" = "Tabuk",
  "الحدود الشمالية" = "Northern Borders",
  "حايل" = "Hail",
  "الباحة" = "Al-Baha"
)

# Apply translations 
combined_data <- combined_data %>%
  mutate(
    region = recode(region, !!!region_translation)
  )

# Check unique regions
unique(combined_data$region)
str(combined_data)

# Adding the recent 2025 dataset here
new_2025 <- read.csv("Transactions/2025.csv")

# Understand the data
str(new_2025)

#--Start Cleaning the 2025 data to match the combined--#
# 1. change the column names to match
new_2025 <- new_2025 %>%
  rename(transaction_id = Transaction.No.,
         plan = Plan.No.,
         transaction_date = Deal.Date,
         type = MOJ.Type,
         category = Use,
         price_per_sqm = Price.per.sqm,
         area_sqm = Area.Size,
         transaction_price = Transaction.Price,
         region = Region)

# 2. Clean and convert character fields to numbers
new_2025 <- new_2025 %>%
  mutate(transaction_id = as.character(transaction_id),
         price_per_sqm = as.numeric(gsub(",", "", price_per_sqm)),
         area_sqm = as.numeric(gsub(",", "", area_sqm)),
         transaction_price = as.numeric(gsub(",", "", transaction_price)),
         transaction_date = as.Date(transaction_date))

unique(new_2025$region)

# 3. Standardize the region names to match 
region_map_2025 <- c(
  "Mecca Region" = "Makkah",
  "Medina Region" = "Madinah",
  "Al Jawf Region" = "Al-Jouf",
  "Riyadh Region" = "Riyadh",
  "Tabuk Region" = "Tabuk",
  "Asir Region" = "Asir",
  "Jazan Province" = "Jazan",
  "Ha'il Region" = "Hail",
  "Al Qassim Province" = "Qassim",
  "Najran Region" = "Najran",
  "Al Bahah Region" = "Al-Baha",
  "Northern Borders Region" = "Northern Borders"
)

# 4. Recode region names accordingly 
new_2025 <- new_2025 %>%
  mutate(region = recode(region, !!!region_map_2025))

# 5. Fill missing columns with NAs to merge seamlessly
new_2025 <- new_2025 %>%
  mutate(
    city = NA_character_,
    neighborhood = NA_character_,
    plot = NA_character_,
    longitude = NA_character_,
    latitude = NA_character_
  ) %>%
  select(
    transaction_id, category, type, transaction_price, area_sqm, price_per_sqm,
    transaction_date, region, city, neighborhood, plan, plot, longitude, latitude
  )

# 6. Merge datasets
combined_data <- bind_rows(combined_data, new_2025)

#---Filtering and rearranging the dataset for better clarity---#
# Filtering data and removing unnecessary columns
filtered_data <- combined_data %>%
  select(-neighborhood, -plan, -plot, -city)

# Rearrange data by Year to allow for aggregation
filtered_data <- filtered_data %>%
  mutate(year = year(transaction_date))
filtered_data <- filtered_data %>%
  arrange(year) %>%
  select(year, everything())

#---Al relevant datasets used in shaping covariates can be found here: https://www.stats.gov.sa/en/---#
#--Second, we add average salaries--#
#--- Step 1: Load and Clean Region Labels ---#
average_salaries <- read.csv("Macro Data Adjusted for R/Average Salaries for R.csv")

average_salaries <- average_salaries %>%
  mutate(
    region = ifelse(grepl("Region Office|office|Governorate Office", X, ignore.case = TRUE), trimws(X), NA),
    region = gsub("(?i)Region Office - Jeddah", "", region, perl = TRUE),
    region = gsub("(?i)Region Office", "", region, perl = TRUE),
    region = gsub("(?i)Governorate Office", "", region, perl = TRUE),
    region = gsub("(?i)office", "", region, perl = TRUE),
    region = trimws(region)
  ) %>%
  fill(region, .direction = "down")

#--- Step 2: Keep Only Male (2) and Female (2) Rows ---#
average_salaries <- average_salaries %>%
  filter(grepl("\\(2\\)", X)) %>%
  select(-X)

#--- Step 3: Reshape to Long Format ---#
average_salaries <- average_salaries %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "monthly_avg_salary"
  ) %>%
  mutate(
    year = as.integer(gsub("X", "", year)),
    monthly_avg_salary = as.numeric(monthly_avg_salary)
  ) %>%
  filter(!is.na(monthly_avg_salary))

#--- Step 4: Aggregate by Region-Year ---#
average_salaries <- average_salaries %>%
  group_by(region, year) %>%
  summarize(
    monthly_avg_salary = mean(monthly_avg_salary, na.rm = TRUE),
    .groups = "drop"
  )

#--- Step 5: Compute Annual Salary ---#
average_salaries <- average_salaries %>%
  mutate(annual_avg_salary = monthly_avg_salary * 12)

#--- Step 6: Map Region Names to Match Main Dataset ---#
region_salary_mapping <- c(
  "Riyadh" = "Riyadh",
  "Makkah" = "Makkah",
  "Eastern" = "Eastern Province",
  "Eastern District" = "Eastern Province",
  "Madinah" = "Madinah",
  "Tabuk" = "Tabuk",
  "Northern Border District" = "Northern Borders",
  "Al-Qassim" = "Qassim",
  "Al-Jouf" = "Al-Jouf",
  "Hail" = "Hail",
  "Al-Baha" = "Al-Baha",
  "Makkah - Jeddah" = "Makkah",
  "Aseer" = "Asir",
  "Najran" = "Najran",
  "Jazan" = "Jazan"
)

average_salaries <- average_salaries %>%
  mutate(region = recode(region, !!!region_salary_mapping))

#--- Step 7: Ensure No Duplicates by Re-aggregating if Needed ---#
average_salaries <- average_salaries %>%
  group_by(region, year) %>%
  summarize(
    monthly_avg_salary = mean(monthly_avg_salary, na.rm = TRUE),
    annual_avg_salary = mean(annual_avg_salary, na.rm = TRUE),
    .groups = "drop"
  )

#--- Step 8: Merge into Main Dataset ---#
main_data <- filtered_data %>%
  inner_join(average_salaries, by = c("region", "year"))


# Adding Population 
population <- read.csv("Macro Data Adjusted for R/Population by Region for R.csv")

# Step 1: Sum both female and male population
population <- population %>%
  group_by(Region, Year) %>%
  summarize(
    total_population = sum(Population, na.rm = TRUE), # Sum all population values
    .groups = "drop" # Ungroup after summarizing
  )

# Step 2: Standardize region names
region_pop_mapping <- c(
  "Ar Riyadh" = "Riyadh",
  "Makkah Al Mukarramah" = "Makkah",
  "Eastern Region" = "Eastern Province",
  "Al Madinah Al Munawwarah" = "Madinah",
  "Al Bahah" = "Al-Baha",
  "Al Qaseem" = "Qassim",
  "Al Jawf" = "Al-Jouf",
  "Aseer" = "Asir",
  "Najran" = "Najran",
  "Tabuk" = "Tabuk",
  "Hail" = "Hail",
  "Jazan" = "Jazan",
  "Northern Borders" = "Northern Borders"
)

# Step 3: Apply region mapping
population <- population %>%
  mutate(Region = recode(Region, !!!region_pop_mapping))

unique(population$Region)
# Step 4: Merge data with main_data 
main_data <- main_data %>%
  inner_join(population, by = 
               c("region" = "Region", "year" = "Year"))

# Step 5: Check if regions are done correctly
unique(main_data$region)

#--Fourth, add population growth rates--#
# Step 1: Aggregate population by year
population_by_year <- main_data %>%
  group_by(region, year) %>%
  summarise(population = first(total_population), .groups = "drop")

# Step 2: Calculate growth rates by year 
population_growth <- population_by_year %>%
  group_by(region) %>%
  mutate(population_growth = (population - lag(population)) / lag(population) * 100) %>%
  ungroup()

# Step 3: Merge into the main data
main_data <- main_data %>%
  left_join(population_growth %>% select(region, year, population_growth),
            by = c("region", "year"))

#--Fifth, add tourist figures (inbound and domestic)--#
inbound_tourists <- read.csv("Macro Data Adjusted for R/Inbound Tourists for R.csv")
domestic_tourists <- read.csv("Macro Data Adjusted for R/Domestic Tourist.csv")

# Starting with Inbound Tourists, to make sure clean up process is thorough
# Step 1: Clean up inbound tourists by renaming first column for clarity
colnames(inbound_tourists)[1] <- "region"

# Step 2: Reshape the data to long format
inbound_tourists <- inbound_tourists %>%
  pivot_longer(
    cols = starts_with("X"),         
    names_to = "year",               
    values_to = "num_inbound_tourists"       
  ) %>%
  mutate(
    year = as.numeric(gsub("X", "", year)),  
    num_inbound_tourists = as.numeric(gsub(",", "", num_inbound_tourists))  
  )

# Step 3: 2017 data seem to be missing, thus we will be using a small 
# linear model to estimate that figure
inbound_tourists <- inbound_tourists %>%
  group_by(region) %>%
  mutate(
    num_inbound_tourists = ifelse(
      is.na(num_inbound_tourists) & sum(!is.na(num_inbound_tourists)) > 1,  # Check if enough data exists
      predict(lm(num_inbound_tourists ~ year, data = cur_data()), newdata = cur_data()),  # Predict missing
      num_inbound_tourists  # Keep original value if not missing or insufficient data
    )
  ) %>%
  ungroup()

# Step 4: Join with main_data (no recoding of regions is needed as 
# tourist indicators have been  extracted from a large excel workbook, 
# so the excel is already clean)
main_data <- main_data %>%
  left_join(inbound_tourists, by = c("region", "year"))

# Then we move to domestic tourists
# Step 1: Rename the first column for clarity
colnames(domestic_tourists)[1] <- "region"

# Step 2: Reshape the data to long format
domestic_tourists <- domestic_tourists %>%
  pivot_longer(
    cols = starts_with("X"),         # Columns to reshape
    names_to = "year",               # New column for years
    values_to = "num_tourists"       # New column for values
  ) %>%
  mutate(
    year = as.numeric(gsub("X", "", year)),  # Remove 'X' and convert year to numeric
    num_tourists = as.numeric(gsub(",", "", num_tourists))  # Remove commas and convert to numeric
  )

# Step 3: Merge to main data (no recoding of regions is needed as 
# tourist indicators have been  extracted from a large excel workbook, 
# so the excel is already clean)
main_data <- main_data %>%
  left_join(domestic_tourists, by = c("region", "year"))

# Step 4: Rename domestic tourists column in main_data for clarity
main_data <- main_data %>%
  rename(domestic_tourists = num_tourists)

# Step 5: Add total total tourists (sum inbound and domestic)
main_data <- main_data %>%
  mutate(total_tourists = domestic_tourists + num_inbound_tourists) %>% # Create new column
  select(-domestic_tourists, -num_inbound_tourists) # Remove original columns

# Then we move to employment figures
employment <- read.csv("Macro Data Adjusted for R/Employment for R.csv")

# Step 1: Rename the first column for clarity
colnames(employment)[1] <- "region"

# Step 2: Reshape the data to long format
employment <- employment %>%
  pivot_longer(
    cols = starts_with("X"),         # Columns to reshape
    names_to = "year",               # New column for years
    values_to = "total_employment"       # New column for values
  ) %>%
  mutate(
    year = as.numeric(gsub("X", "", year)),  # Remove 'X' and convert year to numeric
    total_employment = as.numeric(gsub(",", "", total_employment))  # Remove commas and convert to numeric
  )


# Step 4: Join with main_data (no recoding of regions is needed as 
# employment figures have been extracted from a large excel workbook, 
# so the excel is already clean)
main_data <- main_data %>%
  left_join(employment, by = c("region", "year"))

# Step 5: Normalize employment to account for size of regions and workforce 
main_data <- main_data %>% 
  mutate(normalized_employment = if_else(
    total_population > 0, 
    total_employment / total_population, 
    NA_real_
  ))

#---Here we add Mega Project Dummy & Religious Regions + Tourist 
# Intersection---#

# Step 1: Assign dummies for each megaproject to each region
main_data <- main_data %>%
  mutate(
    
    # Dummy for Masar in Makkah (announced June 2020, active from 2020)
    masar_dummy = ifelse(region == "Makkah" & year >= 2020, 1, 0),
    
    # Dummy for Jeddah Central in Jeddah (announced Dec 2021, active from 2022)
    jeddah_central_dummy = ifelse(region == "Makkah" & year >= 2022, 1, 0),
    
    # Dummy for Qiddiyah City in Riyadh (announced April 2017, active from 2017)
    qiddiyah_dummy = ifelse(region == "Riyadh" & year >= 2017, 1, 0),
    
    # Dummy for Rua Al Madinah in Madinah (announced 2022)
    rua_almadinah_dummy = ifelse(region == "Madinah" & year >= 2022, 1, 0),
    
    # Dummy for King Salman Airport in Riyadh (announced November 2022, active from 2022)
    king_salman_airport_dummy = ifelse(region == "Riyadh" & year >= 2022, 1, 0), 
    
    # Dummy for religious regions 
    religious_dummy = ifelse(region %in% c("Makkah", "Madinah"), 1, 0), 
  )

# Step 2: Aggregate into a single "Mega Project" dummy in order for SCM
# to read it correctly
main_data <- main_data%>%
  mutate(
    mega_project_dummy = as.numeric(
      masar_dummy == 1 | jeddah_central_dummy == 1 | 
        qiddiyah_dummy == 1 | rua_almadinah_dummy == 1 | 
        king_salman_airport_dummy == 1
    )
  ) %>%
  # Remove individual dummy variables
  select(-masar_dummy, -jeddah_central_dummy, 
         -qiddiyah_dummy, -rua_almadinah_dummy,
         -king_salman_airport_dummy)


# Step 3: Add interaction between religious regions and tourists, 
# emphasizing that religious regions benefit more from tourists 
# than other regions
main_data <- main_data %>%
  mutate(tourism_religious_interaction = religious_dummy * total_tourists)

# Here we add transaction volumes for each region across the years
main_data <- main_data %>%
  group_by(region, year) %>%
  mutate(transaction_volume = n()) %>%
  ungroup

#First, we add number of households
num_households <- read.csv("Macro Data Adjusted for R/Number of Households.csv")

#Understand data structure
str(num_households)

# 1. Clean and reshape the data 
num_households <- num_households %>%
  pivot_longer(cols = -Region, names_to = "year", values_to = "households") %>%
  mutate(
    year = as.integer(str_remove(year, "X")),
    households = as.numeric(str_replace_all(households, "[,\\s]", ""))
  )

# Step 2: Add missing years 2014–2025 per region
num_households <- expand.grid(
  Region = unique(num_households$Region),
  year = 2013:2025
) %>%
  left_join(num_households, by = c("Region", "year")) %>%
  arrange(Region, year)


# Step 3: CAGR Forecast Function 
forecast_households <- function(df) {
  df <- df %>% arrange(year)
  
  # Fill 2014–2016 using 2013–2017 CAGR
  if (all(c(2013, 2017) %in% df$year)) {
    val_2013 <- df$households[df$year == 2013]
    val_2017 <- df$households[df$year == 2017]
    
    if (!is.na(val_2013) && !is.na(val_2017)) {
      cagr_1317 <- (val_2017 / val_2013)^(1 / 4) - 1
      for (y in 2014:2016) {
        df$households[df$year == y] <- round(val_2013 * (1 + cagr_1317)^(y - 2013))
      }
    }
  }
  
  # Fill 2019–2020 using closest forward CAGR
  known <- df %>% filter(!is.na(households)) %>% arrange(year)
  if (any(df$year == 2018 & !is.na(df$households)) &&
      any(df$year == 2021 & !is.na(df$households))) {
    val_2018 <- df$households[df$year == 2018]
    val_2021 <- df$households[df$year == 2021]
    cagr_1821 <- (val_2021 / val_2018)^(1 / 3) - 1
    for (y in 2019:2020) {
      df$households[df$year == y] <- round(val_2018 * (1 + cagr_1821)^(y - 2018))
    }
  }
  
  # Forecast 2022–2025 using most recent 2 known years
  known_recent <- df %>% filter(!is.na(households), year >= 2015) %>% arrange(desc(year))
  if (nrow(known_recent) >= 2) {
    y1 <- known_recent$year[1]
    y2 <- known_recent$year[2]
    v1 <- known_recent$households[1]
    v2 <- known_recent$households[2]
    
    if (!is.na(v1) && !is.na(v2) && (y1 - y2) > 0) {
      cagr_recent <- (v1 / v2)^(1 / (y1 - y2)) - 1
      for (y in 2022:2025) {
        df$households[df$year == y] <- round(v1 * (1 + cagr_recent)^(y - y1))
      }
    }
  }
  
  return(df)
}

# Step 4: Apply function 
num_households <- num_households %>%
  group_by(Region) %>%
  group_modify(~ forecast_households(.x)) %>%
  ungroup()

# Step 5: rename region 
num_households <- num_households %>%
  rename(region = Region)

# Step 5: Merge with original dataset
main_data <- main_data %>%
  left_join(num_households, by = c("region", "year")) %>%
  mutate(households = if_else(is.na(households), 0, households))

#---Now we add household expenditure and we do the same thing---#
household_exp <- read.csv("Macro Data Adjusted for R/Household Expenditure.csv")

# Understand Data
str(household_exp)

# Step 1: Clean and reshape
household_exp <- household_exp %>%
  mutate(across(starts_with("X"), as.character)) %>%
  pivot_longer(cols = -Region, names_to = "year", values_to = "expenditure") %>%
  mutate(
    year = as.integer(str_remove(year, "X")),
    expenditure = as.numeric(str_replace_all(expenditure, "[,\\s]", ""))
  )

# Step 2: Expand year range
household_exp <- expand.grid(
  Region = unique(household_exp$Region),
  year = 2013:2025
) %>%
  left_join(household_exp, by = c("Region", "year")) %>%
  arrange(Region, year)

# Step 3: Forecast function
forecast_expenditure <- function(df) {
  df <- df %>% arrange(year)
  
  # Fill 2014–2017 using 2013–2018 CAGR
  if (all(c(2013, 2018) %in% df$year)) {
    val_2013 <- df$expenditure[df$year == 2013]
    val_2018 <- df$expenditure[df$year == 2018]
    if (!is.na(val_2013) && !is.na(val_2018)) {
      cagr_1318 <- (val_2018 / val_2013)^(1 / 5) - 1
      for (y in 2014:2017) {
        df$expenditure[df$year == y] <- round(val_2013 * (1 + cagr_1318)^(y - 2013))
      }
    }
  }
  
  # Fill 2019–2025 using 2018–2023 CAGR
  if (all(c(2018, 2023) %in% df$year)) {
    val_2018 <- df$expenditure[df$year == 2018]
    val_2023 <- df$expenditure[df$year == 2023]
    if (!is.na(val_2018) && !is.na(val_2023)) {
      cagr_1823 <- (val_2023 / val_2018)^(1 / 5) - 1
      for (y in 2019:2025) {
        df$expenditure[df$year == y] <- round(val_2018 * (1 + cagr_1823)^(y - 2018))
      }
    }
  }
  
  return(df)
}

# Step 4: Apply per region
household_exp <- household_exp %>%
  group_by(Region) %>%
  group_modify(~ forecast_expenditure(.x)) %>%
  ungroup()

# Step 5: rename region 
household_exp <- household_exp %>%
  rename(region = Region)

# Step 5: Merge with original dataset
main_data <- main_data %>%
  left_join(household_exp, by = c("region", "year")) %>%
  mutate(expenditure = if_else(is.na(expenditure), 0, expenditure))

# Here we are calculating Affordability and the ratio as well # 
# Set your assumptions
DBR <- 0.60
interest_rate <- 0.03   # Annual interest rate
tenure_years <- 25
LTV <- 0.60

# Calculate derived values
n_years <- tenure_years
r <- interest_rate

# Annuity present value factor
annuity_factor <- function(rate, years) {
  (1 - (1 + rate)^(-years)) / rate
}

# Apply calculations
affordability <- main_data %>%
  group_by(region, year) %>%
  summarise(
    annual_avg_salary = first(annual_avg_salary),
    max_monthly_payment = (annual_avg_salary / 12) * DBR,
    max_annual_payment = max_monthly_payment * 12,
    max_loan = max_annual_payment * annuity_factor(r, n_years),
    affordable_price = max_loan / LTV,
    .groups = "drop"
  )

# Merge onto clean data
main_data <- main_data %>%
  left_join(affordability %>% select(region, year, affordable_price), by = c("region", "year")) %>%
  mutate(
    affordability_ratio = affordable_price / transaction_price
  )

#---Starting cleaning process in prep for the SCM---#
# Step 1: Remove missing values from price per sqm, area, and transaction prices
main_data <- main_data %>%
  filter(!is.na(transaction_price), !is.na(price_per_sqm), !is.na(area_sqm))

# Step 2: Replace missing values in numerical predictors with 0 
main_data <- main_data %>%
  mutate(
    total_population = ifelse(is.na(total_population), 0, total_population),
    total_tourists = ifelse(is.na(total_tourists), 0, total_tourists),
    normalized_employment = ifelse(is.na(normalized_employment), 0, normalized_employment),
    total_employment = ifelse(is.na(total_employment), 0, total_employment),
    annual_avg_salary = ifelse(is.na(annual_avg_salary), 0, annual_avg_salary),
    monthly_avg_salary = ifelse(is.na(monthly_avg_salary), 0, monthly_avg_salary),
    population_growth = ifelse(is.na(population_growth), 0, population_growth),
    transaction_volume = ifelse(is.na(transaction_volume), 0, transaction_volume),
    expenditure = ifelse(is.na(expenditure), 0, expenditure),
    households = ifelse(is.na(households), 0, households)
  )

# Step 3: Convert transaction_date to Date type (if not already)
main_data <- main_data %>%
  mutate(transaction_date = as.Date(transaction_date, format = "%Y-%m-%d")
  )

# Step 4: Filter out rows where any of the specified columns have a value of 0
main_data_filtered <- main_data %>%
  filter(
    total_employment != 0,
    normalized_employment != 0,
    total_population != 0,
    total_tourists != 0,
    annual_avg_salary != 0,
    monthly_avg_salary != 0,
    population_growth != 0,
    transaction_volume != 0,
    expenditure != 0,
    households != 0
  )

#---Removing Outliers and Cleaning Property Types---#
# Step 1: Prepare outlier function
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR       
  upper_bound <- Q3 + 1.5 * IQR
  ifelse(x >= lower_bound & x <= upper_bound, x, NA)
}

# Outliers still exist, so we will enforce minimum acceptable area and price persqm
# based on market knowledge
min_area_sqm <- 50  
min_price_per_sqm <- 800

# Step 3: Apply function to remove outliers and enforce minimum thresholds
main_data_filtered <- main_data_filtered %>%
  mutate(
    transaction_price = remove_outliers(transaction_price),
    price_per_sqm = remove_outliers(price_per_sqm),
    area_sqm = remove_outliers(area_sqm)
  ) %>%
  filter(
    !is.na(transaction_price),
    !is.na(price_per_sqm),
    !is.na(area_sqm),
    area_sqm >= min_area_sqm,              # Enforce minimum area
    price_per_sqm >= min_price_per_sqm     # Enforce minimum price per sqm
  )

# Step 3: Filter dataset to only relevant property types, 
# that is Villas, Apartments, and House
main_data_filtered <- main_data_filtered %>%
  filter((type %in% c("Villa", "Apartment", "House")))

# Additional step, check for any duplicate transaction_id
main_data_filtered %>%
  count(transaction_id) %>%
  filter(n > 1)

# Additional step, Al Baha and Northern Borders seem to be missing data
# from 2016, 2020, and 2021, hence they will be removed
main_data_filtered <- main_data_filtered %>%
  filter(!region %in% c("Northern Borders", "Al-Baha"))

# Additional NEW step, here we add the log prices to see if it makes any
#difference 
main_data_filtered <- main_data_filtered %>%
  mutate(
    log_price_per_sqm = log1p(price_per_sqm)
  )

# Adjustment Step: Save this data for DiD
write.csv(main_data_filtered, "Clean Data for DiD.csv", row.names = FALSE)


#---SCM Analysis Prep---#
# Step 1: Add treated indicator & filter for relevant columns
scm_data <- main_data_filtered %>%
  select(
    year, region, transaction_date, transaction_price, price_per_sqm,
    log_price_per_sqm, normalized_employment, 
    total_employment, total_tourists, religious_dummy, total_population,
    mega_project_dummy, tourism_religious_interaction, transaction_volume,
    population_growth, annual_avg_salary, expenditure, households, affordable_price,
    affordability_ratio, area_sqm
  ) %>%
  mutate(
    Treated = ifelse(region == "Makkah", 1, 0)  # Mark treated region
  )


# Step 2: Summarize data
scm_data <- scm_data %>%
  group_by(region, year) %>%
  summarise(
    avg_transaction_price = mean(transaction_price, na.rm = TRUE),
    avg_price_per_sqm = mean(price_per_sqm, na.rm = TRUE),
    avg_area_sqm = mean(area_sqm, na.rm = TRUE),
    log_price_per_sqm = mean(log_price_per_sqm, na.rm = TRUE),
    avg_total_employment = first(total_employment),
    normalized_employment = first(normalized_employment),
    total_tourists = first(total_tourists),
    avg_religious_dummy = first(religious_dummy),
    avg_mega_project_dummy = first(mega_project_dummy),
    avg_tourism_religious_interaction = first(tourism_religious_interaction),
    avg_total_population = first(total_population),
    transaction_volume = first(transaction_volume),
    population_growth = first(population_growth),
    avg_annual_salary = first(annual_avg_salary),
    avg_households = first(households),
    avg_affordable_price = first(affordable_price),
    avg_affordability_ratio = first(affordability_ratio),
    avg_expenditure = first(expenditure),
    Treated = first(Treated),
    .groups = "drop"
  )

str(scm_data)

# Step 3: Balance Data
scm_data_balanced <- scm_data %>%
  complete(
    region,
    year = seq(min(year), max(year), by = 1),
    fill = list(
      avg_transaction_price = NA,
      avg_price_per_sqm = NA,
      area_sqm = NA,
      log_price_per_sqm = NA,
      avg_total_employment = NA,
      normalized_employment = NA,
      total_tourists = NA,
      avg_religious_dummy = 0,
      avg_mega_project_dummy = 0,
      avg_tourism_religious_interaction = 0,
      avg_total_population = NA,
      transaction_volume = NA,
      population_growth = NA,
      avg_annual_salary = NA,
      avg_households = NA,
      avg_affordability_ratio = NA,
      avg_affordable_price = NA,
      avg_expenditure = NA,
      Treated = 0
    )
  ) %>%
  arrange(region, year)

# Step 4: Assign unique numeric identifiers to regions
scm_data_balanced <- scm_data_balanced %>%
  mutate(region_id = as.numeric(factor(region)))

# Adjusting for error
write.csv(scm_data_balanced, "SCM Balanced with New Predictors & Matched.csv", row.names = FALSE)
scm_data_balanced <- read.csv("SCM Balanced with New Predictors & Matched.csv")

# Step 5: Prepare data for SCM using dataprep - TRUE PRICE
dataprep.out.true <- dataprep(
  foo = scm_data_balanced,  # Balanced dataset
  predictors = c(
    "normalized_employment",
    "total_tourists",
    "avg_tourism_religious_interaction",
    "avg_total_population",
    "population_growth",
    "avg_expenditure",
    "avg_households",
    "avg_affordability_ratio"
  ),  # Full predictor set
  predictors.op = "mean",  # Aggregation operation
  dependent = "avg_price_per_sqm",  # Dependent variable
  unit.variable = "region_id",  # Numeric region ID
  unit.names.variable = "region",  # Region names
  time.variable = "year",  # Time dimension
  treatment.identifier = scm_data_balanced %>% filter(Treated == 1) %>% pull(region_id) %>% unique(),
  controls.identifier = scm_data_balanced %>% filter(Treated == 0) %>% pull(region_id) %>% unique(),
  time.predictors.prior = seq(2017, 2020),  # Pre-treatment years
  time.optimize.ssr = seq(2017, 2020),  # Optimization window
  time.plot = seq(2017, 2022)  # Full plot range
)

# Run Synthetic Control Method
synth_results_true <- synth(dataprep.out.true)

# Summary of results
synth_summary_true <- summary(synth_results_true)

# Plotting the Results
synth_plot_true <- path.plot(
  synth.res = synth_results_true, 
  dataprep.res = dataprep.out.true, 
  Ylab = "Average Price per Sqm", 
  Xlab = "Year",
  Legend = c("Makkah (Treated)", "Synthetic Makkah"),
  Legend.position = "bottomright"
)

#---Gaps Plot---#
# Extract actual vs synthetic figures
actual_treated_true <- dataprep.out.true$Y1plot
synthetic_control_true <- dataprep.out.true$Y0plot %*% synth_results_true$solution.w

# Calculate the gaps (differences)
gaps_true <- actual_treated_true - synthetic_control_true

# Generate a Gaps Plot
gaps_plot_true <- gaps.plot(
  synth.res = synth_results_true,
  dataprep.res = dataprep.out.true,
  Ylab = "Gap in Average Price per Sqm",
  Xlab = "Year",
  Main = "Gaps Plot: Difference Between Treated and Synthetic Control"
)

#---LOG PRICES---#

# Step 5: Prepare data for SCM using dataprep - LOG PRICE
dataprep.out.log <- dataprep(
  foo = scm_data_balanced,  # Balanced dataset
  predictors = c(
    "normalized_employment",
    "total_tourists",
    "avg_tourism_religious_interaction",
    "avg_total_population",
    "population_growth",
    "avg_expenditure",
    "avg_households",
    "avg_affordability_ratio"
  ),  # Full predictor set
  predictors.op = "mean",  # Aggregation operation
  dependent = "log_price_per_sqm",  # Dependent variable
  unit.variable = "region_id",  # Numeric region ID
  unit.names.variable = "region",  # Region names
  time.variable = "year",  # Time dimension
  treatment.identifier = scm_data_balanced %>% filter(Treated == 1) %>% pull(region_id) %>% unique(),
  controls.identifier = scm_data_balanced %>% filter(Treated == 0) %>% pull(region_id) %>% unique(),
  time.predictors.prior = seq(2017, 2020),  # Pre-treatment years
  time.optimize.ssr = seq(2017, 2020),  # Optimization window
  time.plot = seq(2017, 2022)  # Full plot range
)

# Run Synthetic Control Method
synth_results_log <- synth(dataprep.out.log)

# Summary of results
synth_summary_log <- summary(synth_results_log)

# Plotting the Results
synth_plot_log <- path.plot(
  synth.res = synth_results_log, 
  dataprep.res = dataprep.out.log, 
  Ylab = "Log Price per Sqm", 
  Xlab = "Year",
  Legend = c("Makkah (Treated)", "Synthetic Makkah"),
  Legend.position = "bottomright"
)

#---Gaps Plot---#
# Extract actual vs synthetic figures
actual_treated_log <- dataprep.out.log$Y1plot
synthetic_control_log <- dataprep.out.log$Y0plot %*% synth_results_log$solution.w

# Calculate the gaps (differences)
gaps_log <- actual_treated_log - synthetic_control_log

# Generate a Gaps Plot
gaps_plot_log <- gaps.plot(
  synth.res = synth_results_log,
  dataprep.res = dataprep.out.log,
  Ylab = "Gap in Log Price per Sqm",
  Xlab = "Year",
  Main = "Gaps Plot: Difference Between Treated and Synthetic Control"
)




#---VISUALIZATIONS---#
#--The following is made for tabulations and figures used throughout the study--#
# Unit weights
round(synth_results_true$solution.w, 3)

# Region names
dataprep.out.true$unit.names

# Pre- vs post-treatment MSPE
summary(synth_results_true)

# Actual vs synthetic for each year
actual_treated_true <- dataprep.out.true$Y1plot
synthetic_values_true <- dataprep.out.true$Y0plot %*% 
  synth_results_true$solution.w
gaps_table <- actual_treated_true - synthetic_values_true
data.frame(Year = as.numeric(rownames(actual_treated_true)), 
           Actual = actual_treated_true, Synthetic = synthetic_values_true, Gap = gaps_table)

# FOR LOG #
# Region names
dataprep.out.log$unit.names

# Pre- vs post-treatment MSPE
summary(synth_results_log)

# Actual vs synthetic for each year
actual_treated_log <- dataprep.out.log$Y1plot
synthetic_values_log <- dataprep.out.log$Y0plot %*% 
  synth_results_log$solution.w
gaps_table_log <- actual_treated_log - synthetic_values_log
data.frame(Year = as.numeric(rownames(actual_treated_log)), 
           Actual = actual_treated_log, Synthetic = synthetic_values_log, Gap = gaps_table_log)

#--- SCM Combined Summary Table ---#

# Create summary table
scm_summary_df <- data.frame(
  Metric = c(
    "Treatment Year",
    "Treated Unit",
    "Top Control Regions (Weights)",
    "Actual (2022) – True Price",
    "Synthetic (2022) – True Price",
    "Treatment Effect (2022) – True Price",
    "Actual (2022) – Log Price",
    "Synthetic (2022) – Log Price",
    "Treatment Effect (2022) – Log Price",
    "Pre-Treatment Gap Range (2017–2020)",
    "2021 Gap – True Price",
    "2021 Gap – Log Price"
  ),
  Value = c(
    "2021",
    "Makkah (ID: 7)",
    "Region 6 (0.688), Region 10 (0.312)",
    round(actual_treated_true["2022", 1], 2),
    round(synthetic_values_true["2022", 1], 2),
    round(gaps_table["2022", 1], 2),
    round(actual_treated_log["2022", 1], 4),
    round(synthetic_values_log["2022", 1], 4),
    round(gaps_table_log["2022", 1], 4),
    "< ±50 (SAR) / < ±0.03 (log)",
    round(gaps_table["2021", 1], 2),
    round(gaps_table_log["2021", 1], 4)
  )
)

# View the table in R
print(scm_summary_df)

# Save to CSV
write.csv(scm_summary_df, "scm_summary_table.csv", row.names = FALSE)


# Extract unique Region ID–Name pairs
region_mapping <- scm_data_balanced[, c("region_id", "region")]
region_mapping <- unique(region_mapping)

# Create a named vector for mapping
region_names <- setNames(region_mapping$region, region_mapping$region_id)

# Extract Variable Weights
synth_results_true$solution.v

# Transpose so variable names become row names
variable_weights <- as.data.frame(t(synth_results_true$solution.v))

# Create clean dataframe
var_weight_df <- data.frame(
  Variable = rownames(variable_weights),
  Weight = round(variable_weights[, 1], 3)
)

# Reset row names for clarity
rownames(var_weight_df) <- NULL

# View clean variable weights
print(var_weight_df)


# Extract weights
unit_weights <- round(synth_results_true$solution.w, 3)

# Convert to a data frame, preserving IDs
weights_df <- data.frame(
  Region_ID = as.integer(rownames(unit_weights)),
  Weight = unit_weights[, 1]
)

# Add region names using your existing mapping
weights_df$Region_Name <- region_names[as.character(weights_df$Region_ID)]

# Reorder columns for clarity
weights_df <- weights_df[, c("Region_ID", "Region_Name", "Weight")]

# Filter non-zero weights
weights_df <- weights_df[weights_df$Weight > 0, ]

# View final clean table
print(weights_df)

# Table of SCM Metrics
scm_metrics <- data.frame(
  Metric = c(
    "Treatment Year",
    "Treated Unit",
    "Top Control Regions (Weights)",
    "Actual (2022) – True Price",
    "Synthetic (2022) – True Price",
    "Treatment Effect (2022) – True Price",
    "Actual (2022) – Log Price",
    "Synthetic (2022) – Log Price",
    "Treatment Effect (2022) – Log Price",
    "Pre-Treatment Gap Range (2017–2020)",
    "2021 Gap – True Price",
    "2021 Gap – Log Price"
  ),
  Value = c(
    "2021",
    paste0(region_names["7"], " (ID: 7)"),
    paste0(weights_df$Region_Name, " (", weights_df$Weight, ")", collapse = ", "),
    round(actual_treated_true["2022", 1], 2),
    round(synthetic_values_true["2022", 1], 2),
    round(gaps_table["2022", 1], 2),
    round(actual_treated_log["2022", 1], 4),
    round(synthetic_values_log["2022", 1], 4),
    round(gaps_table_log["2022", 1], 4),
    "< ±50 (SAR) / < ±0.03 (log)",
    round(gaps_table["2021", 1], 2),
    round(gaps_table_log["2021", 1], 4)
  )
)

# Reformat your variable weights into same column style
predictor_weights_df <- data.frame(
  Metric = paste0(var_weight_df$Variable, " (weight)"),
  Value = var_weight_df$Weight
)

# Combine
scm_summary_df <- rbind(scm_metrics, predictor_weights_df)

print(scm_summary_df)


write.csv(scm_summary_df, "SCM_Summary_Table.csv", row.names = FALSE)


# Create Gaps Plot with custom formatting
gaps_plot_true <- gaps.plot(
  synth.res = synth_results_true,
  dataprep.res = dataprep.out.true,
  Ylab = "Gap in Average Price per Sqm (SAR)",
  Xlab = "Year",
  Main = "Gaps Plot: Difference Between Treated and Synthetic Control"
)

# Add custom vertical dashed intervention line at 2021
abline(v = 2021, lty = 2, col = "red", lwd = 2)

# Add rotated label for intervention
text(x = 2021.1, 
     y = max(dataprep.out.true$Y1plot - dataprep.out.true$Y0plot %*% synth_results_true$solution.w, na.rm = TRUE) * 0.95, 
     labels = "Intervention", 
     col = "red", pos = 4, cex = 0.8)

# Bold title
title(main = "Gaps Plot: Difference Between Treated and Synthetic Control", font.main = 2)

# Create Gaps Plot with custom formatting - LOGGED
gaps_plot_log <- gaps.plot(
  synth.res = synth_results_log,
  dataprep.res = dataprep.out.log,
  Ylab = "Gap in Logged Prices",
  Xlab = "Year",
  Main = "Log Gaps Plot: Difference Between Treated and Synthetic Control"
)

# Add custom vertical dashed intervention line at 2021
abline(v = 2021, lty = 2, col = "red", lwd = 2)

# Add rotated label for intervention
text(x = 2021.1, 
     y = max(dataprep.out.log$Y1plot - dataprep.out.log$Y0plot %*% synth_results_log$solution.w, na.rm = TRUE) * 0.95, 
     labels = "Intervention", 
     col = "red", pos = 4, cex = 0.8)

# Bold title
title(main = "Log Gaps Plot: Difference Between Treated and Synthetic Control", font.main = 2)


# Plotting the SCM
# Create the Synthetic Control Path Plot
synth_plot_true <- path.plot(
  synth.res = synth_results_true, 
  dataprep.res = dataprep.out.true, 
  Ylab = "Average Price per Sqm (SAR)", 
  Xlab = "Year",
  Main = "Synthetic Control Method",
  Legend = c("Makkah (Treated)", "Synthetic Makkah"),
  Legend.position = "bottomright"
)

# Add vertical red dashed intervention line at 2021
abline(v = 2021, lty = 2, col = "red", lwd = 2)

# Add "Intervention" label (horizontal)
text(x = 2021.1, 
     y = max(c(dataprep.out.true$Y1plot, dataprep.out.true$Y0plot %*% synth_results_true$solution.w), na.rm = TRUE) * 0.95, 
     labels = "Intervention", 
     col = "red", pos = 4, cex = 0.8)

# Bold title and axis labels
title(main = "Synthetic Control Method", font.main = 2)

# Create the Synthetic Control Path Plot - LOGGED
synth_plot_log <- path.plot(
  synth.res = synth_results_log, 
  dataprep.res = dataprep.out.log, 
  Ylab = "Logged Prices", 
  Xlab = "Year",
  Main = "Synthetic Control Method with Logged Prices",
  Legend = c("Makkah (Treated)", "Synthetic Makkah"),
  Legend.position = "bottomright"
)

# Add vertical red dashed intervention line at 2021
abline(v = 2021, lty = 2, col = "red", lwd = 2)

# Add "Intervention" label (horizontal)
text(x = 2021.1, 
     y = max(c(dataprep.out.log$Y1plot, dataprep.out.log$Y0plot %*% synth_results_log$solution.w), na.rm = TRUE) * 0.95, 
     labels = "Intervention", 
     col = "red", pos = 4, cex = 0.8)

# Bold title and axis labels
title(main = "Synthetic Control Method with Logged Prices", font.main = 2)





save.image(file = "Thesis Code 2.RData")