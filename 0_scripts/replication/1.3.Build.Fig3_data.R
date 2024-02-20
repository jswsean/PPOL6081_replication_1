# Script to build data for the paper's Figure 3 and Table 1

# ============================================================================ #
# Preliminaries ----
# ============================================================================ #

# Load the required packages
library(here)
library(dplyr)
library(readstata13)
library(readr)
library(tidyr)
library(stringr)

# ============================================================================ #
# Load data ----
# ============================================================================ #

# Load the citygov data
hits_city_gov <- read.dta13(
  here('1_raw', 'hits_citygov.dta')
)

# Load the city manager dates data
city_manager_dates <- read_delim(
  here('1_raw', 'city_manager_dates.txt')
)


# ============================================================================ #
# Wrangle ----
# ============================================================================ #

# Citygov data
hits_city_gov <- hits_city_gov %>%
  mutate(
    # Modify city names
    city = str_to_upper(city),
    city = if_else(state == "CA" & str_detect(city, "SAN BERNARDIN"), "SAN BERNARDINO", city), 
    city = if_else(state == "DE" & city == "WILMING", "WILMINGTON", city), 
    city = if_else(state == "LA" & str_detect(city, "NATCHITOCH"), "NATCHITOCHES", city), 
    city = if_else(city == "MT VERNON", "MOUNT VERNON", city)
  ) %>% 
  # Drop based on city and state columns 
  filter(
    !is.na(city), !is.na(state), 
    city != "", state != ""
  ) %>% 
  # Sum across rows by state city year
  summarise(
    across(
      c(mayor, city_manager, city_council, mayor_x, city_manager_x, city_council_x), 
      ~ sum(.x, na.rm = TRUE)
    ), 
    .by = c(state, city, year)
  ) %>% 
  # Arrange state, city, year
  arrange(state, city, year) %>% 
  # Filter out select states
  filter(
    !is.na(state), 
    !(state == "PA" & city == "OTTAWA"), 
    !(state == "TX" & city == "OTTAWA"), 
    !(city == "WINNIPEG"), 
    !(city == "VANCOUVER")
  ) %>% 
  # Sort based on state city
  arrange(state, city)


# Merge with city manager dates 
hits_city_gov <- hits_city_gov %>% 
  full_join(
    city_manager_dates, 
    by = c("state", "city")
  )

# Second round of wrangling
hits_city_gov <- hits_city_gov %>% 
  # Drop samples that are outside the year frame
  filter(year >= 1876 & year <= 1977) %>% 
  mutate(
    # Anomalous x values
    first_year = as.numeric(str_replace_all(first_year, "\\?", ""))
  )


# ============================================================================ #
# Export data ----
# ============================================================================ #

# Write fig 3 data
write_csv(
  hits_city_gov, 
  here('2_build', 'Fig3_data.csv')
)


