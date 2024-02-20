# Script to build the paper's Table 1

# ============================================================================ #
# Preliminary ----
# ============================================================================ #

# Load required libraries
library(here)
library(dplyr)
library(readr)
library(fixest)
library(kableExtra)

# ============================================================================ #
# Data load ----
# ============================================================================ #

# Load Fig 3 data
fig3_data <- read_csv(
  here('2_build', 'Fig3_data.csv'),
  show_col_types = FALSE
)


# ============================================================================ #
# Pre-analysis wrangle ----
# ============================================================================ #

fig3_data <- fig3_data %>% 
  # Generate row totals
  mutate(
    total = mayor + city_manager + city_council,
    total_x = mayor_x + city_manager_x + city_council_x
  ) %>% 
  # Generate relative props 
  mutate(
    across(
      c(mayor, city_manager, city_council), 
      ~ if_else(total >= 50, .x / total, NA_real_),
      .names = "r_{.col}"
    ),
    across(
      c(mayor_x, city_manager_x, city_council_x), 
      ~ if_else(total_x >= 10, .x / total_x, NA_real_), 
      .names = "r_{.col}"
    )
  ) %>% 
  # Generate indicators 
  mutate(
    # Indicator for whether year >= first_year
    city_manager_govt = if_else(year < first_year | is.na(first_year), FALSE, TRUE),
  ) %>% 
  # Generate state city count of rmayor
  mutate(
    x = sum(!is.na(r_mayor)),
    .by = c(state, city)
  ) %>%
  # Filter for state cities with count more/equal to 10
  filter(x >= 10) %>% 
  # Filter for good case
  filter(
    (good_case == "***** currently council-manager" & !is.na(first_year)) | 
      (good_case == "***** currently mayor-council")
  ) %>% 
  # Keep years above/equal to 1890
  filter(year >= 1890) %>% 
  # Generate state_city variable 
  mutate(
    state_city = stringr::str_c(state, " ", city)
  )


# ============================================================================ #
# Regressions ----
# ============================================================================ #

# Model 1:
model_1 = feols(
  r_mayor ~ city_manager_govt | year + state_city,
  data = fig3_data,
  vcov = ~state_city
)

# Model 2: 
model_2 = feols(
  r_city_manager ~ city_manager_govt | year + state_city,
  data = fig3_data,
  vcov = ~state_city
)

# Model 3:
model_3 = feols(
  r_mayor_x ~ city_manager_govt | year + state_city,
  data = fig3_data,
  vcov = ~state_city
)

# Model 4:
model_4 = feols(
  r_city_manager_x ~ city_manager_govt | year + state_city,
  data = fig3_data,
  vcov = ~state_city
)

# Set dictionary for variable export
var_dict = c("city_manager_govtTRUE" = "Council-manager government form",
             "year" = "Year", 
             "state_city" = "City",
             "r_mayor" = "Rel. Coverage of Mayor", 
             "r_city_manager" = "Rel. Coverage of City Manager", 
             "r_mayor_x" = "Rel. Coverage of Mayor", 
             "r_city_manager_x" = "Rel. Coverage of City Manager")

# Summarizing the table 
tab1 <- etable(
  model_1, model_2, model_3, model_4, 
  headers = list(":_:" = list("All Mentions" = 2, "Using City Name Filter" = 2)),
  tex = TRUE, 
  digits = 2,
  digits.stats = 2,
  drop = "Const",
  dict = var_dict,
  style.tex = style.tex("aer"),
  se.below = TRUE
)

# ============================================================================ #
# Export the table ----
# ============================================================================ #

writeLines(
  tab1, 
  con = here('3_docs', 'tab', 'replicated', 'Tab1.tex')
)


