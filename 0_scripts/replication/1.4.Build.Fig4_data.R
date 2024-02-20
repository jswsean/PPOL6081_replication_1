# Script to build data for the paper's Figure 4

# ============================================================================ #
# Preliminaries ----
# ============================================================================ #

# Load the required packages
library(here)
library(dplyr)
library(readstata13)
library(readr)

# ============================================================================ #
# Load data ----
# ============================================================================ #

# Load hits gov council data
hits_gov_council <- read.dta13(
  here('1_raw', 'hits_govcouncil.dta')
)

# ============================================================================ #
# Wrangle ----
# ============================================================================ #

# Hits gov council data ----
hits_gov_council <- hits_gov_council %>% 
  na.omit() %>% 
  mutate(
    # Generate pre-post 1959 cutoff
    period = if_else(year < 1959, 0, 1)
  )

# ============================================================================ #
# Export ----
# ============================================================================ #

# Export sthe hits gov council data
write_csv( 
  hits_gov_council, 
  here('2_build', 'Fig4_data.csv')
)

