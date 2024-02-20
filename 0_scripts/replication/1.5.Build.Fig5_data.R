# Script to build data for the paper's Figure 5

# ============================================================================ #
# Preliminaries ----
# ============================================================================ #

# Load the required packages
library(here)
library(dplyr)
library(readstata13)
library(readr)

# ============================================================================ #
# Data load ----
# ============================================================================ #

# Load the candidate hits data
hits_candidates <- read.dta13(
  here('1_raw', 'hits_candidates.dta')
)

# Load the hits_partycommittee data 
hits_party_committee <- read.dta13(
  here('1_raw', 'hits_partycommittee.dta')
)

# Load Mayhew's TPO scores
mayhew_TPO <- read.dta13(
  here('1_raw', 'Mayhew_TPO_Scores.dta')
)

# ============================================================================ #
# Wrangle ----
# ============================================================================ #

# Merge the two data with hits_candidates as the base frame
hits_candidates <- hits_candidates %>% 
  inner_join(
    hits_party_committee, 
    by = c("state", "year")
  ) %>% 
  filter(!(state %in% c("DC", "V", "Victoria", "NSW", "New South Wales"))) %>% 
  # Filter out states before they were admitted to the union
  filter(!(state == "ND" & year < 1889)) %>% 
  filter(!(state == "SD" & year < 1889)) %>% 
  filter(!(state == "MT" & year < 1889)) %>% 
  filter(!(state == "WA" & year < 1888)) %>% 
  filter(!(state == "ID" & year < 1900)) %>% 
  filter(!(state == "WY" & year < 1900)) %>% 
  filter(!(state == "UT" & year < 1896)) %>% 
  filter(!(state == "OK" & year < 1907)) %>% 
  filter(!(state == "AZ" & year < 1912)) %>% 
  filter(!(state == "NM" & year < 1912)) %>% 
  filter(!(state == "AK" & year < 1958)) %>% 
  filter(!(state == "HI" & year < 1958)) %>% 
  # Merge with Mayhew's TPO scores
  left_join(
    mayhew_TPO, 
    by = "state"
  ) %>% 
  mutate(
    # Generate N of oth candidates
    n_othr = democ_othr + repub_othr, 
    # Party power
    party_power = if_else(candidate_hits > 500, 100 * n_othr / (n_othr + candidate_hits), NA_real_)
  ) %>% 
  # State average of party_power
  mutate(
    x = mean(party_power, na.rm = TRUE),
    party_power_norm = party_power/x,
    .by = state
  ) %>% 
  # Keep only relevant variables
  select(state, year, party_power_norm)

# ============================================================================ #
# Export the result ----
# ============================================================================ #

write_csv(
  hits_candidates, 
  here('2_build', 'Fig5_data.csv')
)


