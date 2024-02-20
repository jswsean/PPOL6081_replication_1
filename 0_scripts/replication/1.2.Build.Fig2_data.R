# Script to build data for the paper's Figure 2

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

# Load the hits_speaker data
hits_speakers <- read.dta13(
  here('1_raw', 'hits_speakers.dta')
)

# ============================================================================ #
# Wrangle ----
# ============================================================================ #

# hits speakers data 
hits_speakers <- hits_speakers %>% 
  # Generate row identifier 
  mutate(
    row_id = row_number()
  ) %>% 
  pivot_longer(
    cols = c(hits1:hits3), 
    names_to = "period", 
    values_to = "hits"
  ) %>% 
  mutate(
    # Generate period sequence 
    period = as.numeric(str_replace_all(period, "hits", ""))
  )


# ============================================================================ #
# Export data ----
# ============================================================================ #

# Export the hits_speaker data 
write_csv(
  hits_speakers, 
  here('2_build', 'Fig2_data.csv')
)
