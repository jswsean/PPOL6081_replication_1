# Script to build data for the paper's Figure 1

# ============================================================================ #
# Preliminaries ----
# ============================================================================ #

# Load the required packages
library(here)
library(dplyr)
library(readstata13)
library(tidyr)
library(readr)
library(stringr)

# ============================================================================ #
# Load data ----
# ============================================================================ #

# Load the house committee hits data
hits_housecommittees <- read.dta13(
  here('1_raw', 'hits_housecommittees.dta')
)

# Load groseclose-stewart data 
groseclose_stewart <- read_csv(
  here('1_raw', 'groseclose_stewart_81st_93rd.csv'), 
  show_col_types = FALSE
)

# ============================================================================ #
# Wrangle ----
# ============================================================================ #

# House committee hits data ----
hits_house_committees <- hits_housecommittees %>% 
  filter(year >= 1949 & year <= 1973) %>% 
  # Summarise select terms
  summarise(
    across(
      c(agriculture, appropriations, armed_services, banking, 
        education, energy, foreign_affairs, government_operations, 
        house_administration, judiciary, merchant_marine, natural_resources, 
        post_office, public_works, rules, science, standards_of_official_conduct, 
        veterans_affairs, ways_and_means), 
      ~ sum(.x)
    )
  ) %>% 
  # Generate rowtotal
  mutate(
    total_hits = rowSums(across(everything()))
  ) %>% 
  # Reshape to long format
  pivot_longer(
    cols = c(agriculture:ways_and_means), 
    names_to = "committee", values_to = "hits"
  ) %>% 
  mutate(
    # Generate relative hits measure
    rel_hits = hits / total_hits
  ) %>% 
  # Sort committees
  arrange(-rel_hits) %>% 
  mutate(
    # Generate committee ranks
    rank = row_number(), 
    
    # Modify committee names
    committee = str_to_upper(str_replace_all(committee, "_", " ")), 
    
    # Replace select committee names 
    committee = case_when(
      committee == "ENERGY" ~ "ENERGY AND COMMERCE", 
      committee == "EDUCATION" ~ "EDUCATION AND LABOR",
      .default = committee
    )
  ) %>% 
  # Keep only relevant cols 
  select(committee, rank) %>% 
  # Sort committee 
  arrange(committee)


# Groseclose stewart 81-93rd data  ----
groseclose_stewart <- groseclose_stewart %>% 
  mutate(
    committee = str_to_upper(committee)
  ) %>% 
  rename(gs_rank = rank)

# Merge two dataframes ----
merged <- hits_house_committees %>% 
  inner_join(
    groseclose_stewart, 
    by = "committee"
  )


# ============================================================================ #
# Export ----
# ============================================================================ #

write_csv(
  merged,
  here('2_build', 'Fig1_data.csv')
)


