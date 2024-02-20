# Script to replicate Fig 5 of the Ban et al. paper

# ============================================================================ #
# Preliminaries ----
# ============================================================================ #

# Load the required packages
library(here)
library(dplyr)
library(readr)
library(ggplot2)
library(zoo)

# Set default theme
default_theme <- theme_classic()
theme_set(default_theme)

# ============================================================================ #
# Load data ----
# ============================================================================ #

# Fig 5 data
fig5_data <- read_csv(
  here('2_build', 'Fig5_data.csv'),
  show_col_types = FALSE
)

# ============================================================================ #
# Pre-plot wrangle ----
# ============================================================================ #

# Filter out to select states
select_states <- fig5_data %>% 
  filter(state %in% c("OH", "CA", "IL", "KS", "WI", "PA", "NC", "TX", "NY")) %>% 
  na.omit()

# Create rolling averages
select_states <- select_states %>% 
  mutate(
    state = factor(state, levels = c('OH', 'CA', 'IL', 
                                     'KS', 'WI', 'PA', 
                                     'NC', 'TX', 'NY')),
    party_power_ma = rollapply(party_power_norm, 2, mean, align="right", fill=NA),
    .by = state
  )

# ============================================================================ #
# Plot ----
# ============================================================================ #

# Plot Fig 5
plot_fig5 <- select_states %>% 
  ggplot(
    aes(x = year, y = party_power_ma) 
  ) +
  geom_line() +
  facet_wrap(~state, ncol = 3) +
  coord_cartesian(
    xlim = c(1880, 1980)
  ) +
  labs(
    x = "Year", 
    y = "Party Committee Power"
  )


# ============================================================================ #
# Export plot ----
# ============================================================================ #

# Save the plot.
ggsave(
  here('3_docs', 'fig', 'replicated', 'Fig5.png'),
  plot_fig5, 
  width = 8, 
  height = 8,
  units = 'in', 
  dpi = 500
)
