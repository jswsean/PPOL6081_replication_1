# Script to replicate Fig 2 of the Ban et al. paper

# ============================================================================ #
# Preliminaries ----
# ============================================================================ #

# Load the required packages
library(here)
library(dplyr)
library(readr)
library(ggplot2)

# Set default theme
default_theme <- theme_classic()
theme_set(default_theme)

# ============================================================================ #
# Data load ----
# ============================================================================ #

# Load the built Fig2 data
fig2_data <- read_csv(
  here('2_build', 'Fig2_data.csv'),
  show_col_types = FALSE
)

# ============================================================================ #
# Plot the figure ----
# ============================================================================ #

# Plot Fig 2
plot_fig2 <- fig2_data %>% 
  ggplot(aes( x = factor(period, labels = c("Before", "During Leadership", "After")), 
              y = hits, 
              group = row_id )) +
  geom_line(color = "gray") +
  geom_line(
    data = (fig2_data %>% summarise(hits = mean(hits), .by = period) %>% 
              mutate(row_id = nrow(fig2_data) + 1)), 
    color = "black", 
    size = 1.5
  ) +
  labs(
    x = "", 
    y = "Newspaper Mentions"
  ) +
  coord_cartesian(ylim = c(0, 2500), xlim = c(1.5,2.5))

# ============================================================================ #
# Export plot ----
# ============================================================================ #

# Write plot 
ggsave(
  here('3_docs', 'fig', 'replicated', 'Fig2.png'),
  plot_fig2,
  width = 8, 
  height = 6.5, 
  units = "in",
  dpi = 1000
)
