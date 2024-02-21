# Script to replicate Fig 4 of the Ban et al. paper

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

# Load the Fig 4 data
fig4_data <- read_csv(
  here('2_build', 'Fig4_data.csv'),
  show_col_types = FALSE
)

# ============================================================================ #
# Pre-plot wrangle ----
# ============================================================================ #

fig4_data <- fig4_data %>% 
  mutate(
    # Generate period cutoff
    period = factor(if_else(year <= 1959, 0, 1), labels = c("Pre-1959", "Post-1959"))
  ) 

# ============================================================================ #
# Plot ----
# ============================================================================ #

plot_fig4 <- fig4_data %>% 
  ggplot(
    aes(x = year, y = r_newspaperscom)
  ) +
  geom_smooth(aes(group = period), method = "lm") +
  geom_point(size = 4.5) +
  geom_vline(xintercept = 1959, color = "red", lty = "dashed") +
  geom_vline(xintercept = 1965, color = "red", lty = "dashed") +
  coord_cartesian(
    ylim = c(0.06, 0.16)
  ) +
  labs(
    x = "Year", 
    y = "Relative Coverage of MA Executive Council"
  ) +
  theme(
    axis.text = element_text(size = 14), 
    axis.title = element_text(size = 15)
  )

# ============================================================================ #
# Export plot ----
# ============================================================================ #

ggsave(
  here('3_docs', 'fig', 'replicated', 'Fig4.png'),
  plot_fig4,
  width = 8,
  height = 5.5,
  units = 'in', 
  dpi = 500
)


