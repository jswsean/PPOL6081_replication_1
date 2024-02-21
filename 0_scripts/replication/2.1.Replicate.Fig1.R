# Script to replicate Fig 1 of the Ban et al. paper

# ============================================================================ #
# Preliminaries ----
# ============================================================================ #

# Load the required packages
library(here)
library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)

# Set default theme
default_theme <- theme_minimal()
theme_set(default_theme)

# ============================================================================ #
# Data load ----
# ============================================================================ #

# Load the built Fig1 data
fig1_data <- read_csv(
  here('2_build', 'Fig1_data.csv'),
  show_col_types = FALSE
)


# ============================================================================ #
# Plot ----
# ============================================================================ #

# Plot commands, adapted from the `make_final_graphs.R` function in the 1_raw folder
plot_fig1 <- fig1_data %>% 
  ggplot(aes(x = gs_rank, y = rank)) +
  geom_point() +
  geom_text(aes(label = committee), size = 5.5, nudge_y = .5) +
  geom_smooth(method = "lm") +
  stat_cor(
    aes(label = after_stat(r.label)),
    method = "pearson",
    size = 5.5
  ) +
  coord_cartesian(xlim = c(23, -2), ylim = c(20, 0)) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20)) +
  scale_x_reverse(breaks = c(1, 5, 10, 15, 20)) +
  labs(
    x = "Groseclose-Stewart Ranking",
    y = "Coverage-based Ranking"
  ) +
  theme(
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14)
  )

# ============================================================================ #
# Export plot ----
# ============================================================================ #

# Write plot 
ggsave(
  here('3_docs', 'fig', 'replicated', 'Fig1.png'),
  plot_fig1,
  width = 8, 
  height = 8, 
  units = "in",
  dpi = 500
)
