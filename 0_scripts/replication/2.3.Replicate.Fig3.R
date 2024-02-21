# Script to replicate Fig 3 of the Ban et al. paper

# ============================================================================ #
# Preliminaries ----
# ============================================================================ #

# Load the required packages
library(here)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(ggpubr)

# Set default theme
default_theme <- theme_minimal()
theme_set(default_theme)

# ============================================================================ #
# Load data ----
# ============================================================================ #

# Load Fig 3 data
fig3_data <- read_csv(
  here('2_build', 'Fig3_data.csv'),
  show_col_types = FALSE
)


# ============================================================================ #
# Pre-plot wrangle ----
# ============================================================================ #

# Pre-processed city gov hits data
fig3_data <- fig3_data %>% 
  mutate(
    # Indicator for whether year >= first_year
    city_manager_govt = if_else(year < first_year | is.na(first_year), FALSE, TRUE),
    
    # Duration indicator
    t = year - first_year + .5
  ) 
  
# Get control sums 
control_sums <- fig3_data %>% 
  filter(city_manager_govt == FALSE) %>% 
  # Collapse at the year level
  summarise(
    
    across(
      c(mayor, city_manager, city_council), 
      ~ sum(.x), 
      .names = "control_sum_{.col}"
    ),
    .by = year
  )

# Fig3 data
fig3_data <- fig3_data %>% 
  # Get control sums
  left_join(
    control_sums, 
    by = "year"
  ) %>% 
  # Drop missing state, if any
  filter(!is.na(state), state != "") %>%
  # Summarise count at the city state t level
  summarise(
    across(
      c(mayor, city_manager, city_council, mayor_x, city_manager_x, city_council_x, 
        control_sum_mayor, control_sum_city_manager, control_sum_city_council), 
      ~ sum(.x, na.rm = TRUE)
    ),
    .by = c(state, city, t)
  ) %>% 
  # Generate rowtotal 
  mutate(
    total = mayor + city_manager + city_council, 
    total_x = mayor_x + city_manager_x + city_council_x, 
    total_control = control_sum_mayor + control_sum_city_manager + control_sum_city_council
  ) %>%
  # Keep t inrange [-20,20]
  filter(t >= -20, t <= 20) %>% 
  # Generate proportion of total counts  
  mutate(
    # Mayor, city_manager, city_council
    across(
      c(mayor, city_manager, city_council), 
      ~ if_else(total >= 50, .x/total, NA_real_), 
      .names = "r_{.col}"
    ), 
    
    # Mayor_x, city_manager_x, city_council_x
    across(
      c(mayor_x, city_manager_x, city_council_x), 
      ~ if_else(total_x >= 10, .x/total_x, NA_real_), 
      .names = "r_{.col}"
    ), 
    
    # Same variables but for control 
    across(
      c(control_sum_mayor, control_sum_city_manager, control_sum_city_council), 
      ~ if_else(total_control >= 50, .x/total_control, NA_real_),
      .names = "r_{.col}"
    )
  ) %>%
  mutate(
    # Generate relative proportions
    rel_mayor_council_total = r_city_council + r_mayor,
    rel_mayor_council_control_total = r_control_sum_city_council + r_control_sum_mayor,
    rel_mayor_council = r_mayor / rel_mayor_council_total,
    rel_mayor_council_control = r_control_sum_mayor / rel_mayor_council_control_total, 
    
    # In the original paper, r_mayor was used instead of r_mayor_x
    rel_mayor_council_total_x = r_city_council_x + r_mayor_x, 
    rel_mayor_council_x = r_mayor_x / rel_mayor_council_total_x
  ) %>%
  # Get average of the proportion variables by t/-t
  summarise(
    
    # Get the collapsed average
    across(
      c(r_mayor, r_city_manager, r_city_council, 
        r_mayor_x, r_city_manager_x, r_city_council_x, 
        r_control_sum_mayor, r_control_sum_city_manager, r_control_sum_city_council, 
        rel_mayor_council, rel_mayor_council_control, rel_mayor_council_x), 
      ~ mean(.x, na.rm = TRUE), 
      .names = "{.col}_avg"
    ), 
    
    # Get the collapsed std error 
    across(
      c(r_mayor, r_city_manager, r_city_council, 
        r_mayor_x, r_city_manager_x, r_city_council_x, 
        r_control_sum_mayor, r_control_sum_city_manager, r_control_sum_city_council, 
        rel_mayor_council, rel_mayor_council_control, rel_mayor_council_x), 
      ~ plotrix::std.error(.x, na.rm = TRUE), 
      .names = "{.col}_sd"
    ), 
    .by = t
  ) %>% 
  arrange(t)



# ============================================================================ #
# Plot ----
# ============================================================================ #

# Plot A - Mayors
plot_fig3a <- fig3_data %>% 
  select(t, r_mayor_avg, r_control_sum_mayor_avg, r_mayor_sd, r_control_sum_mayor_sd) %>% 
  pivot_longer(  
    cols = c(r_mayor_avg, r_control_sum_mayor_avg, r_mayor_sd, r_control_sum_mayor_sd), 
    names_to = c("group", "stat"), 
    names_pattern = "^r_(.*)_(.*)$"
  ) %>% 
  pivot_wider( 
    id_cols = c(t, group), names_from = stat, values_from = value
  ) %>% 
  mutate(
    group = if_else(group == "mayor", "Reform cities", "Control cities"), 
    upper = avg + (1.96*sd),
    lower = avg - (1.96*sd)
  ) %>% 
  ggplot(
    aes(x = t, y = avg, fill = group)
  ) +
  geom_vline(xintercept = 0, color = "red", lty = "dashed") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .3) +
  geom_point(aes(shape = group)) +
  annotate(
    "label", x = 0, y = .8, label = "Relative coverage of mayors", size = 4.5 
  ) +
  coord_cartesian(ylim = c(.3, .8)) +
  labs(
    x = "", y = ""
  ) +
  guides(
    fill = guide_legend(title = "Group"),
    shape = "none"
  ) +
  theme(legend.position = c(.3, .175), 
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7))
  


# Plot B - City managers
plot_fig3b <- fig3_data %>% 
  select(t, r_city_manager_avg, r_control_sum_city_manager_avg, r_city_manager_sd, r_control_sum_city_manager_sd) %>% 
  pivot_longer(  
    cols = c(r_city_manager_avg, r_control_sum_city_manager_avg, r_city_manager_sd, r_control_sum_city_manager_sd), 
    names_to = c("group", "stat"), 
    names_pattern = "^r_(.*)_(.*)$"
  ) %>% 
  pivot_wider( 
    id_cols = c(t, group), names_from = stat, values_from = value
  ) %>% 
  mutate(
    group = if_else(group == "city_manager", "Reform cities", "Control cities"), 
    upper = avg + (1.96*sd),
    lower = avg - (1.96*sd)
  ) %>% 
  ggplot(
    aes(x = t, y = avg, fill = group)
  ) +
  geom_vline(xintercept = 0, color = "red", lty = "dashed") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .3) +
  geom_point(aes(shape = group)) +
  annotate(
    "label", x = 0, y = .4, label = "Relative coverage of city managers", size = 4.5 
  ) +
  coord_cartesian(ylim = c(0, .4)) +
  labs(
    x = "", y = ""
  ) +
  guides(
    fill = guide_legend(title = "Group"),
    shape = "none"
  ) +
  theme(
    legend.position = "none"
  )



# Plot C - City councils
plot_fig3c <- fig3_data %>%
  select(t, r_city_council_avg, r_control_sum_city_council_avg, r_city_council_sd, r_control_sum_city_council_sd) %>% 
  pivot_longer(  
    cols = c(r_city_council_avg, r_control_sum_city_council_avg, r_city_council_sd, r_control_sum_city_council_sd), 
    names_to = c("group", "stat"), 
    names_pattern = "^r_(.*)_(.*)$"
  ) %>% 
  pivot_wider( 
    id_cols = c(t, group), names_from = stat, values_from = value
  ) %>% 
  mutate(
    group = if_else(group == "city_council", "Reform cities", "Control cities"), 
    upper = avg + (1.96*sd),
    lower = avg - (1.96*sd)
  ) %>% 
  ggplot(
    aes(x = t, y = avg, fill = group)
  ) +
  geom_vline(xintercept = 0, color = "red", lty = "dashed") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .3) +
  geom_point(aes(shape = group)) +
  annotate(
    "label", x = 0, y = .5, label = "Relative coverage of city council", size = 4.5 
  ) +
  coord_cartesian(ylim = c(0.1, 0.5)) +
  labs(
    x = "", y = ""
  ) +
  guides(
    fill = guide_legend(title = "Group"),
    shape = "none"
  ) + 
  theme(legend.position = "none")



# Plot D - Mayor, relative to city council
plot_fig3d <- fig3_data %>%
  select(t, rel_mayor_council_avg, rel_mayor_council_control_avg, rel_mayor_council_sd, rel_mayor_council_control_sd) %>% 
  pivot_longer(  
    cols = c(rel_mayor_council_avg, rel_mayor_council_control_avg, rel_mayor_council_sd, rel_mayor_council_control_sd), 
    names_to = c("group", "stat"), 
    names_pattern = "^rel_(.*)_(.*)$"
  ) %>% 
  pivot_wider( 
    id_cols = c(t, group), names_from = stat, values_from = value
  ) %>% 
  mutate(
    group = if_else(group == "mayor_council", "Reform cities", "Control cities"), 
    upper = avg + (1.96*sd),
    lower = avg - (1.96*sd)
  ) %>% 
  ggplot(
    aes(x = t, y = avg, fill = group)
  ) +
  geom_vline(xintercept = 0, color = "red", lty = "dashed") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .3) +
  geom_point(aes(shape = group)) +
  annotate(
    "label", x = 0, y = .9, label = "Coverage of mayor relative to city council", size = 4.5 
  ) +
  coord_cartesian(ylim = c(0.4, 0.9)) +
  labs(
    x = "", y = ""
  ) +
  guides(
    fill = guide_legend(title = "Group"),
    shape = "none"
  ) + 
  theme(legend.position = "none")



# Combine in a single plot of multiple panels
plot_fig3 <- ggarrange(
  plot_fig3a, plot_fig3b, 
  plot_fig3c, plot_fig3d
)


# ============================================================================ #
# Export plot ----
# ============================================================================ #

ggsave(
  here('3_docs', 'fig', 'replicated', 'Fig3.png'),
  plot_fig3, 
  width = 8,
  height = 6,
  units = 'in',
  dpi = 500
)


