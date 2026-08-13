library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(viridis)

# read data
#birds <- read_csv("birds.comb.csv", show_col_types = FALSE)

# keep the fields we need and clean them
birds2 <- birds %>%
  transmute(
    survey_id = survey_id,
    habitat   = as.factor(habitat),
    species   = species,
    time      = as.numeric(time)
  ) %>%
  filter(!is.na(survey_id), !is.na(habitat), !is.na(species), !is.na(time))

# for species accumulation, only the FIRST time each species was detected
# in each survey matters
first_det <- birds2 %>%
  group_by(habitat, survey_id, species) %>%
  summarise(first_time = min(time), .groups = "drop")

# choose time points for the curve
# here: every 1 minute from 0 to max survey time
time_grid <- seq(0, ceiling(max(first_det$first_time)), by = 1)

# build cumulative richness curve for each survey
survey_curves <- first_det %>%
  group_by(habitat, survey_id) %>%
  summarise(first_times = list(first_time), .groups = "drop") %>%
  crossing(time_min = time_grid) %>%
  mutate(
    richness = map2_int(first_times, time_min, ~ sum(.x <= .y))
  )

# average across surveys within habitat
habitat_curves <- survey_curves %>%
  group_by(habitat, time_min) %>%
  summarise(
    mean_richness = mean(richness),
    sd_richness   = sd(richness),
    n_surveys     = n(),
    se_richness   = sd_richness / sqrt(n_surveys),
    .groups = "drop"
  )

# overall
overall_curves <- survey_curves %>%#
  group_by(time_min) %>%
  summarise(
    mean_richness = mean(richness),
    sd_richness   = sd(richness),
    n_surveys     = n(),
    se_richness   = sd_richness / sqrt(n_surveys),
    .groups = "drop"
  )

library(dplyr)
library(ggplot2)
library(patchwork)

# Shared y-axis label
ylab_text <- "Mean cumulative species richness of survey"

# Function to avoid repeating the same plotting code
make_sac_plot <- function(data, plot_title, xlab = NULL) {
  ggplot(data, aes(x = time_min, y = mean_richness)) +
    geom_line() +
    geom_ribbon(
      aes(
        ymin = pmax(mean_richness - se_richness, 0),
        ymax = mean_richness + se_richness
      ),
      alpha = 0.3
    ) +
    labs(
      x = xlab,
      y = ylab_text,
      title = plot_title
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(0, 4))
}

# Subset data
habitat_curves_1 <- filter(habitat_curves, habitat == 1)
habitat_curves_2 <- filter(habitat_curves, habitat == 2)
habitat_curves_3 <- filter(habitat_curves, habitat == 3)
habitat_curves_4 <- filter(habitat_curves, habitat == 4)
habitat_curves_5 <- filter(habitat_curves, habitat == 5)

# Create plots
p.sac.1 <- make_sac_plot(habitat_curves_1, "WS 1")
p.sac.2 <- make_sac_plot(habitat_curves_2, "WS 2")
p.sac.3 <- make_sac_plot(habitat_curves_3, "WS 3")
p.sac.4 <- make_sac_plot(habitat_curves_4, "WS 4")
p.sac.5 <- make_sac_plot(habitat_curves_5, "WS 5", xlab = "Time (minutes)")
p.sac.overall <- make_sac_plot(overall_curves, "Overall")

# Combine with one shared y-axis title
p.sac.all <- (p.sac.1 + p.sac.2 + p.sac.3 +
                p.sac.4 + p.sac.5 + p.sac.overall) +
  plot_layout(ncol = 3, axis_titles = "collect_y")

p.sac.all

ggsave("figs/sac.png", dpi = 700)

# plot
p.line <- ggplot(habitat_curves, aes(x = time_min, y = mean_richness)) +
  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  geom_line(aes(colour = habitat), linewidth = 1) +
  geom_ribbon(
    aes(
      ymin = pmax(mean_richness - se_richness, 0),
      ymax = mean_richness + se_richness,
      group = habitat
    ),
    alpha = 0.1
  ) +
  labs(
    x = "Time since start of survey (minutes)",
    y = "Mean cumulative species richness of survey",
    colour = "WS class", # rename to full titles of woodland structure classes
    fill = "WS class"
    #title = "Species accumulation curves by habitat"
  ) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #axis.ticks = element_line()
        ) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  coord_cartesian(ylim = c(0,4))
p.line

ggsave("figs/sac.png", dpi = 700)

# p.scatter <- 
#   ggplot(habitat_curves, aes(x = time_min, y = mean_richness)) + 
#   geom_point(aes(col = habitat)) +
#   geom_errorbar(aes(ymin = mean_richness - se_richness,
#                     ymax = mean_richness + se_richness,
#                     col = habitat))
# p.scatter
# 

##### graph per class ####
# habitat_curves_1 <- filter(habitat_curves, habitat == 1)
# head(habitat_curves_1)
# 
# p.sac.1 <- ggplot(habitat_curves_1, aes(x = time_min, y = mean_richness)) +
#   geom_line() + 
#   geom_ribbon(aes(ymin = pmax(mean_richness - se_richness, 0),
#                   ymax = (mean_richness + se_richness)),
#               alpha = 0.3
#               )  +
#   labs(x = NULL, y = "Mean cumulative species richness of survey",
#        title = "WS 1") +
#   theme_minimal(base_size = 16) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()
#         #axis.ticks = element_line()
#   ) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) + 
#   coord_cartesian(ylim = c(0,4))
# p.sac.1
# 
# habitat_curves_2 <- filter(habitat_curves, habitat == 2)
# 
# p.sac.2 <- ggplot(habitat_curves_2, aes(x = time_min, y = mean_richness)) +
#   geom_line() + 
#   geom_ribbon(aes(ymin = pmax(mean_richness - se_richness, 0),
#                   ymax = (mean_richness + se_richness)),
#               alpha = 0.3
#   ) +
#   labs(x = NULL, y = NULL,
#        title = "WS 2") +
#   theme_minimal(base_size = 16) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()
#         #axis.ticks = element_line()
#   ) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) + 
#   coord_cartesian(ylim = c(0,4))
# p.sac.2
# 
# habitat_curves_3 <- filter(habitat_curves, habitat == 3)
# 
# p.sac.3 <- ggplot(habitat_curves_3, aes(x = time_min, y = mean_richness)) +
#   geom_line() + 
#   geom_ribbon(aes(ymin = pmax(mean_richness - se_richness, 0),
#                   ymax = (mean_richness + se_richness)),
#               alpha = 0.3
#   )  +
#   labs(x = NULL, y = NULL,
#        title = "WS 3") +
#   theme_minimal(base_size = 16) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()
#         #axis.ticks = element_line()
#   ) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) + 
#   coord_cartesian(ylim = c(0,4))
# p.sac.3
# 
# habitat_curves_4 <- filter(habitat_curves, habitat == 4)
# 
# p.sac.4 <- ggplot(habitat_curves_4, aes(x = time_min, y = mean_richness)) +
#   geom_line() + 
#   geom_ribbon(aes(ymin = pmax(mean_richness - se_richness, 0),
#                   ymax = (mean_richness + se_richness)),
#               alpha = 0.3
#   )+
#   labs(x = NULL, y = "Mean cumulative species richness of survey",
#        title = "WS 4") +
#   theme_minimal(base_size = 16) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()
#         #axis.ticks = element_line()
#   ) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) + 
#   coord_cartesian(ylim = c(0,4))
# p.sac.4
# 
# habitat_curves_5 <- filter(habitat_curves, habitat == 5)
# 
# p.sac.5 <- ggplot(habitat_curves_5, aes(x = time_min, y = mean_richness)) +
#   geom_line() + 
#   geom_ribbon(aes(ymin = pmax(mean_richness - se_richness, 0),
#                   ymax = (mean_richness + se_richness)),
#               alpha = 0.3
#   )+
#   labs(x = "Time (Minutes)", y = NULL,
#        title = "WS 5") +
#   theme_minimal(base_size = 16) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()
#         #axis.ticks = element_line()
#   ) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) + 
#   coord_cartesian(ylim = c(0,4))
# p.sac.5
# 
# p.sac.overall <- ggplot(overall_curves, aes(x = time_min, y = mean_richness)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = pmax(mean_richness - se_richness, 0),
#                   ymax = (mean_richness + se_richness)),
#               alpha = 0.3
#   ) +
#   labs(x = NULL, y = NULL,
#        title = "Overall") +
#   theme_minimal(base_size = 16) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()
#         #axis.ticks = element_line()
#   ) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) + 
#   coord_cartesian(ylim = c(0,4))
# p.sac.overall
# 
# p.sac.all <- (p.sac.1 + p.sac.2 + p.sac.3 +
#               p.sac.4 + p.sac.5 + p.sac.overall)
# p.sac.all










