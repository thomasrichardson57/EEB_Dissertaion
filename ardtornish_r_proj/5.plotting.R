#----packages----
library(ggplot2)
library(ggeffects)
library(viridis)
library(tidyr)
library(dplyr)
library(patchwork)

#----Summary Plots----

sp.freq <- ggplot(birds, aes(x = forcats::fct_infreq(species))) +
  geom_bar(fill = 'salmon', colour = NULL) +
  labs(x = "Species", y = "Frequency") +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0,70)) + 
  scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  geom_hline(yintercept = 0)
sp.freq

ggsave("figs/sp.barplot.png", dpi = 700)

species_row_counts <- birds %>%
  count(species, name = "n_rows") %>%
  arrange(desc(n_rows))

species_row_counts


#### OFD ####

# overall
occ <- birds %>%
  distinct(survey_id, species) %>%      # avoids double-counting repeat records at a site
  count(species, name = "n_sites")

ofd <- occ %>%
  count(n_sites, name = "n_species")

ofd$p <- ofd$n_sites / max(ofd$n_sites)

p.ofd <- ggplot(ofd, aes(p, n_species)) + 
  geom_col(fill = 'salmon', col = 'black') + 
  labs(x = NULL,
       y = "Number of Species",
       title = 'Overall') + 
  theme_bw(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  coord_cartesian(ylim = c(0,20)) +
  scale_x_continuous(breaks = c(0,1))
p.ofd

# by hab:

occ.1 <- birds %>%
  filter(habitat == '1') %>%
  distinct(survey_id, species) %>%      # avoids double-counting repeat records at a site
  count(species, name = "n_sites")

ofd.1 <- occ.1 %>%
  count(n_sites, name = "n_species")

ofd.1$p <- ofd.1$n_sites / max(ofd.1$n_sites)

p.ofd.1 <- ggplot(ofd.1, aes(p, n_species)) + 
  geom_col(fill = 'salmon', col = 'black') + 
  labs(x = NULL,
       y = NULL,
       title = "WS 1") + 
  theme_bw(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  coord_cartesian(ylim = c(0,8)) +
  scale_x_continuous(breaks = c(0,1))
p.ofd.1  

occ.2 <- birds %>%
  filter(habitat == '2') %>%
  distinct(survey_id, species) %>%      # avoids double-counting repeat records at a site
  count(species, name = "n_sites")

ofd.2 <- occ.2 %>%
  count(n_sites, name = "n_species")

ofd.2$p <- ofd.2$n_sites / max(ofd.2$n_sites)

p.ofd.2 <- ggplot(ofd.2, aes(p, n_species)) + 
  geom_col(fill = 'salmon', col = 'black') + 
  labs(x = NULL,
       y = NULL,
       title = "WS 2") + 
  theme_bw(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) + 
  coord_cartesian(ylim = c(0,15)) +
  scale_x_continuous(breaks = c(0,1))
p.ofd.2  

occ.3 <- birds %>%
  filter(habitat == '3') %>%
  distinct(survey_id, species) %>%      # avoids double-counting repeat records at a site
  count(species, name = "n_sites")

ofd.3 <- occ.3 %>%
  count(n_sites, name = "n_species")

ofd.3$p <- ofd.3$n_sites / max(ofd.3$n_sites)

p.ofd.3 <- ggplot(ofd.3, aes(p, n_species)) + 
  geom_col(fill = 'salmon', col = 'black') + 
  labs(x = NULL,
       y = "Number of Species",
       title = "WS 3") + 
  theme_bw(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) + 
  coord_cartesian(ylim = c(0,8)) +
  scale_x_continuous(breaks = c(0,1))
p.ofd.3  

occ.4 <- birds %>%
  filter(habitat == '4') %>%
  distinct(survey_id, species) %>%      # avoids double-counting repeat records at a site
  count(species, name = "n_sites")

ofd.4 <- occ.4 %>%
  count(n_sites, name = "n_species")

ofd.4$p <- ofd.4$n_sites / max(ofd.4$n_sites)

p.ofd.4 <- ggplot(ofd.4, aes(p, n_species)) + 
  geom_col(fill = 'salmon', col = 'black') + 
  labs(x = "Propotion of Sites Occupied",
       y = NULL,
       title = "WS 4") + 
  theme_bw(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  coord_cartesian(ylim = c(0, 8))  +
  scale_x_continuous(breaks = c(0,1))
p.ofd.4  

occ.5 <- birds %>%
  filter(habitat == '5') %>%
  distinct(survey_id, species) %>%      # avoids double-counting repeat records at a site
  count(species, name = "n_sites")

ofd.5 <- occ.5 %>%
  count(n_sites, name = "n_species")

ofd.5$p <- ofd.5$n_sites / max(ofd.5$n_sites)

p.ofd.5 <- ggplot(ofd.5, aes(p, n_species)) + 
  geom_bar(stat = "identity",fill = 'salmon', col = 'black') + 
  labs(x = NULL,
       y = NULL,
       title = "WS 5") + 
  theme_bw(base_size = 16) + 
  #coord_cartesian(ylim = c(0,)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  coord_cartesian(ylim = c(0,10)) + 
  scale_x_continuous(breaks = c(0,1))
p.ofd.5  

p.ofd.comb <- (p.ofd + p.ofd.1 + p.ofd.2 + 
  p.ofd.3 + p.ofd.4 + p.ofd.5)
p.ofd.comb

ggsave("figs/ofd.png", dpi = 700)

#----1. Richness----

#### plotting dataframe ####
# 
# rich_figmatrix <- as.data.frame(
#   predict_response(rich_mod_pois, terms = c('habitat'))
#   )
# 
# #### plot ####
# p_rich <- ggplot(rich_figmatrix, aes(x, predicted)) + 
#   geom_point() + 
#   geom_errorbar(aes(x,
#                     ymin = conf.low,
#                     ymax = conf.high), 
#                 width = 0.2) + 
#   theme_minimal() + 
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept = 0) +
#   labs(x = 'WS Structure Class',
#        y = 'Average Species Richness')
# p_rich
# 
# rich_figmatrix2 <- as.data.frame(
#   predict_response(mod, terms = c('habitat'))
# )
# 
# p_rich2 <- ggplot(rich_figmatrix2, aes(x, predicted)) + 
#   geom_point() + 
#   geom_errorbar(aes(x,
#                     ymin = conf.low,
#                     ymax = conf.high), 
#                 width = 0.2) + 
#   theme_minimal() + 
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept = 0) +
#   labs(x = 'WS Structure Class',
#        y = 'Average Species Richness')
# p_rich2

#----2. Turnover----

# Load the tidyr package
library(tidyr)

# Convert the dataframe to long format
figmatrix_turnover <- as.data.frame(
  pivot_longer(
  df.coeff.probs,
  cols = starts_with("habitat"),
  names_to = "habitat",
  values_to = "probability"
  )
  )
head(figmatrix_turnover)

figmatrix_turnover$species <- as.factor(figmatrix_turnover$species)
figmatrix_turnover$habitat <- as.factor(figmatrix_turnover$habitat)
#figmatrix_turnover$low.se <- as.numeric(rep('NA', length(figmatrix_turnover$species)))
#figmatrix_turnover$high.se <- as.numeric(rep('NA', length(figmatrix_turnover$species)))

#figmatrix_turnover$low.se[figmatrix_turnover$species == 'main_effect'] <- probabilities_df$`Lower Probability`
#figmatrix_turnover$high.se[figmatrix_turnover$species == 'main_effect'] <- probabilities_df$`Upper Probability`

tail(figmatrix_turnover)

# matts code: 
mod2 <- glmer(pres ~ -1 + habitat + (1 + habitat|species),
              family = binomial,  data = presence)

habs<- as.factor(c(1:5))
spps <- unique(presence$species)

cf2 <- data.frame(summary(mod2)$coefficients)
cf2$probability <- plogis(cf2$Estimate)
cf2$upper <- plogis(cf2[,1] + cf2[,2])
cf2$lower <- plogis(cf2[,1] - cf2[,2])
cf2$habitat <- unique(figmatrix_turnover$habitat)

#### plot ####

p_turnover <- ggplot(figmatrix_turnover[1:190, ], aes(habitat, probability)) + 
  scale_fill_viridis_d(option = "D") + 
  geom_hline(yintercept = c(0,1),
             linetype = 2, col = 'grey') +
  geom_point(aes(col = species)) +
  geom_line(aes(group = species, col = species),
            alpha = 0.3) +
  theme_bw(base_size = 16) + 
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) +
  labs(x = "WHIA Woodland Structure Class",
       y = "Probability of Observation",
       colour = "Species") + 
  scale_x_discrete(labels = c(
    habitat1 = '1 (Open ground, simple)',
    habitat2 = '2 (Open ground, complex)',
    habitat3 = '3 (Dense regeneration)',
    habitat4 = '4 (Young woodland, stem exlusion stage)',
    habitat5 = '5 (Mature woodland, understorey regeneration)'
  ))
p_turnover

# ggsave("figs/turnover_plot.png", dpi = 1000)

p_turnover.2 <- ggplot() +
  geom_hline(yintercept = c(0,1), linetype = 2, col = 'grey') +
  scale_color_viridis_d(labels = function(x) tools::toTitleCase(gsub("_", " ", x)),
                        option = "C") +
  geom_point(data = figmatrix_turnover[1:185, ],
             aes(habitat, probability, col = species)) +
  geom_line(data = figmatrix_turnover[1:185, ],
            aes(habitat, probability, col = species, group = species),
            alpha = 0.3) + 
  geom_line(data = cf2,
            aes(habitat, probability, group = 1),
            colour = "black", alpha = 0.5) +
  geom_point(data = cf2,
             aes(habitat, probability, shape = "Main effect"),
             colour = "black",
             size = 3) +
  geom_errorbar(data = cf2,
                aes(habitat, ymin = lower, ymax = upper),
                width = 0.2, 
                colour = "black") +
  scale_shape_manual(name = NULL, 
                     values = c("Main effect" = 16)) +
  theme_bw(base_size = 14) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
    #legend.position = 'bottom') +
  labs(x = "WHIA Woodland Structure Class",
       y = "Probability of Observation",
       colour = "Species") + 
  scale_x_discrete(labels = c(
    habitat1 = '1',
    habitat2 = '2',
    habitat3 = '3',
    habitat4 = '4',
    habitat5 = '5'
    ))  
p_turnover.2

ggsave("figs/turnover_plot.png", dpi = 700)


# just plot interesting things
# 
# species_to_plot <- colnames(comm_mat)
# 
# filtered_figmatrix <- figmatrix_turnover %>%
#   filter(species %in% species_to_plot)
# 
# # just.main.effect <- subset(figmatrix_turnover, species == 'main_effect')
# 
# ## try have faded out most species, then full colour top 10??
# 
# p_turnover <- ggplot() + 
#   geom_point(data = filtered_figmatrix, aes(habitat, probability,col = species)) +
# 
#   geom_line(data = filtered_figmatrix, aes(x = habitat, y = probability 
#                                              ,group = species, col = species)) +
#   scale_fill_viridis_d() + 
#   labs(x = 'WHIA Habitat Class',
#        y = 'Probability of Observation') +
#   theme_bw()
# p_turnover
# #p_turnover <- p_turnover +# add the main effect so it can be a different alpha value
#  # geom_point(data = cf.main.ef, aes(x = x, y = predicted))
# 
# p_turnover
# 
# ggsave("figs/filtered_turnover_plot.png", dpi = 1000)
# 
# # just main effect
# 
# hab.effect <- figmatrix_turnover %>%
#   filter(species %in% "main_effect")
# 
# hab.effect$richness <- hab.effect$probability * length(unique(presence$species))
# hab.effect

#### old code ####
# p_turnover_2 <- ggplot(figmatrix_turnover, aes(group, predicted)) + 
#   geom_point(aes(shape = x)) + 
# #  geom_line(aes(group = x)) +
#   theme(axis.text.x = element_text(angle = 50, hjust 
#                                    = 1))
# p_turnover_2
# 
# ggsave("figs/turnover_plot_2.png", dpi = 1000)






