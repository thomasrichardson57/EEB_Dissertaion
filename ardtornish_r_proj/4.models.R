#----Packages----
library(lme4)
library(lmerTest)
library(DHARMa)
library(ggplot2)
library(emmeans)
library(ggeffects)
library(writexl)
#----summary stats----
unique(rich$survey_id)
length(unique(rich$survey_id))

# png("figs/sectorplot.png", width = 480, height = 480)
plot(table(rich$sector), cex = 2,
     xlab = "Sector",
     ylab = "Number of Surveys"
     )
# dev.off()

png("figs/hab_plot.png", width = 480, height = 480)
plot(table(rich$habitat),
     xlab = 'Habitat Class',
     ylab = 'Number of Surveys')
dev.off()

table(rich$habitat)

# total surveys
length(unique(birds$survey_id))

#species list
length(unique(birds$species))

#### subset habitats ####
sp <- c(1:5)

birds.1 <- subset(birds, habitat == 1)
sp[1] <- length(unique(birds.1$species))

birds.2 <- subset(birds, habitat == 2)
unique(birds.2$species)
sp[2] <- length(unique(birds.2$species))#

birds.3 <- subset(birds, habitat == 3)
unique(birds.3$species)
sp[3] <- length(unique(birds.3$species))

birds.4 <- subset(birds, habitat == 4)
unique(birds.4$species)
sp[4] <- length(unique(birds.4$species))

birds.5 <- subset(birds, habitat == 5)
unique(birds.5$species)
sp[5] <- length(unique(birds.5$species))

hab.sp.df <- data.frame(
  hab = as.factor(c(1:5)),
  species = sp
)

png("figs/hab.spp.plot.png", width = 480, height = 480)
plot(hab.sp.df, ylim = c(0,25))
dev.off()

p.hab.rich <-
  ggplot(hab.sp.df, aes(hab, species)) + 
  geom_bar(stat = 'identity',
           fill ='burlywood3', 
           col = 'black') + 
  theme_bw(base_size = 20) + 
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) + 
  coord_cartesian(ylim = c(0,25)) +
  labs(x = "WHIA Woodland Structure Class",
       y = "Number of species observed throughout study") + 
  geom_hline(yintercept = 0)
p.hab.rich

ggsave("figs/hab.sp.bar.png", dpi = 700)

range(birds$elevation)

length(unique(birds$species))

#----1. Plain Richness----
# 
# ggplot(rich, aes(richness)) + 
#   geom_histogram()
# 
# rich_mod_pois <- glm(richness ~ habitat + sector, data = rich,
#                   family = poisson)
# summary(rich_mod_pois)
# anova(rich_mod_pois)
# # par(mfrow = c(2,2))
# # plot(rich_mod1)
# # par(mfrow = c(1,1))
# 
rich_mod_norm <- lm(richness ~ habitat, data = rich)
summary(rich_mod_norm)
anova(rich_mod_norm)

#----2. Hadfield Approach----

#try without class 4
#presence <- presence[presence$habitat != 4, ]
#unique(presence$habitat)

turnover_mod1 <- glmer(pres ~ habitat + (1 + habitat|species),
                       family = binomial,  data = presence)
summary(turnover_mod1)
anova(turnover_mod1)

turnover_mod2 <- glmer(pres ~ habitat + (1|species),
                       family = binomial, data = presence)
summary(turnover_mod2)
anova(turnover_mod2)
anova(turnover_mod1, turnover_mod2)
# mod <- glm(pres ~ habitat * species, family = binomial, data = presence)
# summary(mod)
# anova(mod)
# par(mfrow = c(2,2))
# plot(mod)
# par(mfrow = c(1,1))

# mod2 <- glm(pres ~ habitat + species, family = binomial, data = presence)
# summary(mod2)
# anova(mod2)
# 
# par(mfrow = c(2,2))
# plot(mod2)
# par(mfrow = c(1,1))
# 
# anova(mod, mod2)
# 
# figmatrix1 <- as.data.frame(predict_response(mod, terms = c('habitat', 'species'))
# )
# 
#                             
# sim_res <- simulateResiduals(turnover_mod1)
# plot(sim_res)



#----3. Environmental Variation----
hist(birds$elevation)
max(birds$elevation)

x <- c(1:100)
y <- c(1:100)
plot(x, -exp(y))

turnover_mod1.env <- glm(
  pres ~ elevation + days + start_time.dec,
  family = binomial, data = presence)
summary(turnover_mod1.env)
anova(turnover_mod1.env)


#----4. extract coefficients ----

# overall estimates #
turnover_mod1.summary <- summary(turnover_mod1)
turnover_mod1.fixef <- turnover_mod1.summary$coefficients[, "Estimate"]
turnover_mod1.fixef
# colnames(turnover_mod1.fixef) <- c('habitat1','habitat2','habitat3',
                                 #  'habitat3','habitat4','habitat5')

# sp' specific estimates
df.coeff.1 <- as.data.frame(coef(turnover_mod1)$species)
names(df.coeff.1)
rownames(df.coeff.1)

df.coeff.1 <- rbind(df.coeff.1, turnover_mod1.fixef) # add a line for the 'main effect' (richness?)

#df.coeff.1[38,1]

# change it so that species is an actual column, not just rownames
df.coeff.2 <- data.frame(
  species = rownames(df.coeff.1),
  habitat1 = df.coeff.1$`(Intercept)`,
  habitat2 = df.coeff.1$habitat2,
  habitat3 = df.coeff.1$habitat3,
  habitat4 = df.coeff.1$habitat4,
  habitat5 = df.coeff.1$habitat5
)
df.coeff.2 # check

df.coeff.2[length(df.coeff.2$species),1] <- 'main_effect'
df.coeff.2

#### absolute coeffs ####

#make everything absolute, not relative to intercept (meadow pipit, hab1)
intercept <- df.coeff.2[1, ]
intercept
intercept[3] <- intercept[3] + intercept[2]
intercept[4] <- intercept[4] + intercept[2]
intercept[5] <- intercept[5] + intercept[2]
intercept[6] <- intercept[6] + intercept[2]
intercept

df.coeff.absolute <- df.coeff.2[-1,-1]
head(df.coeff.absolute)
dim(df.coeff.absolute)

# iterate through each column, adding the value of each row to the intercept for
# that column
x <- 1
i <- 1
for (x in 1:length(colnames(df.coeff.absolute))){
  for (i in 1:length(df.coeff.absolute[,1])){
  df.coeff.absolute[i, x] <- df.coeff.absolute[i, x] +intercept[x + 1]
  i + 1
  }
  x + 1
}

# add mipit row back in:
df.coeff.absolute <- rbind(df.coeff.absolute, intercept[-1])
df.coeff.absolute <- df.coeff.absolute[c(nrow(df.coeff.absolute), 1:(nrow(df.coeff.absolute) - 1)), ]

# add species back in:
df.coeff.absolute$species <- df.coeff.2$species
head(df.coeff.absolute)  

#### odds ratios ####
df.coeff.odds <- exp(df.coeff.absolute[ , 1:5])
head(df.coeff.odds)

#### probabilities ####
df.coeff.probs <- df.coeff.odds / (1 + df.coeff.odds)
head(df.coeff.probs)
max(df.coeff.probs)
min(df.coeff.probs)

# add species back in
df.coeff.probs$species <- df.coeff.2$species
head(df.coeff.probs)
tail(df.coeff.probs)

# export as an excel table

write_xlsx(df.coeff.probs, "data/df.turnover.mod.spp.xlsx")

# to convert the SE's for the habitat main effects into probs: [ELM]

# Extract coefficients and standard errors
coefficients <- turnover_mod1.summary$coefficients[, "Estimate"]
se_values <- turnover_mod1.summary$coefficients[, "Std. Error"]

# Calculate probabilities from SEs
calculate_probabilities <- function(coeff, se) {
  # Calculate lower and upper bounds for logits
  lower_logit <- coeff - se
  upper_logit <- coeff + se
  
  # Convert logits to probabilities
  lower_prob <- 1 / (1 + exp(-lower_logit))
  upper_prob <- 1 / (1 + exp(-upper_logit))
  
  # Return a list of probabilities
  return(c(lower_prob, upper_prob))
}

# Apply the function and convert to a data frame
probabilities <- t(mapply(calculate_probabilities, coefficients, se_values))
probabilities_df <- as.data.frame(probabilities, row.names = names(coefficients))
colnames(probabilities_df) <- c("Lower Probability", "Upper Probability")

cf.main.ef <- as.data.frame(
  predict_response(turnover_mod1, terms = c('habitat'))
)

  #### old code ####
# codf.coeff.probs# colnames(df) <- c('species', 'Intercept', 'habitat2', 'habitat3', 
#                   'habitat4', 'habitat5')
# df.no.intercept <- df[,-1]
# df.no.mipit <- df[-1,]
# 
# # From ELM: how to extract ABSOLUTE possibilities to plot, rather than the 
# # in reference to the intercept
# coeff <- coef(turnover_mod1)$species
# coeff
# intercept <- coeff[, "(Intercept)"]
# intercept
# other_effects <- coeff[ , -1] 
# linear_predictor <- intercept

# coeff <- coef(turnover_mod1)$species
# odds_ratios <- exp(coeff)
# probs_obs <- odds_ratios / (1 + odds_ratios)
# probs_obs # probability of observing each species, in each habitat
# 

# 
# mod <- glm(pres ~ habitat*species, data = presence, family = binomial)
# summary(mod)
# anova(mod)
# 
# 
# turnover_mod3 <- glm(pres ~ species*habitat, 
#                      family = binomial,
#                      data = presence)
# anova(turnover_mod3)
# turnover_mod4 <- glm(pres ~ species + habitat, 
#                      family = binomial,
#                      data = presence)
# 
# anova(turnover_mod3, turnover_mod4)
# #LOGIT(mod$coefficients)
# coef(turnover_mod1)
# 
# # Extract the coefficients list
# coef_list <- coef(turnover_mod1)$species
# 
# # Convert the list to a dataframe
# coef_df <- as.data.frame(exp(coef_list))
# coef_df

