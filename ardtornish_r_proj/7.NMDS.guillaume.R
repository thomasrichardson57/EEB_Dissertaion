library(ape)
library(betapart)
library(NbClust)
library(dendextend)
library(vegan)
library(ggplot2)
library(viridis)
library(dplyr)
# com.mat = turnover (how similar the sites are)
beta <- beta.pair(comm_mat)


# NbClust(data = NULL, diss=beta$beta.sim, distance = NULL, method = "ward.D", index = c("frey", "mcclain", "cindex","sihouette","dunn"))
NbClust(data = NULL, diss=beta$beta.sim, distance = NULL, method = "ward.D", index = "frey") ## suggest 2 clusters is best
NbClust(data = NULL, diss=beta$beta.sim, distance = NULL, method = "ward.D", index = "mcclain") ## suggest 2 clusters is best
NbClust(data = NULL, diss=beta$beta.sim, distance = NULL, method = "ward.D", index = "cindex") ## suggest 15 clusters is best
NbClust(data = NULL, diss=beta$beta.sim, distance = NULL, method = "ward.D", index = "silhouette") ## suggest 12 clusters is best
NbClust(data = NULL, diss=beta$beta.sim, distance = NULL, method = "ward.D", index = "dunn") ## returns 0 whatever the number of clusters, so unreliable despite saying 2 clusters is best


k <- 3 # this is the number of clusters we retain - try also 3 and 4



hcluster <- hclust(d = beta$beta.sim, method = "ward.D") # look at ward.D and different algorithms
 plot(hcluster)


##plot dendrogram with fancy colours
dend <- as.dendrogram(hcluster)
clusters <- cutree(hcluster, k = k)
cluster_colors <- c("steelblue", "darkorange", "forestgreen", "firebrick")

## this bit reorder the cluster ids to match the way they are displayed
ordered_clusters  <- unique(clusters[labels(dend)])
remap             <- setNames(seq_along(ordered_clusters), ordered_clusters)
clusters_remapped <- remap[as.character(clusters)]
names(clusters_remapped) <- names(clusters)

dend <- color_branches(dend, k = k, col = cluster_colors)
labels_colors(dend) <- cluster_colors[clusters_remapped[labels(dend)]]

#png("figs/dendrogram.png", width = 480, height = 480)
plot(dend,
     main = "Hierarchical clustering (Simpson dissimilarity)",
     ylab = "Dissimilarity")
#rect.dendrogram(dend, k = k, border = cluster_colors, lty = 2, lwd = 2)
#dev.off()

#### Elm dendrogram ####
# install.packages("factoextra")
library(factoextra)

hcluster2 <- hcluster

hcluster2$labels <- birds$habitat[match(hcluster2$labels, birds$survey_id)]


fviz_dend(hcluster2,
          k = 3,
          rect = TRUE,
          horiz = TRUE,
          cex = 0.5,
          k_colors = c("steelblue", "darkorange", "forestgreen"),
          main = "Hierarchical clustering (Simpson dissimilarity)",
          ylab = "Dissimilarity") 

ggsave("figs/dend.png", dpi = 700)


## nmds = 
nmds_result <- metaMDS(beta$beta.sim,  k             = 2,      # 2D ordination
                       trymax        = 100,    # maximum number of random starts
                       autotransform = FALSE ) # lingoes = corrects for neg' eigenvalues
# pcoa.result <- pcoa(beta$beta.sim, correction = "none") # lingoes = corrects for neg' eigenvalues 
nmds_result


# --- 2. Check stress ---
nmds_result$stress
# Interpreting Stress
# Stress value	Interpretation
# < 0.05	Excellent
# 0.05 – 0.10	Good
# 0.10 – 0.20	Acceptable, interpret with care
# > 0.20	Poor — 2D may be misleading


# --- 3. Shepard diagram: how faithfully the 2D space represents ---
# the original dissimilarities (points close to the line = good fit)
stressplot(nmds_result)

# --- 4. Extract site scores and attach cluster IDs ---
nmds_scores          <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_scores$survey_id     <- rownames(nmds_scores)
# nmds_scores$cluster  <- as.factor(clusters_remapped[as.character(clusters)])
nmds_scores$cluster  <- as.factor(clusters_remapped)

# --- 5. Plot ---
cluster_colors <- c("steelblue", "darkorange", "forestgreen", "firebrick")

# add hab column:

nmds_scores <- nmds_scores %>% 
  left_join(metadata %>% select(survey_id, habitat), by = "survey_id")
nmds_scores$habitat <- as.factor(nmds_scores$habitat)
head(nmds_scores)

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, colour = habitat, label = survey_id)) +
  #scale_color_viridis_d(option = 'E')+
  geom_point(size = 3, aes(pch = cluster)) +
#  geom_text(nudge_y = 0.02, size = 3) +   # remove if site labels are too cluttered
 # scale_colour_manual(values = cluster_colors, name = "Cluster") +
    annotate("text",
           x      = min(nmds_scores$NMDS1),
           y      = max(nmds_scores$NMDS2),
           label  = paste("Stress =", round(nmds_result$stress, 3)),
           hjust  = 0,
           size   = 4) +
  theme_bw(base_size = 16) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "NMDS of Simpson dissimilarity",
       x = "NMDS1", y = "NMDS2",
       col = "WS Class",
       pch = "Cluster") # change labels of habitat classes to full e.g., 1 (open ground, simple)

ggsave("figs/nmds.png", dpi = 700)


#### random forest ####
library(randomForest)

## response = cluster.nb

# add a column of WHIA class
survey_metadata <- read.csv("data/survey_metadata.csv", header = TRUE)

df$hab <- birds$habitat

# Ensure correct variable types
site_data$cluster <- as.factor(site_data$cluster)   # response must be a factor
# categorical predictors must be factors too, e.g.:
site_data$habitat <- as.factor(site_data$habitat)

# Fit the model
rf_model <- randomForest(
  cluster ~ .,     # predict cluster from all other columns
  data       = site_data,
  ntree      = 500,
  importance = TRUE  # needed for variable importance
)

# Summary: includes Out-Of-Bag (OOB) error and confusion matrix
print(rf_model)

# Variable importance (how much each predictor contributes)
varImpPlot(rf_model, main = "Variable Importance")

# Predict cluster for new sites
predicted_clusters <- predict(rf_model, newdata = new_sites)

