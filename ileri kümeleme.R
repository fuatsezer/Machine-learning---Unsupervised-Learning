
##k-means
df <- scale(USArrests)
# Compute hierarchical k-means clustering
library(factoextra)
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
km_data=kmeans(df, 4, nstart=25) 
print(km_data)
fviz_cluster(km_data, data = df,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

##hierarchical k-means clustering (hkmeans)
res.hk <-hkmeans(df, 4)
# Print the results
res.hk
# Visualize the tree
fviz_dend(res.hk, cex = 0.6, palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)
# Visualize the hkmeans final clusters
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())

km_data$centers
res.hk$centers


####
##iris data
data=scale(iris[,-5])
summary(data)
set.seed(123)
  # K-means on iris dataset
km_data <- kmeans(data, 3)
print(km_data)
fviz_cluster(list(data = data, cluster = km_data$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

  ##hierarchical k-means clustering (hkmeans) on iris data
res.hk <-hkmeans(data, 3)
res.hk
  # Visualize the tree
fviz_dend(res.hk, cex = 0.6, palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)
  # Visualize the hkmeans final clusters
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())

#########################
###model-based clustering
########################

library(MASS)
data=geyser
dim(data)
summary(data)
help(geyser)
head(data)
plot(data$duration,data$waiting)
boxplot(data)

# Scatter plot
library("ggpubr")
ggscatter(geyser, x = "duration", y = "waiting")+
  geom_density2d() # Add 2D density

library(mclust)
data=scale(geyser)
mc=Mclust(data)
summary(mc)
View(mc$z)
head(mc$z,10)
mc$G
head(mc$classification,10)

library(factoextra)
# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)


#G=3
mc=Mclust(data, G=3)
summary(mc)
head(mc$z,10)
head(mc$classification,10)


# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)

###################################
#### Density-Based Clustering
###################################
library(factoextra)
summary(multishapes)
as.factor(multishapes[,3])
data("multishapes")
df <- multishapes[, 1:2]
dim(df)
plot(df, col=c("red","blue","green","black","purple","pink")[multishapes[, 3]])
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
km.res <- kmeans(df, 5, nstart = 25)
km.res
fviz_cluster(km.res, df, geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

install.packages("fpc")
library(fpc)
install.packages("dbscan")
library(dbscan)

# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 2)
# Plot DBSCAN results
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
print(db)

dbscan::kNNdistplot(df, k = 5)
abline(h = 0.15, lty = 2)
