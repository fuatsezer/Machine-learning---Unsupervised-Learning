install.packages(c("factoextra", "clustertend"))
library(factoextra)
library(clustertend)
install.packages("NbClust")
library(NbClust)


head(iris)
data=iris[,-5]
head(data)
dim(data)
summary(data)
boxplot(data)

random_data <- apply(data, 2,
                   function(x){runif(length(x), min(x), (max(x)))})
random_data <- as.data.frame(random_data)
dim(random_data)
head(random_data)
summary(random_data)
boxplot(random_data)

par(mfrow=c(2,1))
boxplot(data)
boxplot(random_data)
par(mfrow=c(1,1))

data=scale(data) ## orjinal iris verisi standartlaþtýrýmýþ 
random_data=scale(random_data) ##türetilmiþ iris verisi standartlaþtýrýmýþ


fviz_pca_ind(prcomp(data), title = "PCA - Iris data",
            palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")
fviz_pca_ind(prcomp(data), title = "PCA - Iris data",
             habillage = iris$Species, palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

fviz_pca_ind(prcomp(random_data), title = "PCA - Random data",
             geom = "point", ggtheme = theme_classic())



set.seed(123)
# K-means on iris dataset
km_data <- kmeans(data, 3)
fviz_cluster(list(data = data, cluster = km_data$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

# K-means on the random dataset
km_random_data <- kmeans(random_data, 3)
fviz_cluster(list(data = random_data, cluster = km_random_data$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())
# Hierarchical clustering on the random dataset
fviz_dend(hclust(dist(random_data)), k = 3, 
          as.ggplot = TRUE, show_labels = FALSE)

set.seed(123)
h_data=hopkins(data, nrow(data)-1)
h_random_data=hopkins(random_data, nrow(random_data)-1)
cbind(h_data,h_random_data)

fviz_dist(dist(data), show_labels = FALSE )+
  labs(title = "Iris data")

fviz_dist(dist(random_data), show_labels = FALSE)+
  labs(title = "Random data")


###küme sayýsýnýn seçimi
p1=fviz_nbclust(data, kmeans, method = "wss") +
    labs(subtitle = "Elbow method")
p1

p1=fviz_nbclust(data, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
p1
##Nihat'ýn önermiþ olduðu kod
  wss_degerleri <- vector("numeric", length = 10)
    for (i in 1:10){
    k_sonuclar <- kmeans(data, i,  nstart = 25, iter.max = 200 )
    wss_degerleri[i] <- k_sonuclar$tot.withinss
  }
  plot(wss_degerleri, type = "b")


# Silhouette method
  p2=fviz_nbclust(data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
p3=fviz_nbclust(data, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
    labs(subtitle = "Gap statistic method")

##gap yöntemi için alternatif çizim
library(cluster)
gap_data<- clusGap(data, FUN = kmeans, K.max = 8, B = 60)
gap_data
plot(gap_data)

p3=fviz_gap_stat(
  gap_data,
  linecolor = "steelblue",
  maxSE = list(method = "firstSEmax", SE.factor = 1)
)

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)


#30 indices for choosing the best number of clusters
nb <- NbClust(data, distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "kmeans")
help("NbClust")
nb$All.index
nb$All.CriticalValues
nb$Best.nc
nb$Best.partition
fviz_nbclust(nb)

nb_rd <- NbClust(random_data, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb_rd)

#Computing cluster validation statistics
# K-means clustering
km_data <- eclust(data, "kmeans", k = 3, nstart = 25, graph = TRUE)
km_data
# Visualize k-means clusters
fviz_cluster(km_data, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

# Hierarchical clustering
hc_data <- eclust(data, "hclust", k = 3, hc_metric = "euclidean",hc_method = "ward.D2", graph = FALSE)
# Visualize dendrograms
fviz_dend(hc_data, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

##Silhouette plot
fviz_silhouette(km_data, palette = "jco",
                ggtheme = theme_classic())

silinfo <- km_data$silinfo
silinfo
names(silinfo)
# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width
# The size of each clusters
km_data$size
# Silhouette width of observation
sil <- km_data$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width']<0)
sil[neg_sil_index, , drop = FALSE]

#Computing Dunn index and other cluster validation statistics
install.packages("fpc")
library(fpc)
# Statistics for k-means clustering
km_stats <- cluster.stats(dist(data), km_data$cluster)
# Dun index
km_stats$dunn

#External clustering validation
# Agreement between species and k-means clusters

#km_data <- eclust(data, "kmeans", k = 3, graph = FALSE)
table(iris$Species, km_data$cluster)
species <- as.numeric(iris$Species)
cluster.stats(d = dist(data),species, km_data$cluster)$corrected.rand
cluster.stats(d = dist(data),species, km_data$cluster)$vi
# Agreement between species and HC clusters

#hc_data <- eclust(data, "hclust", k = 3, graph = FALSE)
table(iris$Species, hc_data$cluster)
cluster.stats(d = dist(data),species, hc_data$cluster)$corrected.rand
cluster.stats(d = dist(data),species, hc_data$cluster)$vi

#Compare clustering algorithms 
install.packages("clValid")
library(clValid)
clmethods <- c("kmeans","pam","hierarchical")
intern <- clValid(data, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)


# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(data, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
summary(stab)
# Display only optimal Scores
optimalScores(stab)



