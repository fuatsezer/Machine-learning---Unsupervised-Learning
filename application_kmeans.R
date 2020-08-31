data=USArrests
summary(data)
library(caret)
library(e1071)
pca = prcomp(data, center = TRUE, scale. = TRUE)
summary(pca)
data_new=pca$x[,1:2]

#par(mfrow=c(1,1))
fviz_nbclust(data_new,kmeans,method="wss")
sonuc=kmeans(data_new,4)
sonuc
table(sonuc$cluster)
plot(data_new,col=sonuc$cluster,lwd=2)

fviz_cluster(sonuc, data = data,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

par(mfrow=c(1,1))
fviz_nbclust(pca$x,kmeans,method="wss")
sonuc=kmeans(data_new,4)
sonuc
table(sonuc$cluster)
plot(pca$x[,1:2],col=sonuc$cluster,lwd=2)

fviz_cluster(sonuc, data = data,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

#############################
##farkli distance measuerlar
#############################

install.packages("amap")
library("amap")

fviz_nbclust(scale(USArrests),Kmeans,method = "wss")

sonuc_K=Kmeans(scale(USArrests),centers=3,method="pearson")
print(sonuc_K)

#"euclidean", "maximum", "manhattan", "canberra" 
#"binary", "pearson" , "abspearson" , "abscorrelation"
#"correlation", "spearman" or "kendall"

fviz_cluster(sonuc_K, data = data,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


##seeds data set
data_seeds = data.frame(read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt'))
names(data_seeds) = c('area','perimeter','compactness','length','width','coefficient','groove','types')

str(data_seeds)
table(data_seeds$types)

data=data_seeds[,-ncol(data_seeds)]

boxplot(data_seeds)

###orjinal veri icin


fviz_nbclust(data_seeds,kmeans,method="wss")

sonuc=kmeans(data_seeds,3)
sonuc

table(sonuc$cluster,data_seeds$types)

install.packages("fossil") #rand.indeks için
library("fossil")

rand.index(sonuc$cluster,data_seeds$types)

fviz_cluster(sonuc,data_seeds,ellipse.type="euclid",star.plot=T,ggtheme=theme_minimal())



###standartlastirilmis veri icin

data=scale(data_seeds)
fviz_nbclust(data,kmeans,method="wss")

sonuc=kmeans(data,3)
sonuc

table(sonuc$cluster,data_seeds$types)
rand.index(sonuc$cluster,data_seeds$types)

fviz_cluster(sonuc,data,ellipse.type="euclid",star.plot=T,ggtheme=theme_minimal())
 
























