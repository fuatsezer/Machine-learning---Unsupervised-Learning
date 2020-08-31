data=iris
data=data[,-5]
summary(data)
boxplot(data)
pairs(data[,1:4], col=iris$Species)
plot(data$Petal.Length,data$Petal.Width,col=iris$Species)
head(data)
data_new=scale(data[,3:4])
fviz_nbclust(data_new,kmeans,method="wss")
sonuc=kmeans(data_new,3)
sonuc
table(sonuc$cluster)

s=c(rep(1,50), rep(2,50), rep(3,50))


fviz_cluster(sonuc, data = data_new,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


library("fossil")
table(sonuc$cluster,s)
rand.index(sonuc$cluster,s)
