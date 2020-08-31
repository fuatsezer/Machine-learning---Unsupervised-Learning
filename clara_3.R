##CLARA
set.seed(1234)
data=rbind(cbind(rnorm(200,0,8),rnorm(200,0,8)),
           cbind(rnorm(200,25,8),rnorm(200,25,8)),
           cbind(rnorm(300,50,8),rnorm(300,50,8)))
colnames(data)=c("x","y")
rownames(data)=paste0("S",1:nrow(data))
head(data)
dim(data)

library(cluster)
library(factoextra)


fviz_nbclust(data, clara, method= "silhouette")


data_clara <- clara(data, 3, samples = 50, pamLike = TRUE)
print(data_clara)

dd <- cbind(data, cluster = data_clara$cluster)
head(dd)


fviz_cluster(data_clara,
             ellipse.type = "t", # Concentration ellipse
             geom="point", pointsize=1,
             ggtheme = theme_classic()
)
