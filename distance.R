x1=c(14,4,10)
x2=c(12,2,10)
x3=c(13,3,8)
x4=c(13,3,9)
x5=c(10,1,9)
X=cbind(x1,x2,x3,x4,x5)


dist.euc=dist(X, method="euclidean")
dist.euc

t_X=t(X)
cor(t_X)


install.packages("factoextra")
library(factoextra)
dist.cor=get_dist(X, method="pearson")
dist.cor


library(cluster)
data=USArrests
data=scale(data)

dist_eucl=dist(data, method="euclidean")
View(round(as.matrix(dist_eucl)))
fviz_dist(dist_eucl)


dist_man=dist(data, method="manhattan")
View(round(as.matrix(dist_man)))
fviz_dist(dist_man)

round(as.matrix(dist_eucl)[1:5, 1:5], 2)
round(as.matrix(dist_man)[1:5, 1:5], 2)

dist.cor=get_dist(data, method="pearson")
round(as.matrix(dist.cor)[1:5, 1:5], 1)
fviz_dist(dist.cor)

?flower
data("flower")
head(flower,3)
str(flower)
dd=daisy(flower, metric="gower") #veri setinde numerik veri olmadýðýnda Gower's distance heseplanmalý 
round(as.matrix(dd)[1:3, 1:3], 3)


