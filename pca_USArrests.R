################
install.packages("cluster")
library(cluster)
installed.packages("devtools")
library(devtools)
devtools::install_github("kassambara/factoextra")

?USArrests
data=USArrests
dim(data)
head(data)

summary(data)
apply(data,2,var)

library(pastecs)
stat.desc(data)

require(graphics)
pairs(USArrests)

attach(data)
boxplot(data)
data.cor=cor(data)
data.eigen=eigen(data.cor)



prop.var=data.eigen$value/sum(data.eigen$values )
prop.var
cum.prop.var=cumsum(prop.var)
cum.prop.var

rbind(data.eigen$value,prop.var,cum.prop.var)

plot(prop.var , xlab=" Principal Component ", ylab=" Proportion of
Variance Explained ", ylim=c(0,1) ,type='b')
lines(cum.prop.var, type='b', col="red")

plot(cum.prop.var, xlab=" Principal Component ", ylab ="
Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b')



data.eigen$values
sqrt(data.eigen$values)

data.pca <- prcomp(data, center = TRUE, scale. = TRUE)
summary(data.pca)
data.pca$sdev
data.pca$rotation
data.pca$center
data.pca$scale
data.pca$x
data.pca$x [1:10,]
predict(data.pca)[1:10,]
(scale(data)%*%data.pca$rotation)[1:10,]

par(mfrow=c(2,1))
screeplot(data.pca)
screeplot(data.pca, type='lines')
dev.off()
biplot(data.pca)
cor(data)


install.packages("factoextra")
library(factoextra)
fviz_eig(data.pca)

# Eigenvalues
eig.val <- get_eigenvalue(data.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(data.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# Results for individuals
res.ind <- get_pca_ind(data.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 


res.ind$coord[1:10,]


##Graph of individuals. Individuals with a similar profile are grouped together
fviz_pca_ind(data.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


##Graph of variables. Positive correlated variables point to the same side of the plot. 
##Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(data.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


##Biplot of individuals and variables
fviz_pca_biplot(data.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)




