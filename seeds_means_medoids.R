####seeds
library(cluster)
library(factoextra)
library(fossil)

data_seeds = data.frame(read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt'))
names(data_seeds) = c('area','perimeter','compactness','length','width','coefficient','groove','types')
data=data_seeds[,-ncol(data_seeds)]
data=scale(data)
dim(data)

fviz_nbclust(data,kmeans,method = "wss")

set.seed(123)
km_res=kmeans(data, 3, nstart=25) 
print(km_res)
fviz_cluster(km_res, data = data,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = FALSE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


table_seeds_means_3=table(km_res$cluster,data_seeds$types)
table_seeds_means_3
rand_index_means_3=rand.index(km_res$cluster,data_seeds$types)
rand_index_means_3


##k-medoids
fviz_nbclust(data, pam, method="silhouette") 


set.seed(123)
pam_data_2=pam(data,2)
print(pam_data_2)
fviz_cluster(pam_data_2,
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

table_seeds_medoids_2=table(pam_data_2$clustering,data_seeds$types)
table_seeds_medoids_2
rand.index_medoids_2=rand.index(pam_data_2$clustering,data_seeds$types)
rand.index_medoids_2

fviz_nbclust(data, pam, method= "gap")

pam_data_3=pam(data,3)
print(pam_data_3)

fviz_cluster(pam_data_3,
             ellipse.type = "t", # Concentration ellipse
             repel = FALSE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
table_seeds_medoids_3=table(pam_data_3$clustering,data_seeds$types)
table_seeds_medoids_3
rand.index_medoids_3=rand.index(pam_data_3$clustering,data_seeds$types)
rand.index_medoids_3


cbind(rand_index_means_3,rand.index_medoids_2,rand.index_medoids_3)
