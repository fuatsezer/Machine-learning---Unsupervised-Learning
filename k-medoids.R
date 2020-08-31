#install.packages("cluster","factoextra","fpc") #"fpc" paketi pamk fonksiyonu için gerekli
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)
install.packages("fpc")
library(fpc)
install.packages("fossil") #rand.indeks için
library("fossil")
##K-medoids

data=scale(USArrests)

fviz_nbclust(data, pam, method= "silhouette") #max. olduðu nokta dikkate alýndýðýndan k deðeri 2 olarak seçilir.

set.seed(123)
pam_data_2=pam(data,2)
print(pam_data_2)
pam_data_2$medoids
pam_data_2$clustering

fviz_cluster(pam_data_2,
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)


pamk_data=pamk(data) #bu fonksiyonda kullanýcýnýn k deðeri belirlemesine gerek yoktur.
print(pamk_data)
pamk_data$pamobject



pam_data_4=pam(data,4)
print(pam_data_4)
pam_data_4$medoids
pam_data_4$clustering

fviz_cluster(pam_data_4,
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)


