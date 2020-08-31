library("cluster")
data=scale(USArrests)
head(data)
dist_euc=dist(data, method="euclidean")
dist_man=dist(data, method="manhattan")
as.matrix(dist_euc)[1:6,1:6]
as.matrix(dist_man)[1:6,1:6]

##ward.D2 yöntemi ile
hc_e=hclust(d=dist_euc, method="ward.D2")
plot(hc_e)

hc_m=hclust(d=dist_man, method="ward.D2")
plot(hc_m)

library("factoextra")
fviz_dend(hc_e,cex=.5) #cex yazý büyüklüðü içindir

fviz_dend(hc_m,cex=.5) #cex yazý büyüklüðü içindir

coph_e=cophenetic(hc_e)
cor(dist_euc,coph_e)

coph_m=cophenetic(hc_m)
cor(dist_man,coph_m)

###avarage linkage yöntemi ile
hc_e2=hclust(d=dist_euc, method="average")
plot(hc_e2)
hc_m2=hclust(d=dist_man, method="average")
plot(hc_m2)


fviz_dend(hc_e2,cex=.5) #cex yazý büyüklüðü içindir

fviz_dend(hc_m2,cex=.5) #cex yazý büyüklüðü içindir

coph_e2=cophenetic(hc_e)
cor(dist_euc,coph_e2)

coph_m2=cophenetic(hc_m2)
cor(dist_man,coph_m2)

#### cut tree in 4 groups
grup=cutree(hc_e, k=4)
grup
table(grup)
rownames(data)[grup==1]  
rownames(data)[grup==2]  
rownames(data)[grup==3] 
rownames(data)[grup==4]  

fviz_dend(hc_e, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


fviz_cluster(list(data = data, cluster = grup),
             palette = c("#2E9FDF", "#00FF00", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())


#### cut tree in 3 groups
grup=cutree(hc_e, h=7)
grup
table(grup)
rownames(data)[grup==1]  
rownames(data)[grup==2]  
rownames(data)[grup==3] 
  


fviz_dend(hc_e, k = 3, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


fviz_cluster(list(data = data, cluster = grup),
             palette = c("#2E9FDF", "#00FF00", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())
