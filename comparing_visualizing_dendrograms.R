library("cluster")
library(dendextend)
data=scale(USArrests)

dist_euc=dist(data, method="euclidean")

hc_e_w=hclust(d=dist_euc, method="ward.D2")
plot(hc_e_w)
dend1=as.dendrogram(hc_e_w)

hc_e_a=hclust(d=dist_euc, method="average")
plot(hc_e_a)
dend2=as.dendrogram(hc_e_a)

dist_man=dist(data, method="manhattan")

hc_m_w=hclust(d=dist_man, method="ward.D2")
plot(hc_m_w)
dend3=as.dendrogram(hc_m_w)

hc_m_a=hclust(d=dist_man, method="average")
plot(hc_m_a)
dend4=as.dendrogram(hc_m_a)


tanglegram(dend1,dend2)
dend_list=dendlist(dend1,dend2)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))

tanglegram(dend1,dend3)
dend_list=dendlist(dend1,dend3)
tanglegram(dend1, dend3,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))

dend_list=dendlist(dend1,dend1)
tanglegram(dend1,dend1)
tanglegram(dend1, dend1,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))

dend_list=dendlist(dend2,dend3)
tanglegram(dend2,dend3)
tanglegram(dend2, dend3,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))

#https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html

set.seed(3958)
x <- dendlist(dend1,dend2) %>% untangle(method = "random", R = 10) 
x %>% plot(main = paste("entanglement =", round(entanglement(x), 2)))

x <- dendlist(dend1,dend2) %>% untangle(method = "step2side") 
x %>% plot(main = paste("entanglement =", round(entanglement(x), 2)))

####correlation
cor_cophenetic(dend1, dend2)
cor_bakers_gamma(dend1, dend2)
veya
#dend1:e_w dand2:e_a dand3=m_w dand4=m_a
dend_list=dendlist(dend1,dend2,dend3, dend4)
cor.dendlist(dend_list, method = "cophenetic")
cor.dendlist(dend_list, method = "baker")

###farklý baðlantý fonksyonlarýna göre karþýlaþtýrma yapmak üzere 
dend1 <- data %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- data %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- data %>% dist %>% hclust("average") %>% as.dendrogram
dend4 <- data %>% dist %>% hclust("centroid") %>% as.dendrogram
dend5 <- data %>% dist %>% hclust("ward.D2") %>% as.dendrogram
# Compute correlation matrix
dend_list <- dendlist("Complete" = dend1, "Single" = dend2,
                      "Average" = dend3, "Centroid" = dend4, "ward.D2" = dend5)
cors <- cor.dendlist(dend_list)
cors
# Print correlation matrix
round(cors, 2)
# Visualize the correlation matrix using corrplot package
install.packages("corrplot")
library(corrplot)
corrplot(cors, "pie", "lower")

##### visualizing dendrograms

#dist_euc=dist(data, method="euclidean")
#hc_e_w=hclust(d=dist_euc, method="ward.D2")
#plot(hc_e_w)
#dend1=as.dendrogram(hc_e_w)
library(factoextra)

fviz_dend(hc_e_w, cex=0.5)

fviz_dend(hc_e_w, cex = 0.5,
          main = "Dendrogram - ward.D2",
          xlab = "Objects", ylab = "Distance", sub = "")

fviz_dend(hc_e_w, cex = 0.5, horiz = TRUE)

fviz_dend(hc_e_w, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect_fill = TRUE)

fviz_dend(hc_e_w, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_gray()# Change theme)
)
##themes [ theme_gray(), theme_bw(), theme_minimal(), theme_classic(),theme_void()]

fviz_dend(hc_e_w, cex = 0.5, k = 4, # Cut in four groups
          k_colors = "jco") ##“jco” (journal of clinical oncology)color palette

fviz_dend(hc_e_w, k = 4, cex = 0.4, horiz = TRUE, k_colors = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

fviz_dend(hc_e_w, k = 4, cex = 0.4, horiz = TRUE, k_colors = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

fviz_dend(hc_e_w, cex = 0.6, k = 4,
          k_colors = "jco", type = "circular")

install.packages("igraph")
library("igraph")
fviz_dend(hc_e_w, k = 4, k_colors = "jco",
          type = "phylogenic", repel = TRUE)

fviz_dend(hc_e_w, k = 4, # Cut in four groups
          k_colors = "jco",
          type = "phylogenic", repel = TRUE,
          phylo_layout = "layout.mds")
##phylo_layout = c(“layout.auto”, “layout_with_drl”, “layout_as_tree”, “layout.gem”, “layout.mds”,
##“layout_with_lgl)”


###zooming in the dendrogram
fviz_dend(hc_e_w, xlim = c(1,20), ylim = c(1, 8))

###Plotting a sub-tree of dendrograms
dend_plot <- fviz_dend(hc_e_w, k = 4, # Cut in four groups
                       cex = 0.5, # label size
                       k_colors = "jco")
dend_data <- attr(dend_plot, "dendrogram") # Extract dendrogram data

# Cut the dendrogram at height h = 6
dend_cuts <- cut(dend_data, h = 6)
# Visualize the truncated version containing
# two branches
fviz_dend(dend_cuts$upper)


# Plot the whole dendrogram
print(dend_plot)
# Plot subtree 1
fviz_dend(dend_cuts$lower[[1]], main = "Subtree 1")
# Plot subtree 2
fviz_dend(dend_cuts$lower[[2]], main = "Subtree 2")
# Plot subtree 3
fviz_dend(dend_cuts$lower[[3]], main = "Subtree 3")

# Plot subtree 4
fviz_dend(dend_cuts$lower[[4]], main = "Subtree 4")

#circular trees
fviz_dend(dend_cuts$lower[[4]], type = "circular")

##Saving dendrogram into a large PDF page
pdf("dendrogram.pdf", width=30, height=15) # Open a PDF
p <- fviz_dend(hc_e_w, k = 4, cex = 1, k_colors = "jco" ) # Do plotting
print(p)
dev.off() # Close the PDF

##R code for creating a dendrogram using chaining operator:
library(dendextend)
dend <- USArrests %>% # data
  scale %>% # Scale the data
  dist %>% # calculate a distance matrix,
  hclust(method = "ward.D2") %>% # Hierarchical clustering
  as.dendrogram # Turn the object into a dendrogram.
plot(dend)
