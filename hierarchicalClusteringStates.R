library(datasets)
library(factoextra)
library(pheatmap)

#data
states <- state.x77

#RAW DATA
#compute a distance matrix
distance <- dist(as.matrix(states))
#perform the clustering
hc <- hclust(distance)
#plot the dendrogram
plot(hc)



#SAME OPERATION WITH SCALED DATA
scaled <- scale(states)
#compute a distance matrix
distance_2 <- dist(as.matrix(scaled))
#perform the clustering
hc_2 <- hclust(distance_2)
#plot the dendrogram
plot(hc_2)
#heat map
pheatmap(t(scaled), cutree_cols = 8)

#REMOVING THE AREA COLUMN
states_no_area <- subset(scaled, select = -c(Area))
#compute a distance matrix
distance_3 <- dist(as.matrix(states_no_area))
#perform the clustering
hc_3 <- hclust(distance_3)
#plot the dendrogram
plot(hc_3)

# CLUSTER ONLY USING FROST COLUMN
states_only_frost <- subset(scaled, select = c(Frost))
#compute a distance matrix
distance_4 <- dist(as.matrix(states_only_frost))
#perform the clustering
hc_4 <- hclust(distance_4)
#plot the dendrogram
plot(hc_4)
#plot with factoextra
fviz_dend(hc_4, cex = 0.5, k = 12, palette = "jco")