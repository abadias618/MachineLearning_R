library(datasets)
library(cluster)
library(factoextra)

#data
states <- state.x77
#normalize
scaled <- scale(states)

# finde best k-means using the elbow method
total_ss = NULL;
for (i in 1:25) {
  clusters <- kmeans(scaled, i)
  total_ss[i] <- clusters$tot.withinss
}
plot(total_ss)

# another way of determinining best kmeans with factoextra
fviz_nbclust(scaled, kmeans, method = "gap_stat")

# Cluster into k=5 clusters caulculated with the elbow method (for loop above)
myClusters <- kmeans(scaled, 5)
# Summary of the clusters
summary(myClusters)
# Centers (mean values) of the clusters
myClusters$centers
# Cluster assignments
myClusters$cluster
# Within-cluster sum of squares and total sum of squares across clusters
myClusters$withinss
myClusters$tot.withinss

# Plotting a visual representation of k-means clusters
#with cluster:
#clusplot(scaled, myClusters$cluster, color=TRUE, shade=FALSE, labels=2, lines=0)
#with factoextra
fviz_cluster(myClusters, data = scaled, palette = "jco", ggtheme = theme_minimal())
