#R6
library(R6)

data(iris)

iris <- iris[, -5]

df_scaled <- scale(iris)
kmeans_result <- kmeans(df_scaled, centers = 3, nstart = 10)
iris$cluster <- as.factor(kmeans_result$cluster)

library(ggplot2)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color = cluster))+
         geom_point(size = 3)

library(dbscan)
db <- dbscan(df_scaled, eps = 0.5, minPts = 5)
iris$cluster_db <- as.factor(db$cluster)

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color = cluster_db))+
  geom_point(size = 3)

install.packages("clusterCrit")
library(cluster)
library(clusterCrit)


sil_kmeans <- silhouette(db$cluster, dist(df_scaled))
mean_sil_kmeans <- mean(sil_kmeans[, 3])

db_index_kmeans <- intCriteria(as.matrix(df_scaled), db$cluster, c("Davies_Bouldin"))
db_kmeans <- db_index_kmeans$davies_bouldin


