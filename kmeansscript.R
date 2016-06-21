library(datasets)
library(stats)
head(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species,size = 1)) +
  geom_point()
set.seed(20)
 
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)

# K-means clustering with 3 clusters of sizes 50, 52, 48
# Cluster means:
#   Petal.Length Petal.Width
# 1     1.462000    0.246000 ( 1st 50 ) -sum( 1:50)/50
# 2     4.269231    1.342308 ( 2st 52 ) -sum( 1:48)/48
# 3     5.595833    2.037500( 3st 48 )-sum( 1:52)/52
# 
# Clustering vector:
#   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [45] 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 3 2 2 2 2
# [89] 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 2 3 3 3 3 3
# [133] 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3
# 
# Within cluster sum of squares by cluster:
#   [1]  2.02200 13.05769 16.29167
# (between_SS / total_SS =  94.3 %)


table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)

ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()