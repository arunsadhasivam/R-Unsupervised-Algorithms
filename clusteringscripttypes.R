

km.res <- kmeans(iris.scaled, 3, nstart = 25)
#visualize k-means
fviz_cluster(km.res, data = iris.scaled, geom = "point",
             stand = FALSE, frame.type = "norm")


#partitioning method results(PAM)
# PAM clustering
pam.res <- pam(iris.scaled, 3)
fviz_cluster(pam.res, stand = FALSE, geom = "point",
             frame.type = "norm")



#hierarchial clustering
dist.res <- dist(iris.scaled, method = "euclidean")
# Hierarchical clustering results
hc <- hclust(dist.res, method = "complete")
# Visualization of hclust
plot(hc, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(hc, k = 3, border = 2:4) 

# Cut into 3 groups
hc.cut <- cutree(hc, k = 3)
head(hc.cut, 20)








#Three popular methods for determining the optimal number of clusters

#1 Elbow method
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
data <- iris.scaled
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)
#output shows k=3 , 3 clusters suggested.



#2.Average silhouette method

library(cluster)
k.max <- 15
data <- iris.scaled
sil <- rep(0, k.max)

# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(data, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  sil[i] <- mean(ss[, 3])
}

# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)

require(cluster)
fviz_nbclust(iris.scaled, kmeans, method = "silhouette")
#output shows 2 clusters needed.




#Gap statistic method

# Compute gap statistic
library(cluster)
set.seed(123)
gap_stat <- clusGap(iris.scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

# Print the result
print(gap_stat, method = "firstmax")

# Base plot of gap statistic
plot(gap_stat, frame = FALSE, xlab = "Number of clusters k")
abline(v = 3, lty = 2)



#http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning







            #Compute only an index of interest



library("NbClust")
set.seed(123)
res.nb <- NbClust(iris.scaled, distance = "euclidean",
                  min.nc = 2, max.nc = 10, 
                  method = "complete", index ="gap") 
res.nb # print the results
