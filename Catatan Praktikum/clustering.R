iris2 <-iris
Species <- iris2$Species 
iris2$Species <- NULL

# k-Means Clustering
iris2 <-iris
Species <- iris2$Species 
iris2$Species <- NULL
kmeans.result <- kmeans(iris2, 3) #kmeans(data, n_cluster)
kmeans.result

# Validasi
table(Species, kmeans.result$cluster) # table frekuensi baris thdp. kolom

# Plotting
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)

#####################################
# k-Medoids Clustering
library(fpc)
pamk.result <- pamk(iris2)
pamk.result$nc
table(Species, pamk.result$pamobject$clustering)

layout((matrix(c(1,2),1,2)))
plot(pamk.result$pamobject)
layout(matrix(1))

#####################################
# Hierarchical Clustering
iris2 <- iris
Species <- iris2$Species
iris2$Species <- NULL

rowSample <- sample(1:nrow(iris2), 40) # opsional: sampling
irisSample <- iris2[rowSample,]

hc <- hclust(dist(irisSample), method = "ave")
plot(hc, hang = -1, labels=iris$Species[rowSample])
