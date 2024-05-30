# Install necessary packages if not already installed
if (!require(ClusterR)) {
  install.packages("ClusterR")
}
if (!require(GA)) {
  install.packages("GA")
}
if (!require(datasets)) {
  install.packages("datasets")
}

# Load the libraries
library(ClusterR)
library(foreach)
library(iterators)
library(GA)
library(datasets)
library(ggplot2)

# Load the Iris dataset
data(iris)

# Remove the Species column
iris_data <- iris[, -5]

# Define the fitness function
fitness_function <- function(centroids) {
  centroids_matrix <- matrix(centroids, ncol = ncol(iris_data), byrow = TRUE)
  clusters <- apply(iris_data, 1, function(x) {
    which.min(colSums((t(centroids_matrix) - x)^2))
  })
  wcss <- sum(sapply(unique(clusters), function(k) {
    sum(colSums((iris_data[clusters == k, ] - centroids_matrix[k, ])^2))
  }))
  return(-wcss)  # GA maximizes fitness, so return negative WCSS
}

# Number of clusters
num_clusters <- 3
num_features <- ncol(iris_data)

# Perform k-means clustering with random initialization
set.seed(123)
kmeans_result_random <- kmeans(iris_data, centers = num_clusters, nstart = 20)
print(kmeans_result_random)

# Perform k-means clustering with k-means++ initialization
kmeans_result_kmeanspp <- KMeans_rcpp(iris_data, clusters = num_clusters, num_init = 20, initializer = 'kmeans++')
print(kmeans_result_kmeanspp)

# Set the parameters for GA
ga <- ga(type = "real-valued",
         fitness = fitness_function,
         lower = rep(apply(iris_data, 2, min), num_clusters),
         upper = rep(apply(iris_data, 2, max), num_clusters),
         popSize = 50,
         maxiter = 100,
         run = 50)

# Extract the best centroids from GA
best_centroids <- matrix(ga@solution, ncol = num_features, byrow = TRUE)

# Perform k-means clustering with GA-initialized centroids
kmeans_result_ga <- kmeans(iris_data, centers = best_centroids, nstart = 1)
print(kmeans_result_ga)

cat("WCSS with random initialization:", kmeans_result_random$tot.withinss, "\n")
cat("WCSS with k-means++ initialization:", kmeans_result_kmeanspp$tot.withinss, "\n")
cat("WCSS with GA initialization:", kmeans_result_ga$tot.withinss, "\n")

# Perform PCA for visualization
pca <- prcomp(iris_data, center = TRUE, scale. = TRUE)
pca_data_random <- data.frame(pca$x[, 1:2], Cluster = factor(kmeans_result_random$cluster))
pca_data_kmeanspp <- data.frame(pca$x[, 1:2], Cluster = factor(kmeans_result_kmeanspp$clusters))
pca_data_ga <- data.frame(pca$x[, 1:2], Cluster = factor(kmeans_result_ga$cluster))

# Plot the clusters with random initialization
plot_random <- ggplot(pca_data_random, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering with Random Initialization on Iris Data",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

# Plot the clusters with k-means++ initialization
plot_kmeanspp <- ggplot(pca_data_kmeanspp, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering with k-means++ Initialization on Iris Data",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

# Plot the clusters with GA initialization
plot_ga <- ggplot(pca_data_ga, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering with GA Initialization on Iris Data",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

# Print the plots
print(plot_random)
print(plot_kmeanspp)
print(plot_ga)
