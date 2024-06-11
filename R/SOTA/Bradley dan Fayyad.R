# Load required libraries
library(datasets)
library(stats)
library(fpc)
library(mclust)

# Load iris dataset
data(iris)
X <- iris[, -5]  # remove the species column for clustering

# Refinement Algorithm function
refinement_algorithm <- function(X, k, sub_sample_size=50, n_sub_samples=10) {
  set.seed(42)
  
  # Step 1: Build a set of small random sub-samples of the data
  sub_samples <- lapply(1:n_sub_samples, function(i) X[sample(1:nrow(X), sub_sample_size, replace=FALSE), ])
  
  # Step 2: Cluster data in each sub-sample by K-Means
  sub_sample_centroids <- lapply(sub_samples, function(sub_sample) {
    kmeans(sub_sample, centers=k, nstart=1)$centers
  })
  
  # Step 3: Cluster all centroids of all sub-samples by K-Means
  all_centroids <- do.call(rbind, sub_sample_centroids)
  kmeans_final <- kmeans(all_centroids, centers=k, nstart=10)
  
  # Step 4: Use the centroids of the final clusters as the initial centers for clustering the original data
  initial_centroids <- kmeans_final$centers
  return(initial_centroids)
}

# Define number of clusters
k <- 3

# Get initial centroids using Refinement Algorithm
initial_centroids <- refinement_algorithm(X, k)
print("Initial Centroids:")
print(initial_centroids)

# Fit K-means using the initial centroids
set.seed(42)
kmeans_result <- kmeans(X, centers = initial_centroids, nstart = 1)
print("K-means Clustering Result:")
print(kmeans_result)

# Convert species to numerical labels for ARI calculation
y_numeric <- as.numeric(factor(iris$Species))

# Calculate Adjusted Rand Index (ARI)
ari_score <- adjustedRandIndex(y_numeric, kmeans_result$cluster)
print("Adjusted Rand Index:")
print(ari_score)
