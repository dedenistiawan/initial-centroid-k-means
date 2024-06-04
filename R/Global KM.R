# Load required libraries
library(datasets)
library(stats)
library(fpc)
library(mclust)

# Load iris dataset
data(iris)
X <- iris[, -5]  # remove the species column for clustering

# Function to calculate Euclidean distance
euclidean_distance <- function(a, b) {
  sqrt(sum((a - b) ^ 2))
}

# Function to run k-means with a given set of initial centers
run_kmeans <- function(X, centers) {
  kmeans(X, centers = centers, nstart = 1)
}

# Function to check if centroids are distinct
are_centroids_distinct <- function(centroids) {
  dist_matrix <- as.matrix(dist(centroids))
  return(all(dist_matrix[lower.tri(dist_matrix)] > 0))
}

# Global K-Means Initialization function
global_kmeans_initialization <- function(X, k) {
  set.seed(42)
  
  # Step 1: Start with the first centroid (the mean of the data)
  centroids <- colMeans(X)
  centroids <- matrix(centroids, nrow = 1)
  colnames(centroids) <- colnames(X)
  
  # Step 2: Iteratively add centroids
  for (i in 2:k) {
    best_centroid <- NULL
    best_error <- Inf
    
    # Try adding each data point as the new centroid
    for (j in 1:nrow(X)) {
      candidate_centroids <- rbind(centroids, X[j, , drop = FALSE])
      colnames(candidate_centroids) <- colnames(X)
      
      if (are_centroids_distinct(candidate_centroids)) {
        kmeans_result <- run_kmeans(X, candidate_centroids)
        if (kmeans_result$tot.withinss < best_error) {
          best_centroid <- X[j, , drop = FALSE]
          best_error <- kmeans_result$tot.withinss
        }
      }
    }
    
    # Add the best candidate centroid to the list of centroids
    centroids <- rbind(centroids, best_centroid)
    colnames(centroids) <- colnames(X)  # Ensure column names match the dataset
  }
  
  return(centroids)
}

# Define number of clusters
k <- 3

# Get initial centroids using Global K-Means Initialization
initial_centroids <- global_kmeans_initialization(X, k)
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
