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

# Maximin Initialization function
maximin_initialization <- function(X, k) {
  set.seed(42)
  
  # Step 1: Randomly select the first centroid
  initial_index <- sample(1:nrow(X), 1)
  centroids <- X[initial_index, , drop = FALSE]
  
  # Step 2: Iteratively select remaining centroids
  for (i in 2:k) {
    # Calculate minimum distance from each point to the nearest centroid
    min_distances <- apply(X, 1, function(point) {
      min(apply(centroids, 1, function(centroid) {
        euclidean_distance(point, centroid)
      }))
    })
    
    # Select the point with the maximum of these minimum distances as the next centroid
    next_centroid_index <- which.max(min_distances)
    next_centroid <- X[next_centroid_index, , drop = FALSE]
    
    # Add the selected centroid to the set of centroids
    centroids <- rbind(centroids, next_centroid)
  }
  
  return(centroids)
}

# Define number of clusters
k <- 3

# Get initial centroids using Maximin Initialization
initial_centroids <- maximin_initialization(X, k)
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
