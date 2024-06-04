# Load required libraries
library(datasets)
library(stats)
library(fpc)
library(mclust)

# Load iris dataset
data(iris)
X <- iris[, -5]  # remove the species column for clustering

# Function to partition data along the axis with the highest variance
data_partitioning_initialization <- function(X, k) {
  set.seed(42)
  
  partition_data <- function(X, k) {
    centroids <- list()
    cells <- list(X)
    
    while (length(cells) < k) {
      # Find the cell with the highest variance along any axis
      cell_variances <- lapply(cells, function(cell) apply(cell, 2, var))
      cell_to_split_idx <- which.max(sapply(cell_variances, max))
      cell_to_split <- cells[[cell_to_split_idx]]
      cells <- cells[-cell_to_split_idx]
      
      # Split the cell along the axis with the highest variance
      split_axis <- which.max(apply(cell_to_split, 2, var))
      median_value <- median(cell_to_split[, split_axis])
      
      left_cell <- cell_to_split[cell_to_split[, split_axis] <= median_value, ]
      right_cell <- cell_to_split[cell_to_split[, split_axis] > median_value, ]
      
      cells <- c(cells, list(left_cell), list(right_cell))
    }
    
    for (cell in cells) {
      if (nrow(cell) > 0) {
        centroids <- rbind(centroids, colMeans(cell))
      }
    }
    
    return(as.matrix(centroids))
  }
  
  initial_centroids <- partition_data(X, k)
  return(initial_centroids)
}

# Define number of clusters
k <- 3

# Get initial centroids using Data Partitioning Algorithm
initial_centroids <- data_partitioning_initialization(X, k)
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
