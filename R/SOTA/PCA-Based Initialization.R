# Load required libraries
library(datasets)
library(stats)
library(fpc)
library(mclust)

# Load iris dataset
data(iris)
X <- iris[, -5]  # remove the species column for clustering

# Function to perform PCA-Based Initialization
pca_initialization <- function(X, k) {
  # Step 1: Perform PCA on the dataset
  pca_result <- prcomp(X, center = TRUE, scale. = TRUE)
  
  # Step 2: Project the data onto the first two principal components
  X_pca <- pca_result$x[, 1:2]
  
  # Step 3: Select the extremal points along the principal components as the initial centroids
  centroids <- NULL
  for (i in 1:k) {
    max_index <- which.max(X_pca[, i %% 2 + 1])
    min_index <- which.min(X_pca[, i %% 2 + 1])
    centroids <- rbind(centroids, X[max_index, ])
    centroids <- rbind(centroids, X[min_index, ])
    if (nrow(centroids) >= k) break
  }
  
  centroids <- centroids[1:k, ]
  return(centroids)
}

# Define number of clusters
k <- 3

# Get initial centroids using PCA-Based Initialization
initial_centroids <- pca_initialization(X, k)
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
