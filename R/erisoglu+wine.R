# Load the required libraries
library(fossil)
library(cluster)

# Load the Ruspini dataset
data(ruspini)
data <- ruspini

# Calculate Coefficient of Variation for Each Attribute
cv <- function(x) {
  return (sd(x) / mean(x))
}
cv_values <- sapply(data, cv)
attribute1 <- names(data)[which.max(cv_values)]

# Calculate Correlation Coefficient with the Attribute Having the Highest Coefficient of Variation
correlation_values <- sapply(data, function(x) {
  cor(x, data[[attribute1]])
})
attribute2 <- names(data)[which.min(correlation_values)]

# Compute the Mean of the Selected Attributes
mean_values <- colMeans(data[, c(attribute1, attribute2)])

# Function to calculate vectors
calculate_vector <- function(center, data) {
  return (sqrt(rowSums((t(t(data) - center))^2)))
}

K <- 4  # Number of clusters
cluster_centers <- list(mean_values)
data_vectors <- calculate_vector(mean_values, data[, c(attribute1, attribute2)])
initial_center <- data[which.max(data_vectors), c(attribute1, attribute2)]

# Add initial center to the cluster centers list
cluster_centers <- list(initial_center)

# Iterate to find the next cluster centers
for (k in 2:K) {
  distances <- lapply(cluster_centers, function(center) {
    center <- as.numeric(center)  # Ensure center is numeric
    calculate_vector(center, data[, c(attribute1, attribute2)])
  })
  distances <- do.call(cbind, distances)  # Combine distances into a matrix
  sum_distances <- rowSums(distances)
  next_center <- data[which.max(sum_distances), c(attribute1, attribute2)]
  cluster_centers <- append(cluster_centers, list(next_center))
}

# Function to assign clusters to data points
assign_cluster <- function(data, centers) {
  distances <- lapply(centers, function(center) {
    center <- as.numeric(center)  # Ensure center is numeric
    calculate_vector(center, data)
  })
  distances <- do.call(cbind, distances)  # Combine distances into a matrix
  clusters <- apply(distances, 1, which.min)
  return (clusters)
}

clusters <- assign_cluster(data[, c(attribute1, attribute2)], cluster_centers)

# Add Cluster Information to the Original Dataset
data$Cluster <- clusters

# Calculate mean values of each cluster
cluster_means <- aggregate(. ~ Cluster, data = data, mean)

# Using cluster means as initial centroids for k-means
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(data[, -3], centers = cluster_means[, -1])

# Print the results
print(kmeans_result$centers)
print(table(kmeans_result$cluster))

# Add cluster assignments to the original dataset
data$Cluster <- kmeans_result$cluster

# Since Ruspini dataset does not have true labels, we skip the Rand Index calculation

# Display the first few rows of the updated dataset
print(head(data))

#====================================================
# Load the required libraries
library(fossil)
library(cluster)

# Download and load the Wine dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine <- read.csv(url, header = FALSE)
names(wine) <- c("Type", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash", 
                 "Magnesium", "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols", 
                 "Proanthocyanins", "Color_Intensity", "Hue", 
                 "OD280_OD315_of_Diluted_Wines", "Proline")

data <- wine[, -1]  # Exclude the wine type column for clustering

# Min-Max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data_normalized <- as.data.frame(lapply(data, normalize))

# Calculate Coefficient of Variation for Each Attribute
cv <- function(x) {
  return (sd(x) / mean(x))
}
cv_values <- sapply(data_normalized, cv)
attribute1 <- names(data_normalized)[which.max(cv_values)]

# Calculate Correlation Coefficient with the Attribute Having the Highest Coefficient of Variation
correlation_values <- sapply(data_normalized, function(x) {
  cor(x, data_normalized[[attribute1]])
})
attribute2 <- names(data_normalized)[which.min(correlation_values)]

# Compute the Mean of the Selected Attributes
mean_values <- colMeans(data_normalized[, c(attribute1, attribute2)])

# Function to calculate vectors
calculate_vector <- function(center, data) {
  return (sqrt(rowSums((t(t(data) - center))^2)))
}

K <- 3  # Number of clusters
cluster_centers <- list(mean_values)
data_vectors <- calculate_vector(mean_values, data_normalized[, c(attribute1, attribute2)])
initial_center <- data_normalized[which.max(data_vectors), c(attribute1, attribute2)]

# Add initial center to the cluster centers list
cluster_centers <- list(initial_center)

# Iterate to find the next cluster centers
for (k in 2:K) {
  distances <- lapply(cluster_centers, function(center) {
    center <- as.numeric(center)  # Ensure center is numeric
    calculate_vector(center, data_normalized[, c(attribute1, attribute2)])
  })
  distances <- do.call(cbind, distances)  # Combine distances into a matrix
  sum_distances <- rowSums(distances)
  next_center <- data_normalized[which.max(sum_distances), c(attribute1, attribute2)]
  cluster_centers <- append(cluster_centers, list(next_center))
}

# Function to assign clusters to data points
assign_cluster <- function(data, centers) {
  distances <- lapply(centers, function(center) {
    center <- as.numeric(center)  # Ensure center is numeric
    calculate_vector(center, data)
  })
  distances <- do.call(cbind, distances)  # Combine distances into a matrix
  clusters <- apply(distances, 1, which.min)
  return (clusters)
}

clusters <- assign_cluster(data_normalized[, c(attribute1, attribute2)], cluster_centers)

# Add Cluster Information to the Normalized Dataset
data_normalized$Cluster <- clusters

# Calculate mean values of each cluster using normalized data
cluster_means_normalized <- aggregate(. ~ Cluster, data = data_normalized, mean)

# Using cluster means as initial centroids for k-means
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(data_normalized[, -ncol(data_normalized)], centers = cluster_means_normalized[, -ncol(cluster_means_normalized)])

# Print the results
print(kmeans_result$centers)
print(table(kmeans_result$cluster))

# Add cluster assignments to the original dataset
data$Cluster <- kmeans_result$cluster

# Evaluate clustering using Rand Index
true_labels <- as.integer(wine$Type)  # Convert wine types to numeric
predicted_labels <- data$Cluster

rand_index <- rand.index(true_labels, predicted_labels)
cat("Rand Index:", rand_index, "\n")

# Display the first few rows of the updated dataset
print(head(data))
