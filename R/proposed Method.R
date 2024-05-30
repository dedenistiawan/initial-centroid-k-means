# Load the required libraries
library(fossil)
library(ggplot2)

# Load the Iris dataset
data(iris)
data <- iris[, -5]  # Exclude the species column for clustering

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

K <- 3  # Number of clusters
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
iris$Cluster <- clusters

# Calculate mean values of each cluster
cluster_means <- aggregate(. ~ Cluster, data = iris[, -5], mean)

# Using cluster means as initial centroids for k-means
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(data, centers = cluster_means[, -1])

# Print the results
print(kmeans_result$centers)
print(table(kmeans_result$cluster))

# Add cluster assignments to the original dataset
iris$Cluster <- kmeans_result$cluster

# Evaluate clustering using Rand Index
true_labels <- as.integer(iris$Species)  # Convert species to numeric
predicted_labels <- iris$Cluster

rand_index <- rand.index(true_labels, predicted_labels)
cat("Rand Index:", rand_index, "\n")

# Display the first few rows of the updated dataset
print(head(iris))

# Determine initial cluster center
initial_centers <- do.call(rbind, cluster_centers)
colnames(initial_centers) <- c(attribute1, attribute2)

# Add labels to initial cluster centers
initial_centers_df <- as.data.frame(initial_centers)
initial_centers_df$label <- paste0("C", 1:nrow(initial_centers_df))

# Create scatter plot using attribute1 and attribute2 with clusters and initial cluster center
ggplot(iris, aes_string(x = attribute1, y = attribute2, color = "factor(Cluster)")) +
  geom_point(size = 3) +
  geom_point(data = initial_centers_df, aes_string(x = attribute1, y = attribute2), color = "red", size = 5, shape = 4) +
  geom_segment(data = initial_centers_df, aes(x = initial_centers_df[, 1], y = initial_centers_df[, 2], xend = c(initial_centers_df[-1, 1], initial_centers_df[1, 1]), yend = c(initial_centers_df[-1, 2], initial_centers_df[1, 2])), color = "blue") +
  geom_text(data = initial_centers_df, aes_string(x = attribute1, y = attribute2, label = "label"), vjust = -1, color = "red") +
  labs(title = "Cluster Plot of Iris Dataset with Initial Cluster Centers",
       x = attribute1,
       y = attribute2,
       color = "Cluster") +
  theme_minimal()
