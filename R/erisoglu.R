# Load necessary libraries
library(datasets)
library(cluster)
library(stats)
library(mclust)

# Load the Iris dataset
data(iris)
X <- as.matrix(iris[, -5])
y <- as.numeric(iris$Species)

compute_initial_centers <- function(X, k) {
  # Step 1: Select Principal Axes
  p <- ncol(X)
  cv <- apply(X, 2, sd) / colMeans(X)
  main_axis <- which.max(cv)
  
  corr <- sapply(1:p, function(j) {
    if (j != main_axis) {
      cor(X[, main_axis], X[, j])
    } else {
      0
    }
  })
  second_axis <- which.min(abs(corr))
  
  # Step 2: Compute Mean of Data Points in Selected Axes
  mean_main <- mean(X[, main_axis])
  mean_second <- mean(X[, second_axis])
  center <- c(mean_main, mean_second)
  
  # Step 3: Distance Calculation and Initial Center Selection
  initial_centers <- c()
  distances <- apply(X, 1, function(row) {
    dist(rbind(center, row[c(main_axis, second_axis)]))
  })
  first_center <- which.max(distances)
  initial_centers <- c(initial_centers, first_center)
  
  for (r in 2:k) {
    distances_to_last_center <- apply(X, 1, function(row) {
      dist(rbind(X[initial_centers[r-1], c(main_axis, second_axis)], row[c(main_axis, second_axis)]))
    })
    total_distances <- rowSums(sapply(initial_centers, function(center) {
      apply(X, 1, function(row) {
        dist(rbind(X[center, c(main_axis, second_axis)], row[c(main_axis, second_axis)]))
      })
    }))
    next_center <- which.max(total_distances)
    initial_centers <- c(initial_centers, next_center)
  }
  
  return(X[initial_centers, ])
}

# Compute initial cluster centers for k-means
k <- 3
initial_centers <- compute_initial_centers(X, k)

# Run k-means algorithm with computed initial centers
kmeans_result <- kmeans(X, centers = initial_centers, nstart = 1)

# Evaluate the clustering
labels <- kmeans_result$cluster
accuracy <- sum(y == labels) / length(y)
rand_index <- adjustedRandIndex(y, labels)

print(paste('Accuracy:', round(accuracy, 4)))
print(paste('Adjusted Rand Index:', round(rand_index, 4)))

#======================================================
# Load necessary libraries
library(cluster)
library(MASS)
library(ggplot2)

# Load the Iris dataset
data(iris)
X <- as.matrix(iris[, -5])
true_labels <- as.integer(iris$Species)

# Function to calculate coefficient of variation
cv <- function(x) {
  sd(x) / mean(x)
}

# Function to calculate Euclidean distance
euclidean_distance <- function(x1, x2) {
  sqrt(sum((x1 - x2) ^ 2))
}

# Function to implement the proposed method
initial_centers <- function(X, k) {
  n <- nrow(X)
  p <- ncol(X)
  
  # Step 1: Calculate coefficient of variation for each variable
  cv_values <- apply(X, 2, cv)
  
  # Step 2: Select main axis (variable with maximum CV)
  main_axis <- which.max(cv_values)
  
  # Step 3: Calculate correlation matrix and select second axis (minimum correlation with main axis)
  cor_mat <- cor(X)
  second_axis <- which.min(abs(cor_mat[main_axis, -main_axis]))
  if (second_axis >= main_axis) second_axis <- second_axis + 1
  
  # Step 4: Calculate the mean of data points for the selected axes
  mean_point <- colMeans(X[, c(main_axis, second_axis)])
  
  # Step 5: Determine the initial cluster centers
  distances <- apply(X[, c(main_axis, second_axis)], 1, function(x) euclidean_distance(x, mean_point))
  centers <- matrix(NA, nrow = k, ncol = p)
  centers[1, ] <- X[which.max(distances), ]
  
  for (r in 2:k) {
    distances <- apply(X, 1, function(x) {
      sum(apply(centers[1:(r - 1), , drop = FALSE], 1, function(center) euclidean_distance(x, center)))
    })
    centers[r, ] <- X[which.max(distances), ]
  }
  
  return(centers)
}

# Number of clusters
k <- 3

# Get initial cluster centers
centers <- initial_centers(X, k)

# Perform k-means clustering using the proposed initial centers
set.seed(123)
kmeans_result <- kmeans(X, centers, nstart = 10)

# Get predicted labels
predicted_labels <- kmeans_result$cluster

# Evaluation Metrics

# Error Percentage
error_percentage <- sum(predicted_labels != true_labels) / length(true_labels) * 100

# Rand Index
rand_index <- function(true_labels, predicted_labels) {
  tab <- table(true_labels, predicted_labels)
  sum_comb <- function(n) choose(n, 2)
  n <- sum(tab)
  a <- sum(apply(tab, 1, sum_comb))
  b <- sum(apply(tab, 2, sum_comb))
  c <- sum_comb(n)
  (a + b - sum_comb(nrow(tab))) / c
}
rand_index_value <- rand_index(true_labels, predicted_labels)

# Wilks' Lambda
wilks_lambda <- function(X, predicted_labels) {
  group_means <- aggregate(X, by = list(predicted_labels), FUN = mean)
  within_ss <- sum(apply(X, 1, function(x) min(colSums((t(group_means[, -1]) - x) ^ 2))))
  total_ss <- sum(apply(X, 1, function(x) sum((x - colMeans(X)) ^ 2)))
  within_ss / total_ss
}
wilks_lambda_value <- wilks_lambda(X, predicted_labels)

# Print Evaluation Results
cat("Error Percentage:", error_percentage, "%\n")
cat("Rand Index:", rand_index_value, "\n")
cat("Wilks' Lambda:", wilks_lambda_value, "\n")

#============================================
# Load necessary libraries
library(cluster)
library(MASS)
library(ggplot2)

# Load the Iris dataset
data(iris)
X <- as.matrix(iris[, -5])
true_labels <- as.integer(iris$Species)

# Function to calculate coefficient of variation
cv <- function(x) {
  sd(x) / mean(x)
}

# Function to calculate Euclidean distance
euclidean_distance <- function(x1, x2) {
  sqrt(sum((x1 - x2) ^ 2))
}

# Function to implement the proposed method
initial_centers <- function(X, k) {
  n <- nrow(X)
  p <- ncol(X)
  
  # Step 1: Calculate coefficient of variation for each variable
  cv_values <- apply(X, 2, cv)
  
  # Step 2: Select main axis (variable with maximum CV)
  main_axis <- which.max(cv_values)
  
  # Step 3: Calculate correlation matrix and select second axis (minimum correlation with main axis)
  cor_mat <- cor(X)
  second_axis <- which.min(abs(cor_mat[main_axis, -main_axis]))
  if (second_axis >= main_axis) second_axis <- second_axis + 1
  
  # Step 4: Calculate the mean of data points for the selected axes
  mean_point <- colMeans(X[, c(main_axis, second_axis)])
  
  # Step 5: Determine the initial cluster centers
  distances <- apply(X[, c(main_axis, second_axis)], 1, function(x) euclidean_distance(x, mean_point))
  centers <- matrix(NA, nrow = k, ncol = p)
  centers[1, ] <- X[which.max(distances), ]
  
  for (r in 2:k) {
    distances <- apply(X, 1, function(x) {
      sum(apply(centers[1:(r - 1), , drop = FALSE], 1, function(center) euclidean_distance(x, center)))
    })
    centers[r, ] <- X[which.max(distances), ]
  }
  
  return(centers)
}

# Number of clusters
k <- 3

# Get initial cluster centers
centers <- initial_centers(X, k)

# Perform k-means clustering using the proposed initial centers
set.seed(123)
kmeans_result <- kmeans(X, centers, nstart = 10)

# Get predicted labels
predicted_labels <- kmeans_result$cluster

# Evaluation Metrics

# Error Percentage
error_percentage <- sum(predicted_labels != true_labels) / length(true_labels) * 100

# Rand Index
rand_index <- function(true_labels, predicted_labels) {
  tab <- table(true_labels, predicted_labels)
  sum_comb <- function(n) choose(n, 2)
  n <- sum(tab)
  a <- sum(apply(tab, 1, sum_comb))
  b <- sum(apply(tab, 2, sum_comb))
  c <- sum_comb(n)
  (a + b - sum_comb(nrow(tab))) / c
}
rand_index_value <- rand_index(true_labels, predicted_labels)

# Wilks' Lambda
wilks_lambda <- function(X, predicted_labels) {
  group_means <- aggregate(X, by = list(predicted_labels), FUN = mean)
  within_ss <- sum(apply(X, 1, function(x) min(colSums((t(group_means[, -1]) - x) ^ 2))))
  total_ss <- sum(apply(X, 1, function(x) sum((x - colMeans(X)) ^ 2)))
  within_ss / total_ss
}
wilks_lambda_value <- wilks_lambda(X, predicted_labels)

# Print Evaluation Results
cat("Error Percentage:", error_percentage, "%\n")
cat("Rand Index:", rand_index_value, "\n")
cat("Wilks' Lambda:", wilks_lambda_value, "\n")

# Debugging: Print intermediate values
print("Initial Centers:")
print(centers)

print("Cluster Centers from k-means:")
print(kmeans_result$centers)

print("Predicted Labels:")
print(predicted_labels)

print("True Labels:")
print(true_labels)
