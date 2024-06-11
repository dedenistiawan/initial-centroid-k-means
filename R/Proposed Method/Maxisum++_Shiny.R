# Load necessary libraries
library(shiny)
library(ggplot2)
library(fossil)
library(mclust)

# Define the UI
ui <- fluidPage(
  titlePanel("Iris Clustering Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h3("Clustering Parameters"),
      numericInput("numClusters", "Number of Clusters:", value = 3, min = 2, max = 10, step = 1),
      actionButton("clusterBtn", "Run Clustering")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("irisData")),
        tabPanel("Cluster Plot", plotOutput("clusterPlot")),
        tabPanel("Cluster Centers", tableOutput("clusterCenters")),
        tabPanel("Rand Index", verbatimTextOutput("randIndexOutput")),
        tabPanel("Adjusted Rand Index", verbatimTextOutput("adjustedRandIndexOutput"))
      )
    )
  )
)

# Define the server function
server <- function(input, output) {
  observeEvent(input$clusterBtn, {
    # Load the Iris dataset
    data(iris)
    data <- iris[, -5]  # Exclude the species column for clustering
    
    # Min-Max Normalization Function
    normalize <- function(x) {
      return ((x - min(x)) / (max(x) - min(x)))
    }
    
    # Apply Min-Max Normalization to the Data
    data <- as.data.frame(lapply(data, normalize))
    
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
    
    K <- input$numClusters  # Number of clusters
    cluster_centers <- list(mean_values)
    data_vectors <- calculate_vector(mean_values, data[, c(attribute1, attribute2)])
    initial_center <- data[which.max(data_vectors), c(attribute1, attribute2)]
    
    # Add initial center to the cluster centers list
    cluster_centers <- list(initial_center)
    
    # K-means++ initialization
    set.seed(123)  # For reproducibility
    distances <- rep(Inf, nrow(data))
    for (k in 2:K) {
      for (i in 1:nrow(data)) {
        point <- data[i, c(attribute1, attribute2)]
        min_dist <- min(sapply(cluster_centers, function(center) {
          center <- as.numeric(center)  # Ensure center is numeric
          calculate_vector(center, point)
        }))
        distances[i] <- min(min_dist, distances[i])
      }
      next_center <- data[which.max(distances), c(attribute1, attribute2)]
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
    kmeans_result <- kmeans(data[, -5], centers = cluster_means[, -1])
    
    # Add cluster assignments to the original dataset
    iris$Cluster <- kmeans_result$cluster
    
    # Evaluate clustering using Rand Index
    true_labels <- as.integer(iris$Species)  # Convert species to numeric
    predicted_labels <- iris$Cluster
    
    rand_index <- rand.index(true_labels, predicted_labels)
    adjusted_rand_index <- adjustedRandIndex(true_labels, predicted_labels)
    
    # Output the results
    output$irisData <- renderTable({
      head(iris)
    })
    
    output$clusterCenters <- renderTable({
      kmeans_result$centers
    })
    
    output$randIndexOutput <- renderText({
      paste("Rand Index:", rand_index)
    })
    
    output$adjustedRandIndexOutput <- renderText({
      paste("Adjusted Rand Index:", adjusted_rand_index)
    })
    
    # Determine initial cluster center
    initial_centers <- do.call(rbind, cluster_centers)
    colnames(initial_centers) <- c(attribute1, attribute2)
    
    # Add labels to initial cluster centers
    initial_centers_df <- as.data.frame(initial_centers)
    initial_centers_df$label <- paste0("C", 1:nrow(initial_centers_df))
    
    # Create scatter plot using attribute1 and attribute2 with clusters and initial cluster center
    output$clusterPlot <- renderPlot({
      ggplot(data, aes_string(x = attribute1, y = attribute2, color = "factor(Cluster)")) +
        geom_point(size = 3) +
        geom_point(data = initial_centers_df, aes_string(x = attribute1, y = attribute2), color = "red", size = 5, shape = 4) +
        geom_segment(data = initial_centers_df, aes(x = initial_centers_df[, 1], y = initial_centers_df[, 2], xend = c(initial_centers_df[-1, 1], initial_centers_df[1, 1]), yend = c(initial_centers_df[-1, 2], initial_centers_df[1, 2])), color = "blue") +
        geom_text(data = initial_centers_df, aes_string(x = attribute1, y = attribute2, label = "label"), vjust = -1, color = "red") +
        labs(title = "Cluster Plot of Iris Dataset with Initial Cluster Centers",
             x = attribute1,
             y = attribute2,
             color = "Cluster") +
        theme_minimal()
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
