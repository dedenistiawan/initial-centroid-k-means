# Load necessary libraries
library(shiny)
library(ggplot2)
library(fossil)
library(mclust)
library(readxl)
library(writexl)

# Define the UI
ui <- fluidPage(
  titlePanel("Iris Clustering Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h3("Clustering Parameters"),
      fileInput("file1", "Choose Excel File", accept = c(".xls", ".xlsx")),
      uiOutput("labelColumnUI"),  # Dynamic UI for label column selection
      numericInput("numClusters", "Number of Clusters:", value = 3, min = 2, max = 10, step = 1),
      actionButton("clusterBtn", "Run Clustering"),
      downloadButton("downloadData", "Download Clustered Data"),
      downloadButton("downloadPlot", "Download Cluster Plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("dataTable")),
        tabPanel("Cluster Plot", plotOutput("clusterPlot")),
        tabPanel("Cluster Centers", tableOutput("clusterCenters")),
        tabPanel("Rand Index", verbatimTextOutput("randIndexOutput")),
        tabPanel("Adjusted Rand Index", verbatimTextOutput("adjustedRandIndexOutput"))
      )
    )
  )
)

# Define the server function
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  clustered_data <- reactiveVal(NULL)
  cluster_plot <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    req(input$file1)
    
    # Read the uploaded file
    df <- read_excel(input$file1$datapath)
    
    # Update reactive value
    data(df)
    
    # Update the UI for label column selection
    output$labelColumnUI <- renderUI({
      selectInput("labelColumn", "Select Label Column (Optional):", choices = c("None", names(df)), selected = "None")
    })
  })
  
  observeEvent(input$clusterBtn, {
    req(data())
    
    df <- data()
    
    # Remove non-numeric columns
    numeric_columns <- df[sapply(df, is.numeric)]
    
    # Check if data has at least 2 numeric columns
    if (ncol(numeric_columns) < 2) {
      showNotification("Dataset must have at least 2 numeric columns", type = "error")
      return()
    }
    
    # Min-Max Normalization Function
    normalize <- function(x) {
      return ((x - min(x)) / (max(x) - min(x)))
    }
    
    # Apply Min-Max Normalization to the Data
    numeric_data <- as.data.frame(lapply(numeric_columns, normalize))
    
    # Calculate Coefficient of Variation for Each Attribute
    cv <- function(x) {
      return (sd(x) / mean(x))
    }
    cv_values <- sapply(numeric_data, cv)
    attribute1 <- names(numeric_data)[which.max(cv_values)]
    
    # Calculate Correlation Coefficient with the Attribute Having the Highest Coefficient of Variation
    correlation_values <- sapply(numeric_data, function(x) {
      cor(x, numeric_data[[attribute1]])
    })
    attribute2 <- names(numeric_data)[which.min(correlation_values)]
    
    # Compute the Mean of the Selected Attributes
    mean_values <- colMeans(numeric_data[, c(attribute1, attribute2)])
    
    # Function to calculate vectors
    calculate_vector <- function(center, data) {
      return (sqrt(rowSums((t(t(data) - center))^2)))
    }
    
    K <- input$numClusters  # Number of clusters
    cluster_centers <- list(mean_values)
    data_vectors <- calculate_vector(mean_values, numeric_data[, c(attribute1, attribute2)])
    initial_center <- numeric_data[which.max(data_vectors), c(attribute1, attribute2)]
    
    # Add initial center to the cluster centers list
    cluster_centers <- list(initial_center)
    
    # K-means++ initialization
    set.seed(123)  # For reproducibility
    distances <- rep(Inf, nrow(numeric_data))
    for (k in 2:K) {
      for (i in 1:nrow(numeric_data)) {
        point <- numeric_data[i, c(attribute1, attribute2)]
        min_dist <- min(sapply(cluster_centers, function(center) {
          center <- as.numeric(center)  # Ensure center is numeric
          calculate_vector(center, point)
        }))
        distances[i] <- min(min_dist, distances[i])
      }
      next_center <- numeric_data[which.max(distances), c(attribute1, attribute2)]
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
    
    clusters <- assign_cluster(numeric_data[, c(attribute1, attribute2)], cluster_centers)
    
    # Add Cluster Information to the Original Dataset
    numeric_data$Cluster <- clusters
    df$Cluster <- clusters
    
    # Calculate mean values of each cluster
    cluster_means <- aggregate(. ~ Cluster, data = numeric_data, mean)
    
    # Using cluster means as initial centroids for k-means
    set.seed(123)  # For reproducibility
    kmeans_result <- kmeans(numeric_data[, -ncol(numeric_data)], centers = cluster_means[, -1])
    
    # Add cluster assignments to the original dataset
    df$Cluster <- kmeans_result$cluster
    clustered_data(df)
    
    # Evaluate clustering using Rand Index and Adjusted Rand Index if label column is selected
    if (input$labelColumn != "None") {
      true_labels <- as.integer(as.factor(df[[input$labelColumn]]))  # Convert labels to numeric
      predicted_labels <- df$Cluster
      rand_index <- rand.index(true_labels, predicted_labels)
      adjusted_rand_index <- adjustedRandIndex(true_labels, predicted_labels)
    } else {
      rand_index <- NA
      adjusted_rand_index <- NA
    }
    
    # Output the results
    output$dataTable <- renderTable({
      head(df)
    })
    
    output$clusterCenters <- renderTable({
      kmeans_result$centers
    })
    
    output$randIndexOutput <- renderText({
      paste("Rand Index:", ifelse(is.na(rand_index), "N/A", rand_index))
    })
    
    output$adjustedRandIndexOutput <- renderText({
      paste("Adjusted Rand Index:", ifelse(is.na(adjusted_rand_index), "N/A", adjusted_rand_index))
    })
    
    # Determine initial cluster center
    initial_centers <- do.call(rbind, cluster_centers)
    colnames(initial_centers) <- c(attribute1, attribute2)
    
    # Add labels to initial cluster centers
    initial_centers_df <- as.data.frame(initial_centers)
    initial_centers_df$label <- paste0("C", 1:nrow(initial_centers_df))
    
    # Create scatter plot using attribute1 and attribute2 with clusters and initial cluster center
    p <- ggplot(numeric_data, aes_string(x = attribute1, y = attribute2, color = "factor(Cluster)")) +
      geom_point(size = 3) +
      geom_point(data = initial_centers_df, aes_string(x = attribute1, y = attribute2), color = "red", size = 5, shape = 4) +
      geom_segment(data = initial_centers_df, aes(x = initial_centers_df[, 1], y = initial_centers_df[, 2], xend = c(initial_centers_df[-1, 1], initial_centers_df[1, 1]), yend = c(initial_centers_df[-1, 2], initial_centers_df[1, 2])), color = "blue") +
      geom_text(data = initial_centers_df, aes_string(x = attribute1, y = attribute2, label = "label"), vjust = -1, color = "red") +
      labs(title = "Cluster Plot of Uploaded Dataset with Initial Cluster Centers",
           x = attribute1,
           y = attribute2,
           color = "Cluster") +
      theme_minimal()
    
    cluster_plot(p)
    
    output$clusterPlot <- renderPlot({
      p
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("clustered_data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(clustered_data(), path = file)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("cluster_plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = cluster_plot(), device = "png")
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
