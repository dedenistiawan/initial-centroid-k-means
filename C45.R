library(shiny)
library(C50)
library(caret)
library(partykit)
library(readxl)

# Define UI
ui <- fluidPage(
  titlePanel("C4.5 Decision Tree with Custom Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File",
                accept = c(".xls", ".xlsx")),
      checkboxInput("header", "Header", TRUE),
      uiOutput("selectLabel"),
      numericInput("trainRatio", "Training Set Ratio:", 0.7, min = 0.5, max = 0.9, step = 0.1),
      actionButton("trainButton", "Train Model"),
      uiOutput("customInputs"),
      actionButton("predictButton", "Predict Custom Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tree Plot", plotOutput("treePlot")),
        tabPanel("Model Summary", verbatimTextOutput("modelSummary")),
        tabPanel("Confusion Matrix", verbatimTextOutput("confMatrix")),
        tabPanel("Custom Prediction", verbatimTextOutput("customPrediction"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to read the dataset
  dataset <- reactive({
    req(input$file)
    read_excel(input$file$datapath, col_names = input$header)
  })
  
  # Observe the dataset and update the label select input
  observe({
    data <- dataset()
    updateSelectInput(session, "label", "Select Label Column:", choices = colnames(data))
  })
  
  # Render the select label UI element
  output$selectLabel <- renderUI({
    req(input$file)
    selectInput("label", "Select Label Column:", choices = NULL)
  })
  
  # Reactive expression to train the model
  trainModel <- eventReactive(input$trainButton, {
    data <- dataset()
    targetCol <- input$label
    
    if (is.null(targetCol) || !(targetCol %in% colnames(data))) {
      stop("Please select a valid label column.")
    }
    
    data[[targetCol]] <- as.factor(data[[targetCol]])
    colnames(data)[colnames(data) == targetCol] <- "Target"
    
    # Split dataset into training and testing sets
    set.seed(123)
    trainIndex <- createDataPartition(data$Target, p = input$trainRatio, list = FALSE)
    trainData <- data[trainIndex, ]
    testData <- data[-trainIndex, ]
    
    # Create C4.5 model using C50 package
    model <- C5.0(Target ~ ., data = trainData)
    
    # Predict results on the testing set
    predictions <- predict(model, testData)
    
    # Evaluate model performance
    confMatrix <- confusionMatrix(predictions, testData$Target)
    
    list(model = model, confMatrix = confMatrix, attributes = setdiff(colnames(data), "Target"))
  })
  
  # Observe the train button and render custom input UI elements
  observeEvent(input$trainButton, {
    modelData <- trainModel()
    if (!is.null(modelData)) {
      output$customInputs <- renderUI({
        lapply(modelData$attributes, function(attr) {
          numericInput(attr, paste("Enter value for", attr), value = 0)
        })
      })
    }
  })
  
  # Render the tree plot
  output$treePlot <- renderPlot({
    modelData <- trainModel()
    if (!is.null(modelData)) {
      party_model <- as.party(modelData$model)
      plot(party_model)
    }
  })
  
  # Render the model summary
  output$modelSummary <- renderPrint({
    modelData <- trainModel()
    if (!is.null(modelData)) {
      summary(modelData$model)
    }
  })
  
  # Render the confusion matrix
  output$confMatrix <- renderPrint({
    modelData <- trainModel()
    if (!is.null(modelData)) {
      modelData$confMatrix
    }
  })
  
  # Render the custom prediction
  output$customPrediction <- renderPrint({
    req(input$predictButton)
    modelData <- trainModel()
    if (!is.null(modelData)) {
      newdata <- data.frame(t(sapply(modelData$attributes, function(attr) input[[attr]])))
      colnames(newdata) <- modelData$attributes
      prediction <- predict(modelData$model, newdata)
      prediction
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
