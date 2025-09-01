library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

red_wine <- read.csv("D:/stat 385/winequality-red.csv", sep = ";")
white_wine <- read.csv("D:/stat 385/winequality-white.csv", sep = ";")

# Combine datasets
wine_data <- bind_rows(
  mutate(red_wine, wine_type = "Red"),
  mutate(white_wine, wine_type = "White")
)
# UI
ui <- fluidPage(
  titlePanel("Wine Quality Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("wine_type", "Choose Wine Type:", choices = c("Red", "White")),
      selectInput("xvar", "Choose X-axis Variable:", choices = names(wine_data)[1:11]),
      selectInput("yvar", "Choose Y-axis Variable:", choices = names(wine_data)[1:11]),
      sliderInput("quality", "Quality Range:", min = 0, max = 10, value = c(3, 8)),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("winePlot")),
        tabPanel("Table", tableOutput("wineTable"))
      )
    )
  )
)

#server logic
server <- function(input, output, session) {
  filteredData <- reactive({
    data <- wine_data %>% filter(wine_type == input$wine_type)
    data <- data %>% filter(quality >= input$quality[1], quality <= input$quality[2])
    data
  })
  
  # Plot 
  output$winePlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes_string(x = input$xvar, y = input$yvar, color = 'wine_type')) +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Wine Quality Analysis:", input$wine_type), x = input$xvar, y = input$yvar)
  })
  
  # Table 
  output$wineTable <- renderTable({
    filteredData()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("wine_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file)
    }
  )
}

shinyApp(ui = ui, server = server)

