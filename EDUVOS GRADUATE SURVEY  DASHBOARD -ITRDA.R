# Load required libraries
library(shiny)
library(tidyverse)
library(data.table)

# Load Data
data <- read.csv("graduate_survey (4).csv")  # Ensure the file is in your working directory

# Filter only IT graduates
data <- data %>% filter(StudyField %in% c("IT", "Data Science", "Computer Science"))


# Function to Split Columns Properly
split_column <- function(df, col) {
  df %>%
    filter(!is.na(.data[[col]]) & .data[[col]] != "") %>%
    separate_rows(.data[[col]], sep = ";") %>%
    mutate(!!sym(col) := str_trim(.data[[col]]))
}

# Apply function to relevant columns
cols_to_split <- c("ProgLang", "Databases", "WebFramework", "Platform", "AISearch", "AITool")

# Create a copy of the dataset to store split values
split_data <- data

# Apply function column by column and store results in a list
split_results <- lapply(cols_to_split, function(col) {
  if (col %in% names(data)) {
    split_column(data, col)
  } else {
    NULL
  }
})

# Combine the results into one dataset (removing NULL values)
split_data <- bind_rows(split_results)

# Check the first few rows
print(head(split_data))

# UI
ui <- fluidPage(
  titlePanel("Eduvos IT Graduates Survey Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
        selectInput("study_field", "Select Study Field:", choices = c("IT", "Data Science", "Computer Science"), selected = "IT"),
      selectInput("category", "Select Category:", choices = cols_to_split)
    ),
    
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive Data Filtering
  filteredData <- reactive({
    req(input$study_field)
    data %>% filter(StudyField == input$study_field)
  })
  
  # Generate Bar Plot
  output$barPlot <- renderPlot({
    req(input$category)
    
    df <- filteredData() %>%
      count(.data[[input$category]], sort = TRUE) %>%
      top_n(10, n)
    
    ggplot(df, aes(x = reorder(.data[[input$category]], n), y = n, fill = .data[[input$category]])) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Top 10", input$category, "Used by IT Graduates"),
           x = input$category, y = "Count") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Make legend text readable
        axis.text = element_text(color = "black"),  # Darken axis labels
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold title
        axis.text.x = element_text(size = 12),  # Ensure x-axis labels are readable
        axis.text.y = element_text(size = 12)   # Ensure y-axis labels are readable
      ) +
      geom_text(aes(label = n), hjust = -0.2, size = 5, color = "black")  # Ensure labels appear on bars
  })
}

# Run the app
shinyApp(ui, server)
