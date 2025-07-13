
library(shiny)
library(dplyr)
library(stringr)

# Load n-gram datasets
trigrams <- readRDS("trigrams.rds")
bigrams <- readRDS("bigrams.rds")

predict_next_word <- function(input_phrase) {
  words <- str_split(tolower(input_phrase), "\\s+")[[1]]
  words <- words[words != ""]
  n <- length(words)

  if (n >= 2) {
    w1 <- words[n-1]
    w2 <- words[n]
    prediction <- trigrams %>%
      filter(word1 == w1, word2 == w2) %>%
      slice_max(order_by = n, n = 1) %>%
      pull(word3)
    if (length(prediction) > 0) return(prediction)
  }

  if (n >= 1) {
    w1 <- words[n]
    prediction <- bigrams %>%
      filter(word1 == w1) %>%
      slice_max(order_by = n, n = 1) %>%
      pull(word2)
    if (length(prediction) > 0) return(prediction)
  }

  return("No prediction found.")
}

ui <- fluidPage(
  titlePanel("Next Word Prediction App"),

  sidebarLayout(
    sidebarPanel(
      textInput("user_input", "Enter a phrase:", value = ""),
      actionButton("go_button", "Predict Next Word")
    ),

    mainPanel(
      h4("Predicted Next Word:"),
      verbatimTextOutput("prediction_output")
    )
  )
)

server <- function(input, output) {
  prediction <- eventReactive(input$go_button, {
    predict_next_word(input$user_input)
  })

  output$prediction_output <- renderText({
    prediction()
  })
}

shinyApp(ui = ui, server = server)
