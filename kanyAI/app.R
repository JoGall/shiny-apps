library(shiny)
library(keras)
library(stringr)
library(readr)
library(purrr)
library(tokenizers)
library(shinyjs)
library(Rtts)
source('get_next_text.R')

ui <- fluidPage(
  
  # Application title
  titlePanel("kanyAI"),
  shinyUI(tabPanel("Predict Speech",
            useShinyjs(),
            sidebarPanel(
              helpText("A long short-term memory (LSTM) recurrent neural network (RNN) trained on Kanye West lyrics using",
                       a("Keras and TensorFlow.", href = "https://keras.rstudio.com/", target="_blank")),
              helpText("Higher temperatures generate more 'creative' predictions."),
              helpText("Parental advisory: explicit lyrics."),
              hr(),
              sliderInput("nchar",
                          label = "Output length",
                          min = 100, max = 1000, value = 500),
              sliderInput("temp",
                          label = "Temperature",
                          min = 0.01, max = 1, value = 0.18),
              textInput("seed",
                        "Sentence seed (optional)",
                        placeholder = "Uhh yeah baby girl"),
              actionButton("generate", "Generate Text")
            ),
            mainPanel(
              verbatimTextOutput("next_text")
              
            )
    )
  )
)


server <- function(input, output) {
  txt <- ""

  observeEvent(input$generate, {
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    output$next_text <- renderText({
      progress <- shiny::Progress$new()
      progress$set(message = "kanyAI is thinking...", value = .5)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      isolate(
        txt <<- get_next_text(input$seed, input$nchar, input$temp)
      )
      
    })

  })
}


shinyApp(ui = ui, server = server)

