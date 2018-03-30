library(shiny)
library(shinycssloaders)

shinyUI(fluidPage(
  titlePanel("kanyAI"),
  sidebarLayout(
    sidebarPanel(
      helpText("A long short-term memory (LSTM) recurrent neural network (RNN) trained on Kanye West lyrics using",
               a("Keras and TensorFlow.", href = "https://keras.rstudio.com/", target="_blank")),
      helpText("Higher temperatures generate more 'creative' predictions."),
      helpText("Parental advisory: explicit lyrics."),
      hr(),
      sliderInput("nchar",
                  label = "Output length (characters)",
                  min = 100, max = 1000, value = 500),
      sliderInput("temp",
                  label = "Temperature",
                  min = 0.01, max = 1, value = 0.18),
      actionButton("go", "Go"),
      hr(),
      helpText("By ",
               a("Joe Gallagher", href = "https://jogall.github.io/", target="_blank"),
               "| Code available ",
               a("here.", href = "https://github.com/JoGall/shiny-apps/tree/master/kanyAI", target="_blank"))
    ),
    mainPanel(
      fluidRow(
        wellPanel(
          withSpinner(verbatimTextOutput("text1"), type = 4)
        )
      )
    )
  )
)
)