library(shiny)
library(shinycssloaders)

shinyUI(fluidPage(
  titlePanel("kanyAI"),
  sidebarLayout(
    sidebarPanel(
      helpText("A long short-term memory (LSTM)-based recurrent neural network (RNN) trained on Kanye West lyrics using ",
               a("Keras for R.", href = "https://keras.rstudio.com/", target="_blank"),
               "",
               "Higher temperatures generate predictions with more 'creativity'.
               Viewer discretion is advised."),
      hr(),
      sliderInput("nchar",
                  label = "Output length (characters)",
                  min = 100, max = 1000, value = 500),
      sliderInput("temp",
                  label = "Temperature",
                  min = 0.01, max = 1.5, value = 0.2),
      actionButton("go", "Go"),
      hr(),
      helpText("Scripts available here.")
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