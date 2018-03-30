
library(shiny)
library(shinycssloaders)

# for spinners 2-3 match the background color of wellPanel
options(spinner.color.background="#F5F5F5")

ui <- fluidPage(
  wellPanel(
    tags$b("This example shows the loading spinner whilst the plot is loading and hides the spinner when the plot is not shown."), 
    br(),br(),
    tags$ul(
      tags$li("You can use it to wrap any kind of output."),
      tags$li("To see what happens on recalculation, click the recalculate button"),
      tags$li("To see what happens if no output should be generated, check off 'Show plots'.")
    ),
    actionButton("go","Re-draw plots")
  ),
  mainPanel(
      fluidRow(
        wellPanel(
          tags$b("With spinner:"),
            withSpinner(plotOutput(paste0("plot",4)),type=4)
          )
      )
    )
)

server <- function(input, output,session) {
  for (i in 1:8) {
    output[[paste0("nospin_plot",i)]] <- output[[paste0("plot",i)]] <- renderPlot({
      input$go
      Sys.sleep(2) # just for demo so you can enjoy the animation
      plot(
        x = runif(1e4), y = runif(1e4)
      )
    })
  }
}

shinyApp(ui = ui, server = server)