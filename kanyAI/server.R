source("preprocess.R", local = TRUE)

shinyServer(function(input, output) {
    
  lyrics <- reactiveValues(default = 0)
  
  # generate lyrics
  lyrics <- eventReactive(input$go, {
    generateText(model, input$nchar, input$temp)
  }, ignoreNULL = FALSE
  )

  #outputs
  output$text1 <- renderText({lyrics()})
  # output$text1 <- renderText({"Hiiiiiiiiiii"})
  
  }
)
