library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Season summary statistics in the Premier League era"),

  sidebarLayout(
    # UI
    sidebarPanel(
      selectInput("ycol", "Choose a statistic:", 
                  choices = c("Points" = "Pts",
                              "Goals scored" = "gf",
                              "Goals conceded" = "ga",
                              "Goal difference" = "gd",
                              "League position" = "Pos")),
      sliderInput("seasons", "Choose season range:",
                  min = 1992, max = 2016, value = c(1992, 2016), sep = ""),
      uiOutput('team'),
      uiOutput('draw_mean'),
      uiOutput('per_game'),
      hr(),
      helpText(strong("Author: ",
               a("Joe Gallagher", href = "https://jogall.github.io", target="_blank"))),
      helpText("Data from",
               a("'engsoccerdata'", href = "https://github.com/jalapic/engsoccerdata", target="_blank"),
               "package for R; code for app available",
               a("here", href = "https://github.com/JoGall/shiny-apps/blob/master/EPL_season_stats", target="_blank"))
    ),
    
    # display plot
    mainPanel(
      plotOutput("plot")
    )
  )
))