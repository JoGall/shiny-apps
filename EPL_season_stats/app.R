library(shiny)
library(ggplot2)
library(dplyr)
library(engsoccerdata)

#update results database and define EPL data
england <- rbind(england, subset(england_current(), !(Date %in% england$Date & home %in% england$home)))

dat <- england %>%
  subset(tier == 1 & Season %in% 1992:2016) %>%
  dplyr::group_by(Season) %>%
  do(maketable_all(.)) %>%
  mutate(Pos = as.numeric(Pos)) %>%
  select(Season, team, gf, ga, gd, Pts, Pos)

#define ui with Bootstrap layout
ui <- fluidPage(
  #title
  titlePanel("Season statistics in the PL era"),
  #generate row with a sidebar
  sidebarLayout(      
    #define sidebar with one input
    sidebarPanel(
      selectInput("ycol", "Choose a statistic:", 
                  choices = c("Goals scored" = "gf",
                              "Goals conceded" = "ga",
                              "Goal difference" = "gd",
                              "Points" = "Pts",
                              "League position" = "Pos")),
      sliderInput("seasons", "Choose season range:",
                  min = 1992, max = 2016, value = c(1992, 2016), sep = ""),
      uiOutput('team_choices'),
      uiOutput('draw_mean'),
      hr(),
      helpText("Data from ",
               a("engsoccerdata", href = "https://github.com/jalapic/engsoccerdata", target="_blank"),
               "package | ",
               a("Source code", href = "https://github.com/JoGall/shiny-apps", target="_blank"))
    ),
    # create ggplot
    mainPanel(
      plotOutput("plot")
    )
  )
)

#define server
server <- function(input,output){
  
  #generate team choices depending on season range 
  output$team_choices = renderUI({
    tmp <- subset(dat, Season >= input$seasons[1] & Season <= input$seasons[2])
    selectInput("team", "Choose a team:", 
                choices = sort(unique(tmp$team)))
  })
  
  #determine whether to show checkbox for league averages (not applicable to 'Pos' variable)
  output$draw_mean = renderUI({
    if(input$ycol != "Pos") {
      checkboxInput("mean_line", "Show league average?", FALSE)
    }
  })
  
  #apply filters
  mainData <- reactive({
    a <- subset(dat, Season >= input$seasons[1] & Season <= input$seasons[2])
    return(a)
  })
  
  subsetData <- reactive({
    dd <- mainData()
    ss <- subset(dd, team == input$team)
    #add group variable so geom_line skips missing seasons
    idx <- c(1, diff(ss$Season))
    i2 <- c(1, which(idx != 1), nrow(ss)+1)
    ss$grp <- rep(1:length(diff(i2)), diff(i2))
    return(ss)
  })
  
  #render plot
  output$plot <- renderPlot({
    
    dd <- mainData()
    
    #initialise plot parameters
    ylabel <- ifelse(input$ycol == "gf", "Goals scored",
                     ifelse(input$ycol == "ga", "Goals conceded",
                            ifelse(input$ycol == "gd", "Goal difference",
                                   ifelse(input$ycol == "Pts", "Total points", "League position"))))
    
    if(input$ycol == "gf") ylimits <- c(15, 110)
    if(input$ycol == "ga") ylimits <- c(10, 75)
    if(input$ycol == "gd") ylimits <- c(-75, 75)
    if(input$ycol == "Pts") ylimits <- c(10, 100)
    
    #plot Pos variable
    if(input$ycol == "Pos") {
      ggplot(dd, aes(x = Season, y = Pos)) + 
        geom_point(alpha=0) +
        geom_line(data = subsetData(), aes(group = grp), col="red", lwd=1.5) +
        geom_point(data = subsetData(), aes(group = grp), col="red", size = 3) +
        scale_y_reverse(lim = c(22,1), 
                        breaks = c(1, 4, 6, 8, 10, 12, 14, 17, 20, 22)) +
        scale_x_continuous(breaks = unique(dd$Season),
                           labels = paste0(unique(dd$Season),
                                           "-",
                                           substr(unique(dd$Season)+1, 3, 4))) +
        ylab(ylabel) +
        xlab("") +
        ggtitle(input$team) +
        theme(axis.text.x = element_text(angle=90, hjust=1),
              text = element_text(size=20),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.border= element_blank(),
              axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black"))
      
    } else {
      
      #plot other continuous variables
      ggplot(dd, aes(x = Season, y = dd[,input$ycol])) +
        geom_point(size = 1.5, alpha = 0.5) +
        {if(input$mean_line) stat_summary(fun.y=mean, geom="line", lwd = 1.5, alpha = 0.5)} +
        geom_point(data = subsetData(), aes(x = Season, y = subsetData()[,input$ycol], group = grp), col="red", size = 3) +
        geom_line(data = subsetData(), aes(x = Season, y = subsetData()[,input$ycol], group = grp), col="red", lwd=1.5) +
        scale_y_continuous(lim = ylimits) +
        scale_x_continuous(breaks = unique(dd$Season),
                           labels = paste0(unique(dd$Season),
                                           "-",
                                           substr(unique(dd$Season)+1, 3, 4))) +
                                           {if(input$mean_line) stat_summary(fun.y=mean, geom="line", lwd = 1.5, alpha = 0.5)} +
        ylab(ylabel) +
        xlab("") +
        ggtitle(input$team) +
        theme(axis.text.x = element_text(angle=90, hjust=1),
              text = element_text(size=20),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.border= element_blank(),
              axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black"))
    }
  })
}

#run
shinyApp(ui, server)