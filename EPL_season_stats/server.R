library(shiny)
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
  select(Season, team, gf, ga, gd, Pts, Pos, GP)

shinyServer(function(input,output, session) {
  
  output$team <- renderUI({
    selectInput("teams", "Select team:", choices = sort(unique(dat$team[dat$Season %in% seq(input$seasons[1], input$seasons[2])]))
                             
  observeEvent({
    input$team
    input$seasons
    }, {
      
      # generate team choices depending on season range
      output$team <- renderUI({
        # if previous team choice not in new data, default to first team alphabetically
        prevSel <- if(!input$team %in% sort(unique(dat$team[dat$Season %in% seq(input$seasons[1], input$seasons[2])]))) sort(unique(dat$team[dat$Season %in% seq(input$seasons[1], input$seasons[2])]))[1]
        
        # Create the drop-down menu for the city selection
        selectInput("teams", "Select team:", 
                    choices = sort(unique(dat$team[dat$Season %in% seq(input$seasons[1], input$seasons[2])])),
                    selection = prevSel)
      })
  })
  
  # show checkbox to draw league averages?
  output$draw_mean = renderUI({
    if(!input$ycol %in% c("Pos", "gd")) {
      checkboxInput("mean_line", "Show league average?", FALSE)
    }
  })
  
  # show radio buttons to switch between raw / per game values?
  output$per_game = renderUI({
    if(!input$ycol %in% c("Pos", "gd")) {
      radioButtons("per_game", "Values as:",
                   c("raw" = "raw",
                     "per game" = "rel"),
                   inline = TRUE)
    }
  })
  
  # filter main data
  mainData <- reactive({
    if(is.null(input$seasons) | is.null(input$per_game)) {
      return(NULL)
    }
    
    a <- subset(dat, Season >= input$seasons[1] & Season <= input$seasons[2])
    #transform absolute values to relative (per game)
    if(input$per_game == "rel") a <- mutate(a, gf = gf / GP, ga = ga / GP, Pts = Pts / GP)
    return(a)
  })
  
  # filter team subset of data
  subsetData <- reactive({
    if(is.null(input$team) | is.null(mainData())) {
      return(NULL)
    }
    
    dd <- mainData()
    ss <- subset(dd, team == input$team)
    # add group variable so geom_line skips missing seasons
    idx <- c(1, diff(ss$Season))
    i2 <- c(1, which(idx != 1), nrow(ss)+1)
    ss$grp <- rep(1:length(diff(i2)), diff(i2))
    return(ss)
  })
  
  #render plot
  output$plot <- renderPlot({
    
    if (is.null(mainData()) | is.null(subsetData())) {
      return(NULL)
    }
    
    dd <- mainData()
    
    # plot parameters: y axis label
    ylabel <- ifelse(input$ycol == "gf", "Goals scored",
                     ifelse(input$ycol == "ga", "Goals conceded",
                            ifelse(input$ycol == "gd", "Goal difference",
                                   ifelse(input$ycol == "Pts", "Total points", "League position"))))
    
    # plot parameters: y axis limits
    if(input$per_game == "rel") {
      if(input$ycol == "gf") ylimits <- c(0.4, 2.9)
      if(input$ycol == "ga") ylimits <- c(0.3, 2.5)
      if(input$ycol == "Pts") ylimits <- c(0.2, 2.75)
    } else {
      if(input$ycol == "gf") ylimits <- c(15, 110)
      if(input$ycol == "ga") ylimits <- c(10, 105)
      if(input$ycol == "gd") ylimits <- c(-75, 75)
      if(input$ycol == "Pts") ylimits <- c(10, 100)
    }
    
    # plot Pos variable
    if(input$ycol == "Pos") {
      ggplot(dd, aes(x = Season, y = Pos)) + 
        geom_point(alpha=0) +
        geom_point(data = subsetData(), aes(group = grp), col="red", size = 3) +
        {if(nrow(subsetData()) > 1) geom_line(data = subsetData(), aes(group = grp), col="red", lwd=1.5) } +
        {if(input$seasons[1] < 1995) scale_y_reverse(lim = c(22,1), breaks = c(1, 4, 6, 8, 10, 12, 14, 17, 20, 22)) else scale_y_reverse(lim = c(20,1), breaks = c(1, 4, 6, 8, 10, 12, 14, 17, 20))} +
        scale_x_continuous(breaks = unique(dd$Season), labels = paste0(unique(dd$Season), "-", substr(unique(dd$Season)+1, 3, 4))) +
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
      # plot other continuous variables
      ggplot(dd, aes(x = Season, y = dd[,input$ycol])) +
        geom_point(size = 1.5, alpha = 0.5) +
        {if(input$mean_line) stat_summary(fun.y=mean, geom="line", lwd = 1.5, alpha = 0.5)} +
        geom_point(data = subsetData(), aes(x = Season, y = subsetData()[,input$ycol], group = grp), col="red", size = 3) +
        {if(nrow(subsetData()) > 1) geom_line(data = subsetData(), aes(x = Season, y = subsetData()[,input$ycol], group = grp), col="red", lwd=1.5) } +
        scale_y_continuous(lim = ylimits) +
        scale_x_continuous(breaks = unique(dd$Season), labels = paste0(unique(dd$Season), "-", substr(unique(dd$Season)+1, 3, 4))) +
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
              axis.line.y = element_line(color="black")
              )
    }
  })
})
