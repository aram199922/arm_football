library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(reshape2)
library(DT)

arm_nt <- read_excel("ArmenianNT.xlsx")
arm_pl_league <- read_excel("ArmenianPL_League.xlsx")
nt_europe <- read_excel("NationalTeams_Europe.xlsx")
armenian_team_dat <- read_excel("ArmenianPL_Teams.xlsx")


interactive_europe <- nt_europe %>%
  mutate(LogPopulation = log(Population))

interactive_europe <- interactive_europe %>%
  mutate(LogGDPperCapita = log(GDPperCapita))

interactive_europe$NationalTeam <- trimws(interactive_europe$NationalTeam)
interactive_europe$NationalTeam <- as.character(interactive_europe$NationalTeam)

interactive_team_dat <- armenian_team_dat 

ui <- fluidPage(
  titlePanel("Interactive Dashboard for Armenian Football Analysis"),
  
  HTML("<br><p>Here you can see our dashboard, which contains interactive plots,
       which will mainly describe and visualise Armenian national football team 
       compared to other countries. As well as the same for Armenian natioanl 
       football league.</p><br>"), 
  
  navbarPage( 
              tabPanel("UEFA National Teams", 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("yVar", "Select Y Variable for National Teams", 
                                       choices = c("Won", "Drawn", "Lost", "GoalsFor", 
                                                   "GoalsAgainst", "WinPercentage", "GF_P", 
                                                   "GA_P", "AverageAttendance", "AverageAge", 
                                                   "TeamMarketValue", "LogGDPperCapita", 
                                                   "LogPopulation")),
                           checkboxGroupInput("colorVar", "Select Variables for Color Grouping", 
                                              choices = c("MostPopularSport", "PostSoviet"))
                         ), 
                         mainPanel(
                           HTML("<br><p>This dynamic scatterplot shows the correlation between the country's FIFA Ranking and different features present in the data. Perceive each point as one country. There are also color variables which can be selected to detect country by specific functionalities.
                                </p><br>"),
                           plotOutput("scatterPlot")
                         )
                          
              ), 
              
              tabPanel("National Teams of Armenia, Georgia, Azerbaijan", 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("yLineChartVar", "Select Y Variable for Line Chart", 
                                       choices = c("Matches", "Won", "Drawn", "Lost", "GoalsFor", 
                                                   "GoalsAgainst", "WinPercentage", "PPM", "GF_P", 
                                                   "GA_P", "FIFARanking", "OpponentsAvgRanking", 
                                                   "LeaguePlayers", "LeaguePlayersGamesPercent", 
                                                   "AverageAge", "MarketValue"))
                         ), 
                         mainPanel(
                           HTML("<br><p>The Line chart presents Armenian, Georgian and Azerbaijani football national teams' performance by each Season (by the Season's correlation with different variables from the dataset)</p><br>"),
                           plotOutput("lineChart")
                         )
                       )), 
              
              
              tabPanel("Armenian Premier League Teams",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("yAPLVar", "Select Y Variable for Armenian Premier League", 
                                       choices = c("Teams", "BestEuropeFinish", "Foreigners", 
                                                   "ForeignersMinutesPercent", "AverageAge", 
                                                   "PlayersPlayedNT", "MatchesPlayedNT", 
                                                   "NationalsInTop10Goalscorers", "UEFAPoints", 
                                                   "UEFARanking", "MarketValue"))
                         ), 
                         mainPanel(HTML("<br><p>The jittered scatterplot shows how different features of Armenian, Georgian, Azerbaijani National Leagues change over the years. </p><br>"),
                                   plotOutput("aplLineChart"))
                       )
                       ), 
              tabPanel("National Leagues", 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("teamFeature", "Select Team Feature", 
                                       choices = c("Points", "AvgPointsPerMatch", 
                                                   "GoalsFor", "GoalsAgainst", "GoalDifference", 
                                                   "GSForeigners", "PlayersUsed", 
                                                   "ForeignersInTheTeam", "ForeignersMinutesPercent", 
                                                   "AverageAge", "MarketValue", "Trophies", 
                                                   "PlayersPlayedNT", "MatchesPlayedNT")),
                           selectInput("seasonSelect", "Select Season", 
                                       choices = unique(interactive_team_dat$Season))
                           
                         ), 
                         mainPanel(
                           HTML("<br><p>The bar chart helps us to go deeper into the Armenian Premiere League's performance with analyzing each Team by their performance. </p><br>"),
                           plotOutput("barPlot")
                         )
                       ))
              )
              
))
  
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    data <- interactive_europe
    yVariable <- input$yVar
    colorVariable <- input$colorVar
    
    data$color <- "black" 
    
    if ("MostPopularSport" %in% colorVariable) {
      data$color[data$MostPopularSport == 0] <- "blue"
      data$color[data$MostPopularSport == 1] <- "red"
    }
    if ("PostSoviet" %in% colorVariable) {
      data$color[data$PostSoviet == 0] <- "blue"
      data$color[data$PostSoviet == 1] <- "red"
    }
    
    p <- ggplot(data, aes_string(x = "FIFAPosition", y = yVariable, color = "color")) +
      geom_point() +
      theme_minimal() +
      labs(title = 'National Team Performance by Factors', x = "FIFA Position", y = yVariable)
    
    if ("MostPopularSport" %in% colorVariable) {
      p <- p + scale_color_manual(values = c("blue" = "blue", "red" = "red"),
                                  labels = c("Not Most Popular Sport", "Most Popular Sport"))
    } else if ("PostSoviet" %in% colorVariable) {
      p <- p + scale_color_manual(values = c("blue" = "blue", "red" = "red"),
                                  labels = c("Not Post Soviet", "Post Soviet"))
    } else {
      p <- p + scale_color_identity() # Use the colors as is
    }
    
    armenia_data <- data[3, ]
    if (nrow(armenia_data) > 0) {
      p <- p + geom_text(
        aes(label = "Armenia", x = FIFAPosition, y = !!sym(yVariable)),
        data = armenia_data,
        vjust = 1.6, 
        size = 3,             
        color = "black"      
      )
    }
    p
  })
  
  output$aplLineChart <- renderPlot({
    req(input$yAPLVar)
    ggplot(arm_pl_league, aes_string(x = "Season", y = input$yAPLVar, color = "NationalLeague")) +
      geom_point() + 
      theme_minimal() +
      labs(title = "Arm/Geo/Aze National League Statistics", x = "Season", y = input$yAPLVar) + 
      geom_jitter()
  })
  
  output$lineChart <- renderPlot({
    req(input$yLineChartVar)
    ggplot(arm_nt, aes_string(x = "Year", y = input$yLineChartVar, color = "NationalTeam")) +
      geom_line() +
      theme_minimal() +
      labs(title = "Arm/Geo/Aze National Team Statistics", x = "Year", y = input$yLineChartVar) + 
      scale_x_continuous(breaks = seq(min(arm_nt$Year), max(arm_nt$Year), by = 2)) 
  })
  
  
  output$barPlot <- renderPlot({
    filtered_data <- interactive_team_dat[interactive_team_dat$Season == input$seasonSelect, ]
      ggplot(filtered_data, aes_string(x = "Team", y = input$teamFeature)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = 'Armenian League Teams Statistics for Seasons', x = "Team", y = input$teamFeature) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
}

shinyApp(ui, server)


