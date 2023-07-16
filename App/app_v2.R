# Read libraries
library(shiny)
library(shinythemes)
library(reshape2)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library (plotly)


# Set the working directory
setwd("C:/Users/adars/OneDrive/Escritorio/ProjecteLolShiny/Data")

# Read data
data_lec <- read_csv("PlayerStatesTrajectory.csv")
data_traj <- read_csv("PlayersTrajectoyData.csv")

# Define ui
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  "LeagueStats",
                  tabPanel("Player stats",
                           sidebarPanel(
                             tags$h3("Search Bar"),
                             selectInput("player", "Select Player:", choices = unique(data_lec$Player),
                                         width = "100%"),
                             uiOutput("yearSelection")
                           ),
                  mainPanel(
                        textOutput("player_name"),
                        textOutput("year_team")
                          ),
                  mainPanel(
                    plotlyOutput("trajectoryPlot")
                          )
                  ),
                  tabPanel("Team stats", "Under construction 👷"),
                  tabPanel("Team comparison", "Under construction 👷"),
                  tabPanel("Player similarity", "Under construction 👷")
                ) # navbar page
              ) #fluidpage

server <- function(input, output, session){
  
  selected_player <- reactive({
    filter(data_lec, Player == input$player) 
  })
  
  output$yearSelection <- renderUI({selectInput("year", "Select Year:", choices = unique(selected_player()$Year),
                                                width = "100%")})
  
  selected_data <- reactive({
    filter(data_lec, Player == input$player & Year == input$year) 
  })
  
  # Display player's name
  output$player_name <- renderText({
    player_data <- selected_data()
    paste("Official name: ", unique(player_data$Player))
  })
  
  # Display year and team information
  output$year_team <- renderText({
    player_data <- selected_data()
    mean_kda <- mean(player_data$KDA)
    paste("Year:", unique(player_data$Year), " | KDA:", mean_kda)
  })
  
  # Display trajectory plot
  
  output$trajectoryPlot <- renderPlotly({
    data_aux <- data_traj %>% filter(Player == input$player)
    dfr <- data.frame(
    name = factor(c(data_aux$`Teams trajectory`), levels = data_aux$`Teams trajectory`),
    start.date = as.Date(c(data_aux$start_date)),
    end.date = as.Date(c(data_aux$end_date))
    )
    mdfr <- melt(dfr, measure.vars = c("start.date", "end.date"))
    plot_traj <- ggplot(mdfr, aes(value, name)) + 
      geom_line(size = 6, colour = "skyblue") +
      xlab(NULL) + 
      ylab(NULL)
    ggplotly(plot_traj)
  }) 
  
}

# Run the Shiny app
shinyApp(ui, server)

