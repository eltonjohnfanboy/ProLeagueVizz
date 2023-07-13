# Read libraries
library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)

# Set the working directory
# setwd("C:/Users/adars/OneDrive/Escritorio/ProjecteLolShiny/Data")

# Read data
data_lec <- read_csv("PlayerStatesTrajectory.csv")

# UI function
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Applying the "flatly" theme for a better visual appeal
  tags$head(
    tags$style(
      HTML(
        "
        .player-square {
          background-color: #EDEDED;
          border-radius: 8px;
          padding: 20px;
          text-align: center;
          font-size: 24px;
          font-weight: bold;
          margin-bottom: 20px;
        }
        .year-team-section {
          background-color: #F5F5F5;
          border-radius: 8px;
          padding: 20px;
          text-align: center;
          font-size: 18px;
          margin-bottom: 20px;
        }
        "
      )
    )
  ),
  titlePanel("League of Legends Player Stats"),
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Select Player:", choices = unique(data_lec$Player),
                  width = "100%")  # Set the width of the dropdown to 100%
    ),
    mainPanel(
      div(class = "player-square",
          textOutput("player_name")),
      div(class = "year-team-section",
          textOutput("year_team"))
    )
  )
)

# Server function
server <- function(input, output) {
  # Filter data based on selected player
  selected_player <- reactive({
    filter(data_lec, Player == input$player) %>% slice(1)
  })
  
  # Display player's name
  output$player_name <- renderText({
    input$player
  })
  
  # Display year and team information
  output$year_team <- renderText({
    player_data <- selected_player()
    paste("Year:", player_data$Year, " | Team:", player_data$Team)
  })
}

# Run the Shiny app
shinyApp(ui, server)

