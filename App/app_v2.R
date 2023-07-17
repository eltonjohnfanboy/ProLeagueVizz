# Read libraries
library(shiny)
library(shinythemes)
library(reshape2)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(plotly)
library(bslib)


# Set the working directory
setwd("C:/Users/adars/OneDrive/Escritorio/ProjecteLolShiny/Data")


# Read data
data_lec <- read_csv("PlayerStats.csv")
data_traj <- read_csv("PlayersTrajectoyData.csv")

# Define ui
ui <- fluidPage(
                includeCSS("C:/Users/adars/OneDrive/Escritorio/ProjecteLolShiny/www/style.css"),
                theme = shinythemes::shinytheme("slate"),
                navbarPage(
                  "LeagueStats",
                  tabPanel("Player stats",
                           sidebarPanel(
                                  tags$h3("Search Bar"),
                                  selectInput("player", "Select Player:", choices = unique(data_lec$Player),
                                              width = "100%"),
                                  uiOutput("yearSelection"),
                                  uiOutput("eventSelection")
                            ),
                  mainPanel(
                        uiOutput("player_info_box")
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
  
selected_player_year <- reactive({
    filter(data_lec, Player == input$player & Year == input$year) 
  })

  output$eventSelection <- renderUI({selectInput("event", "Select event:", choices = unique(selected_player_year()$Event),
                                                width = "100%")})
  
  selected_data <- reactive({
    filter(data_lec, Player == input$player & Year == input$year & Event == input$event) 
  })
  
  # Display player's name
  output$player_name <- renderText({
    player_data <- selected_data()
    paste("Selected player: ", unique(player_data$Player))
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
                geom_line(size = 6, colour = "#6a6e70") +
                xlab(NULL) + 
                ylab(NULL) +
                ggtitle("Player Trajectory:") +
                theme_dark() +       # Use a minimalistic theme
                theme(
                plot.title = element_text(color="white"),
                panel.background = element_rect(fill = "black"),     # Set panel background to black
                plot.background = element_rect(fill = "black")       # Set plot background to black
              )
    ggplotly(plot_traj)
  })

  # Player information
  output$player_info_box <- renderUI({
    selected_player <- selected_data()
      # Create a list to store the static boxes
    info_boxes <- lapply(seq_len(nrow(selected_player)), function(i) {
      div(
        class = "info-box",
        h3(paste("Player:", selected_player$"Player original name")),
        p(paste("Team:", selected_player$Team[i])),
        p(paste("Position:", selected_player$Position[i])),
        p(paste("KDA:", selected_player$KDA[i]))
      )
    })
    
    box_columns <- lapply(info_boxes, function(box) {
      column(width = 4, box)  # Adjust the width as needed
    })
    
    # Return the boxes wrapped in a fluidRow
    fluidRow(box_columns)
  })

}

# Run the Shiny app
shinyApp(ui, server)
