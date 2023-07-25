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
library(forcats)


# Set the working directory
setwd("C:/Users/adars/OneDrive/Escritorio/ProjecteLolShiny/Data")


# Read data
data_lec <- read_csv("PlayerStats.csv")
data_indiv <- read_csv("PlayerStatsGeneral.csv")
data_traj <- read_csv("PlayersTrajectoyData.csv")

# Define ui
ui <- fluidPage(
                includeCSS("C:/Users/adars/OneDrive/Escritorio/ProjecteLolShiny/www/style.css"),
                theme = shinythemes::shinytheme("slate"),
                navbarPage(
                  "LeagueStats",
                  tabPanel("Player stats",
                          fluidPage(
                            fluidRow(
                              column(width = 4,
                                      tags$h3("Search Bar"),
                                      selectInput("player", "Select player:", choices = unique(data_lec$Player),
                                                  width = "100%", selected = "Elyoya"),
                                      uiOutput("yearSelection"),
                                      uiOutput("eventSelection")
                              ),
                              column(width = 8, uiOutput("player_info_box"))
                            ),
                            fluidRow(
                              column(width = 12, plotlyOutput("plots1"))
                            )
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
  
  output$yearSelection <- renderUI({selectInput("year", "Select year:", choices = unique(selected_player()$Year),
                                                width = "100%", selected = "2021")})
  
selected_player_year <- reactive({
    filter(data_lec, Player == input$player & Year == input$year) 
  })

  output$eventSelection <- renderUI({selectInput("event", "Select event:", choices = unique(selected_player_year()$Event),
                                                width = "100%", selected = "MSI 2021")})
  
  selected_data <- reactive({
    filter(data_lec, Player == input$player & Year == input$year & Event == input$event) 
  })
  
  # Display player's name
  output$player_name <- renderText({
    player_data <- selected_data()
    paste("Selected player: ", unique(player_data$Player))
  })
  
  # Display trajectory plot and DEATH% and KP scatterplot
  output$plots1 <- renderPlotly({
    data_plotTraj <- data_traj %>% filter(Player == input$player)
    dfr <- data.frame(
    name = data_plotTraj$`Teams trajectory` %>% fct_inorder(),
    start.date = as.Date(c(data_plotTraj$start_date)),
    end.date = as.Date(c(data_plotTraj$end_date))
    )
    mdfr <- melt(dfr, measure.vars = c("start.date", "end.date"))
    plot_traj <- ggplot(mdfr, aes(value, name)) + 
                geom_line(size = 6, colour = "#6a6e70") +
                xlab(NULL) + 
                ylab(NULL) +
                labs(title = "") +
                theme_dark() +       # Use a minimalistic theme
                theme(
                plot.title = element_text(color="white"),
                panel.background = element_rect(fill = "black"),     # Set panel background to black
                plot.background = element_rect(fill = "black")       # Set plot background to black
              )
    p1 <- ggplotly(plot_traj)

    data_event <- data_indiv %>% filter(Event == input$event & Year == input$year)
    data_aux <- data_indiv %>% filter(Player == input$player & Event == input$event & Year == input$year)
    p <- ggplot(data_event, aes(x = KP, y = `DTH%`)) + 
                geom_point(data = data_aux, color = "red", size = 3, aes(text = paste("Player: ", Player, "<br>", "Position: ", Position))) +
                geom_point(data = data_event[data_event$Player != input$player, ], color = "gray", size = 3, aes(text = paste("Player: ", Player, "<br>", "Position: ", Position))) +
                #geom_text(data = data_aux, aes(label = Player), hjust = 0, vjust = 10, size = 4, color = "red") +
                labs(title = "", x = "Kill participation (%)", y = "Team's death participation (%)") + 
                theme_dark() +
                theme(
                plot.title = element_text(color="white"),
                axis.title.x = element_text(color = "white"),
                axis.title.y = element_text(color = "white"),
                panel.background = element_rect(fill = "black"),     # Set panel background to black
                plot.background = element_rect(fill = "black")       # Set plot background to black
                )

    p2 <- ggplotly(p)

    combined_plot <- subplot(p1, p2, titleY = TRUE, titleX = TRUE, margin = 0.05)
    
    return(combined_plot)

  })

  

  # Player information
  output$player_info_box <- renderUI({
    selected_player <- selected_data()
      # Create a list to store the static boxes
    info_boxes <- lapply(seq_len(nrow(selected_player)), function(i) {
      div(
        class = "info-box",
        h3(paste("Player:", selected_player$"Player original name"[i])),
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
