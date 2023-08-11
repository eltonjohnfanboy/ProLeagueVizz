library(shiny)
library(plotly)

# Sample data (replace this with your actual data)
data <- data.frame(
  Player = c("Player A", "Player B", "Player C", "Player D"),
  KDA = c(3.2, 4.5, 2.8, 5.1),
  Winrate = c(65, 75, 55, 80),  # Assuming the Winrate values are in the 0-100 range
  NumGames = c(150, 200, 100, 250)
)

# Scale Winrate to the 0-1 range
data$Winrate <- data$Winrate / 100

ui <- fluidPage(
  titlePanel("League of Legends Bubble Chart"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotlyOutput("bubblePlot")
    )
  )
)

server <- function(input, output) {
  output$bubblePlot <- renderPlotly({
    bubbleChart <- plot_ly(data, x = ~NumGames, y = ~Winrate,
                           text = ~Player, size = ~KDA, sizes = c(10, 100),
                           marker = list(color = "rgba(50, 171, 96, 0.6)", opacity = 0.6)) %>%
      layout(
        title = "Player Performance Bubble Chart",
        xaxis = list(title = "Number of Games"),
        yaxis = list(title = "Winrate"),
        showlegend = FALSE,
        hovermode = "closest"
      )
    
    # Style the layout
    bubbleChart %>% config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
