library(shiny)
library(visNetwork)

# Sample data: Nodes and edges
nodes <- data.frame(id = 1:10, label = paste("Node", 1:10))
edges <- data.frame(from = c(1, 1, 2, 3, 3, 4, 5, 6, 7), to = c(2, 3, 4, 5, 6, 7, 8, 9, 10))

# Sample data: Player - Champ mapping
player_champs <- data.frame(player_id = c(rep(1, 4), rep(2, 6)), champ_id = sample(1:10, 10, replace = TRUE))

ui <- fluidPage(
  selectInput("selected_player", "Select a player:", choices = c(1, 2)),
  visNetworkOutput("network")
)

server <- function(input, output, session) {
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges) %>%
      visOptions(highlightNearest = TRUE)
  })
  
  observe({
    selected_player <- input$selected_player
    if (!is.null(selected_player)) {
      player_champs_subset <- player_champs[player_champs$player_id == selected_player, ]
      highlighted_nodes <- player_champs_subset$champ_id
      
      nodes_subset <- nodes
      nodes_subset$size <- ifelse(nodes_subset$id %in% highlighted_nodes, 30, 15)
      nodes_subset$color <- ifelse(nodes_subset$id %in% highlighted_nodes, "red", "blue")
      
      visNetworkProxy("network") %>%
        visUpdateNodes(nodes_subset)
    }
  })
}

shinyApp(ui, server)
