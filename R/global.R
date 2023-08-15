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
library(ggridges)
library(viridis)
library(gridExtra)
library(shinyjs)
library(DT)
library(visNetwork)

# Set the working directory
setwd("C:/Users/adars/OneDrive/Escritorio/ProjecteLolShiny")


# Read data
data_lec <- read_csv("./Data/PlayerStats.csv")
data_indiv <- read_csv("./Data/PlayerStatsGeneral.csv")
data_traj <- read_csv("./Data/PlayersTrajectoyData.csv")
df_early_vision_aggr <- read_csv("./Data/PlayersGolggStats.csv")
df_trophies <- read_csv("./Data/PlayerTrophies.csv")
data_champ_sim <- read_csv("./Data/finalChampSimilarity.csv")
champ_pos <- read_csv("./Data/champPosition.csv")
cpool_players <- read_csv("./Data/PlayersChampPool.csv")



# Data processing
nodes <- as.data.frame(data_champ_sim$Champion, data_champ_sim$Champion)
colnames(nodes) <- c("label")
nodes$id <- nodes$label

champion_names <- data_champ_sim$Champion

# Initialize an empty list to store edges
edges <- list()

# Iterate through each row of the matrix to create edges
for (i in 1:nrow(data_champ_sim)) {
  from_champion <- champion_names[i]
  
  # Iterate through each champion column
  for (j in 2:ncol(data_champ_sim)) {
    to_champion <- colnames(data_champ_sim)[j]
    width_value <- data_champ_sim[i, j]
    
    # Create an edge and append to the list
    edge <- c(from_champion, to_champion, width_value)
    edges <- c(edges, list(edge))
  }
}

# Convert the list of edges into a data frame
edges_df <- do.call(rbind, edges)

# Set column names
colnames(edges_df) <- c("from", "to", "width")

#Create graph for Louvain
edges_df <- as.data.frame(edges_df)

nodes <- left_join(nodes, champ_pos, by = "label")
colnames(nodes)[3] <- "group"

# Create a visNetwork graph
filtered_edges <- edges_df[edges_df$width > 0.85, ]
filtered_edges$from <- as.character(filtered_edges$from)
filtered_edges$to <- as.character(filtered_edges$to)
filtered_edges <- filtered_edges[filtered_edges$from != filtered_edges$to, , drop = FALSE]
filtered_edges$title <- paste0(filtered_edges$from, " to ",filtered_edges$to, "<br> Similarity : ", round(as.numeric(filtered_edges$width), 2))