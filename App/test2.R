# Libraries ---------------------------------------------------------------
library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(visNetwork)
library(geomnet)
library(igraph)


# Set the working directory
setwd("C:/Users/adars/OneDrive/Escritorio/ProjecteLolShiny/Data")

# Read data
data_lec <- read_csv("finalChampSimilarity.csv")
champ_pos <- read_csv("champPosition.csv")


#Nodes
nodes <- as.data.frame(data_lec$Champion, data_lec$Champion)
colnames(nodes) <- c("label")
nodes$id <- nodes$label

champion_names <- data_lec$Champion

# Initialize an empty list to store edges
edges <- list()

# Iterate through each row of the matrix to create edges
for (i in 1:nrow(data_lec)) {
  from_champion <- champion_names[i]
  
  # Iterate through each champion column
  for (j in 2:ncol(data_lec)) {
    to_champion <- colnames(data_lec)[j]
    width_value <- data_lec[i, j]
    
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


visNetwork(nodes, filtered_edges, width = "100%") %>%
  visIgraphLayout() %>%
  visNodes(
    shape = "dot",
    color = list(
      background = "#0085AF",
      border = "#013848",
      highlight = "#FF8000"
    ),
    shadow = list(enabled = TRUE, size = 10)
  ) %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#0085AF", highlight = "#C62F4B")
  ) %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
             selectedBy = "group") %>% 
  visLayout(randomSeed = 11)


