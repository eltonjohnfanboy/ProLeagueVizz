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


setwd("C:/Users/adars/OneDrive/Escritorio/ProjecteLolShiny/Data")
df_early_vision_aggr <- read_csv("PlayersGolggStats.csv")

if ("All players" == "All players2") {
  dp_df <- df_early_vision_aggr %>% filter(matched_event == "MSI 2021")
} else{
  pos <- df_early_vision_aggr %>% filter(Player == "Elyoya") %>% select(Position) %>% unique()
  print(pos)
  dp_df <- df_early_vision_aggr %>% filter(matched_event == "MSI 2023" & Position == pull(pos))
}
dp_df$y <- 1