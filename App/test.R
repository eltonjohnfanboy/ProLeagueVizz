library(reshape2)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)

setwd("C:/Users/adars/OneDrive/Escritorio/ProjecteLolShiny/Data")
data_lec <- read_csv("PlayersTrajectoyData.csv")
#data_lec$converted_dates <- as.Date(data_lec$converted_dates)



data_aux <- data_lec %>% filter(Player == "113")



dfr <- data.frame(
  name = factor(c(data_aux$`Teams trajectory`), levels = data_aux$`Teams trajectory`),
  start.date = as.Date(c(data_aux$start_date)),
  end.date = as.Date(c(data_aux$end_date))
)

mdfr <- melt(dfr, measure.vars = c("start.date", "end.date"))
ggplot(mdfr, aes(value, name)) + 
  geom_line(size = 6, colour = "skyblue") +
  xlab(NULL) + 
  ylab(NULL)
