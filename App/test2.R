library(ggplot2)

# Sample data (replace with your actual data)
player_results <- data.frame(
  Player = c("Player A", "Player A", "Player A", "Player A", "Player B", "Player B", "Player B"),
  Position = c("1st", "2nd", "3rd", "9th", "1st", "2nd", "5th"),
  Trophies_Won = c(3, 2, 1, 0, 4, 2, 1)
)

# Plot the trophies won by a player (Bar Chart)
trophies_plot <- ggplot(player_results, aes(x = Player, y = Trophies_Won, fill = Player)) +
  geom_bar(stat = "identity") +
  xlab("Player") +
  ylab("Number of Trophies Won") +
  scale_fill_manual(values = c("Player A" = "blue", "Player B" = "green")) +
  theme_minimal()

# Plot the overall tournament results (Grouped Bar Chart)
overall_results_plot <- ggplot(player_results, aes(x = Position, y = Trophies_Won, fill = Player)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  xlab("Tournament Position") +
  ylab("Number of Trophies Won") +
  scale_fill_manual(values = c("Player A" = "blue", "Player B" = "green")) +
  theme_minimal()

# Display the plots
trophies_plot
overall_results_plot
