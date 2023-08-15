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
  
  dp_data_player_event <- reactive({
    filter(df_early_vision_aggr, Player == input$player & matched_event == input$event)
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
                labs(title = "Player's trajectory") +
                theme_dark() +       # Use a minimalistic theme
                theme(
                plot.title = element_text(color="white"),
                panel.background = element_rect(fill = "black"),     # Set panel background to black
                plot.background = element_rect(fill = "black")       # Set plot background to black
              )
    p1 <- ggplotly(plot_traj)
    return(p1)
  })

  output$table <- renderDataTable({
    data_aux <- df_trophies %>% filter(Name == input$player & Year == input$year)
    df_table <- data_aux[c("Tournament", "Tier", "Position", "Date")]

    background <- "value == '1st' ? 'gold' : value != 'else' ? '' : ''"
    text_color <- "white"
    class(background) <- "JS_EVAL"
    
    datatable(
      df_table, 
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>% formatStyle(
      'Position',
      target = 'row',
      backgroundColor = background,
      color = text_color
    )
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
        p(paste("Position:", selected_player$Position[i]))
      )
    })
    
    box_columns <- lapply(info_boxes, function(box) {
      column(width = 8, box)  # Adjust the width as needed
    })
    
    # Return the boxes wrapped in a fluidRow
    fluidRow(box_columns)
  })


  output$density_plots <- renderPlot({
    # Create density plot for the chosen StatColumn

    if (input$player_option == "All players") {
      dp_df <- df_early_vision_aggr %>% filter(matched_event == input$event)
    } else{
      pos <- df_early_vision_aggr %>% filter(Player == input$player) %>% select(Position) %>% unique()
      dp_df <- df_early_vision_aggr %>% filter(matched_event == input$event & Position == pull(pos))
    }
    dp_df$y <- 1

  density_plotCSD <- ggplot(dp_df, aes(x = `CSD@15`, y = y, fill = 0.5 - abs(0.5 - after_stat(ecdf)))) +
                  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
                  scale_fill_viridis_c(name = "Density", direction = -1, option = "C") +
                  xlab("CS difference at 15") +
                  ylab("Density") +
                  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
                  theme(
                        plot.title = element_text(color="white"),
                        axis.title.x = element_text(color = "white"),
                        axis.text.x = element_text(color = "white"),
                        axis.title.y = element_blank(),
                        axis.text.y = element_blank(),
                        panel.background = element_rect(fill = "black"),     # Set panel background to black
                        plot.background = element_rect(fill = "black"),       # Set plot background to black
                        legend.text = element_text(color = "white"),
                        legend.title = element_text(color = "white")
                        )

    # Get the value of the chosen player in the selected column
    player_stat_value <- dp_data_player_event()$`CSD@15`

    # Add a vertical line to indicate player's position
    density_plotCSD <- density_plotCSD +
      geom_vline(xintercept = player_stat_value, linetype = "dashed", color = "red") +
      annotate("text", x = player_stat_value, y = 1, vjust = -1, label = input$player, color = "red")


  density_plotXPD <- ggplot(dp_df, aes(x = `XPD@15`, y = y, fill = 0.5 - abs(0.5 - after_stat(ecdf)))) +
                  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
                  scale_fill_viridis_c(name = "Density", direction = -1, option = "C") +
                  xlab("XP difference at 15") +
                  ylab("Density") +
                  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
                  theme(
                        plot.title = element_text(color="white"),
                        axis.title.x = element_text(color = "white"),
                        axis.text.x = element_text(color = "white"),
                        axis.title.y = element_blank(),
                        axis.text.y = element_blank(),
                        panel.background = element_rect(fill = "black"),     # Set panel background to black
                        plot.background = element_rect(fill = "black"),       # Set plot background to black
                        legend.text = element_text(color = "white"),
                        legend.title = element_text(color = "white")
                        )

    # Get the value of the chosen player in the selected column
    player_xpd_value <- dp_data_player_event()$`XPD@15`

    # Add a vertical line to indicate player's position
    density_plotXPD <- density_plotXPD +
      geom_vline(xintercept = player_xpd_value, linetype = "dashed", color = "red") +
      annotate("text", x = player_xpd_value, y = 1, vjust = -1, label = input$player, color = "red")

    # Plot
    combined_density_plots <- grid.arrange(density_plotCSD, density_plotXPD, ncol=2)
  
    print(combined_density_plots)
  })




  output$aggr_histograms <- renderPlotly({
    # Create aggresin histogram plot for the chosen StatColumn

    if (input$display_option_aggr == "All players") {
      dp_df <- df_early_vision_aggr %>% filter(matched_event == input$event)
    } else{
      pos <- df_early_vision_aggr %>% filter(Player == input$player) %>% select(Position) %>% unique()
      dp_df <- df_early_vision_aggr %>% filter(matched_event == input$event & Position == pull(pos))
    }
    dp_df$y <- 1
    dp_df$IsSelectedPlayer <- ifelse(dp_df$Player == input$player, "Selected", "Others")

  top_10_players <- dp_df %>%
    arrange(desc(DPM)) %>%
    head(10)

  hist_dpm <- ggplot(top_10_players, aes(x = reorder(Player, DPM), y = DPM, fill = IsSelectedPlayer)) +
                  geom_bar(stat = "identity", show.legend = FALSE) +
                  xlab("Player") +
                  ylab("Damage per minute") +
                  theme(
                        plot.title = element_text(color="white"),
                        axis.title.y = element_text(color = "white"),
                        axis.text.y = element_text(color = "white"),
                        axis.title.x = element_text(color = "white"),
                        axis.text.x = element_text(color = "white"),
                        panel.background = element_rect(fill = "black"),     # Set panel background to black
                        plot.background = element_rect(fill = "black"),       # Set plot background to black
                        panel.grid = element_blank(),
                        legend.text = element_text(color = "white"),
                        legend.title = element_text(color = "white")
                        ) +
                        coord_flip()
  hist_dpm <- ggplotly(hist_dpm)

  hist_KApm <- ggplot(top_10_players, aes(x = reorder(Player, KA_PM), y = KA_PM, fill = IsSelectedPlayer)) +
                  geom_bar(stat = "identity", show.legend = FALSE) +
                  xlab("Player") +
                  ylab("K+A per minute") +
                  theme(
                        plot.title = element_text(color="white"),
                        axis.title.y = element_text(color = "white"),
                        axis.text.y = element_text(color = "white"),
                        axis.title.x = element_text(color = "white"),
                        axis.text.x = element_text(color = "white"),
                        panel.background = element_rect(fill = "black"),     # Set panel background to black
                        plot.background = element_rect(fill = "black"),       # Set plot background to black
                        panel.grid = element_blank(),
                        legend.text = element_text(color = "white"),
                        legend.title = element_text(color = "white")
                        ) +
                        coord_flip()
    hist_KApm <- ggplotly(hist_KApm)

    combined_hist_plot <- subplot(hist_dpm, hist_KApm, titleY = TRUE, titleX = TRUE, margin = 0.05) %>%
                          layout(showlegend = FALSE)

    print(combined_hist_plot)
  })



  output$eff_scatter <- renderPlotly({
    # Create aggresin histogram plot for the chosen StatColumn

    if (input$display_option_eff == "All players") {
      dp_df <- df_early_vision_aggr %>% filter(matched_event == input$event)
      data_event <- data_indiv %>% filter(Event == input$event & Year == input$year)
    } else{
      pos <- df_early_vision_aggr %>% filter(Player == input$player) %>% select(Position) %>% unique()
      dp_df <- df_early_vision_aggr %>% filter(matched_event == input$event & Position == pull(pos))
      data_event <- data_indiv %>% filter(Event == input$event & Year == input$year & Position == pull(pos))
    }
    data_aux <- data_indiv %>% filter(Player == input$player & Event == input$event & Year == input$year)

    p1 <- ggplot(data_event, aes(x = KP, y = `DTH%`)) + 
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

    p1 <- ggplotly(p1)

    p2 <- ggplot(data_event, aes(x = `GOLD%`, y = `DMG%`)) + 
                geom_point(data = data_aux, color = "red", size = 3, aes(text = paste("Player: ", Player, "<br>", "Position: ", Position))) +
                geom_point(data = data_event[data_event$Player != input$player, ], color = "gray", size = 3, aes(text = paste("Player: ", Player, "<br>", "Position: ", Position))) +
                #geom_smooth(method = NULL, level = 0.9) +
                #geom_text(data = data_aux, aes(label = Player), hjust = 0, vjust = 10, size = 4, color = "red") +
                labs(title = "", x = "Gold share (%)", y = "Damage share (%)") + 
                theme_dark() +
                theme(
                plot.title = element_text(color="white"),
                axis.title.x = element_text(color = "white"),
                axis.title.y = element_text(color = "white"),
                panel.background = element_rect(fill = "black"),     # Set panel background to black
                plot.background = element_rect(fill = "black")       # Set plot background to black
                )

    p2 <- ggplotly(p2)

    combined_plot <- subplot(p1, p2, titleY = TRUE, titleX = TRUE, margin = 0.05)
    
    return(combined_plot)

  })

  # champion pool graph
  output$cpool_graph <- renderVisNetwork({
    
    highlighted_nodes <- 
      if (input$display_option_cpool == "Selected tournament") {
        cpool_players %>% filter(Player == input$player & matched_event == input$event) %>% pull(Champion)
      } else{
        cpool_players %>% filter(Player == input$player & matched_event == "ALL") %>% pull(Champion)
      }
  
    aux_nodes <- nodes
    aux_nodes <- aux_nodes %>%
      mutate(size = ifelse(label %in% highlighted_nodes, 75, 25)
    )

    p1 <- visNetwork(aux_nodes, filtered_edges, width = "100%") %>%
          visNodes(
            font = list(color = "white", size = 20)
            ) %>%
          addFontAwesome() %>%
          visLegend(width = 0.1, position = "left", main = "") %>%
          visInteraction(hover = TRUE, tooltipDelay = 100, hideEdgesOnDrag = TRUE, tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;font-color:black;background-color: black;') %>%
          visOptions(width = "585", height = "500px", highlightNearest = TRUE)

    return(p1)

  })
  
  # champion pool bubble
  output$bubbleCpool <- renderPlotly({
    
    bubble_data <- 
      if (input$display_option_cpool == "Selected tournament") {
        cpool_players %>% filter(Player == input$player & matched_event == input$event)
      } else{
        cpool_players %>% filter(Player == input$player & matched_event == "ALL")
      }
    
    champ_pos$Champion <- champ_pos$label
    bubble_data <- left_join(bubble_data, champ_pos, by = "Champion")
    position_colors <- c(Top = "#4da2c4", Jungle = "#41ee41", Middle = "yellow", Bottom = "#c05cff", Support = "#f35e5e")
    
    
    bubbleChart <- plot_ly(bubble_data, x = ~`Nb games`, y = ~`Win Rate(%)`,
                           size = ~KDA, sizes = c(70, 500),
                           color = ~Position,
                           colors = position_colors,
                           marker = list(opacity = 0.45),
                           hoverinfo = 'text',
                           text = ~paste('Champion:', Champion, '<br>KDA:', KDA, '<br>Games player:', `Nb games`, '<br>Win rate (%):', `Win Rate(%)`)) %>%
                  layout(
                    title = "Players Champion Performance",
                    xaxis = list(title = "Number of Games"),
                    yaxis = list(title = "Winrate"),
                    showlegend = TRUE, # Show the legend for color
                    hovermode = "closest",
                    font = list(color = "white"),
                    paper_bgcolor = "black",
                    plot_bgcolor = "black",
                    titlefont = list(color = "white"),
                    xaxis_title_font = list(color = "white"),
                    yaxis_title_font = list(color = "white")
                  )
    
    # Style the layout
    p1 <- bubbleChart %>% config(displayModeBar = FALSE)
    return(p1)
  })

  output$playerImage <- renderImage({
    image_directory <- "./PlayerImages"
    img_path <- file.path(image_directory, paste0("image_", input$player, ".jpg"))
    list(src = img_path,
         alt = input$player,
         width = "200")
  }, deleteFile = FALSE)

  output$first_blood_stats <- renderText({
    paste("First Blood particip.:", dp_data_player_event()$`FB_Participation%`, "%", "<br>",
        "First Blood victim:", dp_data_player_event()$`FB_Victim%`, "%")
  })

  output$basic_aggr_stats <- renderText({
    paste("KDA:", selected_data()$KDA, "<br>",
        "Solo kills:", dp_data_player_event()$SoloKills)
  })

  observeEvent(input$btn_section1, {
    shinyjs::toggle("content_section1")
  })
  
  observeEvent(input$btn_section2, {
    shinyjs::toggle("content_section2")
  })

  observeEvent(input$btn_section3, {
    shinyjs::toggle("content_section3")
  })

  observeEvent(input$btn_section4, {
    shinyjs::toggle("content_section4")
  })

}