# Define ui
ui <- fluidPage(
                useShinyjs(),
                includeCSS("./www/style.css"),
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
                              column(width = 4, uiOutput("player_info_box")),
                              column(width = 4, imageOutput("playerImage"))
                            ),
                            fluidRow(
                              column(width = 6,
                              tags$div(
                                      style = "margin-bottom: 10px;",  # Add margin between the top plot and the bottom plot
                                      plotlyOutput("plots1")
                                    )
                              ),
                              column(width = 6,
                                tags$style(HTML("
                                  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate,
                                  .dataTables_wrapper .dataTables_filter input, .dataTables_wrapper .dataTables_length select {
                                    color: white;
                                  }
                                  .dataTables_wrapper thead th {
                                    color: white;
                                    background-color: #6a6e70; /* Dark gray background for column headers */
                                  }
                                ")),
                                DTOutput("table")
                              )
                            ),
                            actionButton("btn_section1", "Early game", style = "width: 100%;"),
                            fluidRow(id = "content_section1", style = "display: none;",
                              column(width = 12,
                                  div(class = "early-game-container",
                                    h3("Early Game"),
                                    fluidRow(
                                      column(width = 2,
                                        div(class = "first-blood-container",
                                        h4("First Blood Stats"), htmlOutput("first_blood_stats"),
                                        ),
                                        ),
                                      column(width = 10, plotOutput("density_plots"))
                                    ),
                                    div(class = 'early-radioButtons-container',
                                          radioButtons("player_option", "Display option:",
                                                    choices = c("All players", "Position wise"),
                                                    selected = "All players")
                                      )
                                    ))
                            ),
                            actionButton("btn_section2", "Aggression", style = "width: 100%;"),
                            fluidRow(id = "content_section2", style = "display: none;",
                              column(width = 12,
                                  div(class = "agression-container",
                                    h3("Aggression"),
                                    fluidRow(
                                      column(width = 2,
                                        div(class = "aggr-data-container",
                                        h4("Player Aggression Stats"), htmlOutput("basic_aggr_stats"),
                                        ),
                                        ),
                                      column(width = 10, plotlyOutput("aggr_histograms"))
                                    ),
                                    div(class = 'aggr-radioButtons-container',
                                          radioButtons("display_option_aggr", "Display option:",
                                                    choices = c("All players", "Position wise"),
                                                    selected = "All players")
                                      )
                                    ))
                            ),
                            actionButton("btn_section3", "Efficiency", style = "width: 100%;"),
                            fluidRow(id = "content_section3", style = "display: none;",
                              column(width = 12,
                                  div(class = "agression-container",
                                    h3("Efficiency"),
                                    fluidRow(
                                      column(width = 12, plotlyOutput("eff_scatter"))
                                    ),
                                    div(class = 'aggr-radioButtons-container',
                                          radioButtons("display_option_eff", "Display option:",
                                                    choices = c("All players", "Position wise"),
                                                    selected = "All players")
                                      )
                                    ))
                            ),
                            actionButton("btn_section4", "Champion pool", style = "width: 100%;"),
                            fluidRow(id = "content_section4", style = "display: none;",
                              column(width = 12,
                                  div(class = "cpool-container",
                                    h3("Champion pool"),
                                    fluidRow(
                                      column(width = 6, visNetworkOutput("cpool_graph")),
                                      column(width = 6, div(class = "cpool-bubble", plotlyOutput("bubbleCpool"),
                                                            div(class = "cpool-expl-box", "To the right, a graph emerges. Bigger nodes denote champions player by the chosen player, whether from the selected tournament or across all matches. This similarity graph interconnects champions, unveiling playstyle patterns and potential selections. Currently the graph's affiliations stem from a content-based filtering recommendation system, it aspires to evolve into a more refined recommendation system.")))
                                    ),
                                    div(class = 'aggr-radioButtons-container',
                                          radioButtons("display_option_cpool", "Display option:",
                                                    choices = c("Selected tournament", "All history"),
                                                    selected = "Selected tournament")
                                      )
                                    ))
                            )
                          )
                  ),
                  tabPanel("Team stats", "Under construction 👷"),
                  tabPanel("Team comparison", "Under construction 👷")
                ) # navbar page
              ) #fluidpage