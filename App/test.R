library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .expand-content {
        max-height: 0;
        overflow: hidden;
        transition: max-height 0.5s ease-out;
      }
      .expanded {
        max-height: 500px; /* Adjust this value to control the size of the expanded content */
        margin: 20px 0;
      }
      #expand_wrapper {
        display: flex;
        align-items: center;
        justify-content: center;
        height: 100vh;
      }
      #expand_button {
        background-color: #007BFF;
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        font-size: 18px;
        cursor: pointer;
        outline: none;
      }
    "))
  ),
  div(
    id = "expand_wrapper",
    div(
      id = "expand_button",
      "Click to Expand"
    ),
    div(
      class = "expand-content",
      id = "expand_content",
      p("This is the hidden content that will be expanded when you click the button.")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$expand_button, {
    runjs("$('.expand-content').toggleClass('expanded');")
  })
}

shinyApp(ui, server)
