library(shiny)

ui <- fluidPage(
  shinybrowser::detect()
)

server <- function(input, output, session) {
  observe({
    str(shinybrowser::get_all_info())
  })
}

shinyApp(ui, server)
