library(shiny)
ui <- fluidPage(
   shinybrowser::detect(),
   "Your browser information:",
   verbatimTextOutput("browser_info")
)
server <- function(input, output, session) {
   output$browser_info <- renderPrint({
      shinybrowser::get_all_info()
   })
}
shinyApp(ui, server)
