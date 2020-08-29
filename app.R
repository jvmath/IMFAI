library(shiny)
library(shinythemes)

ui <- navbarPage("IMFAI", theme = shinytheme("darkly"),
  tabPanel("Juros Simples"),
  tabPanel("Juros Compostos"),
  tabPanel("Sistema SAC"),
  tabPanel("Sistema Price")
)

server <- function(input, output) {
  
}

shinyApp(ui, server)