library(shiny)
library(shinythemes)

ui <- navbarPage("IMFAI", theme = shinytheme("darkly"),
  tabPanel("Juros Simples",
    sidebarLayout(
      sidebarPanel(
        numericInput("c",
          "Valor Inicial",
          0,00
        ),
        numericInput("i",
          "Taxa de Juros (%)",
          0,00
        ),
        numericInput("t",
          "Tempo da Aplicação",
          0,00
        )
      ),
      mainPanel(
               
      )
    )
  ),
  tabPanel("Juros Compostos"),
  tabPanel("Sistema SAC"),
  tabPanel("Sistema Price")
)

server <- function(input, output) {
  
}

shinyApp(ui, server)