library(shiny)
library(shinythemes)

ui <- navbarPage("IMFAI", theme = shinytheme("darkly"),
  tabPanel("Juros Simples",
    sidebarLayout(
      sidebarPanel(
        numericInput("cJS",
          "Valor Inicial",
          0,00
        ),
        numericInput("iJS",
          "Taxa de Juros (%)",
          0,00
        ),
        numericInput("tJS",
          "Tempo da Aplicação",
          0,00
        )
      ),
      mainPanel(
        textOutput("resultadoJS")
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