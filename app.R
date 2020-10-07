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
        h1(textOutput("resultadoJS"))
      )
    )
  ),
  tabPanel("Juros Compostos",
           sidebarLayout(
             sidebarPanel(
               numericInput("cJC",
                            "Valor Inicial",
                            0,00
               ),
               numericInput("iJC",
                            "Taxa de Juros (%)",
                            0,00
               ),
               numericInput("tJC",
                            "Tempo da Aplicação",
                            0,00
               )
             ),
             mainPanel(
               h1(textOutput("resultadoJSC"))
             )
           )
  ),
  tabPanel("Sistema SAC",
           sidebarLayout(
             sidebarPanel(
               numericInput("value",
                            "Valor do Emprestimo",
                            0,00
               ),
               numericInput("i",
                            "Taxa de Juros (%)",
                            0,00
               ),
               numericInput("k",
                            "Tempo da Aplicação em meses",
                            0,00
               )
             ),
             mainPanel(
               tableOutput("resultadoSA"),
               h1(textOutput("msg"))
             )
           )
  ),
  tabPanel("Sistema Price")
)

server <- function(input, output) {
  output$resultadoSA <- renderTable({
    
    D <- matrix(nrow = input$k, ncol = 1)
    P <- matrix(nrow = input$k, ncol = 1)
    J <- matrix(nrow = input$k, ncol = 1)
    for(l in 1:((input$k)-1)){
      A <- input$value/input$k
      D0 <- input$value
      D[1,] <- D0-A
      D[l+1,] <- D[l,]-A
      
      J[1,] <- D0*(input$i/100)
      J[l+1,] <- D[l,]*(input$i/100)
      
      P[l,] <- J[l,]+A
      P[input$k,] <- J[input$k,]+A
    }
    
    Epoca <- seq(1:input$k)
    
    result <- data.frame("Época"=c(0, Epoca, "Soma"), 
                         "Amortização"=c("-", rep(A,input$k), sum(rep(A,input$k))), 
                         "Juros"=c("-", J, sum(J)), 
                         "Prestação"=c("-", P, sum(P)), 
                         "Dívida"=c(D0, D, "-"))
    result
    
  })
  
  output$msg <-renderText({
    cat("A taxa de juros utilizada é =", input$i,"%")
  })
    
  {output$resultadoJS <- renderText({
    input$tJS*(input$iJS/100)*input$cJS
  })}
    
  }

shinyApp(ui, server)