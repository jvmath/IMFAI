library(shiny)
library(shinythemes)
library(stringr)
library(data.table)
library(png)
library(markdown)
ui <- navbarPage("IMFAI", theme = shinytheme("sandstone"),
                 tabPanel("Inic",
                        HTML('<iframe width="560" height="315"
                             src="https://www.youtube.com/embed/Ka2pWqXS1WA"
                             frameborder="0"
                             allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                             allowfullscreen></iframe>')
                 ),
                 ##
                 tabPanel("Juros Simples",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("cJS",
                                           "Valor Inicial",
                                           0,00
                              ),
                              numericInput("iJS",
                                           "Taxa de Juros mensal (%)",
                                           0,00
                              ),
                              numericInput("tJS",
                                           "Tempo da Aplicação em meses",
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
                                           "Taxa de Juros mensal (%)",
                                           0,00
                              ),
                              numericInput("tJC",
                                           "Tempo da Aplicação em meses",
                                           0,00
                              )
                            ),
                            mainPanel(
                              h1(textOutput("resultadoJC"))
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
                              DT::dataTableOutput("resultadoSA"),
                              h1(textOutput("msg"))
                            )
                          )
                 ),
                 
                 tabPanel("Sistema Price",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("valuep",
                                           "Valor do Emprestimo",
                                           0
                                           
                              ),
                              numericInput("ip",
                                           "Taxa de Juros (%)",
                                           0
                              ),
                              numericInput("kp",
                                           "Tempo da Aplicação",
                                           0
                              ),
                              textOutput("resultadoP")
                            ),
                            mainPanel(
                              textOutput("NoValue"),
                              DT::dataTableOutput("resultadoPR")
                            )
                          )
                 ),
                 tabPanel("Sobre", icon = icon("comment", lib = "glyphicon"),
                          includeMarkdown("sobre.md")),
                 tabPanel("Analise Financeira",
                          p("Pagina criada para o projeto TCC...") )
)


server <- function(input, output) {
  output$resultadoSA <- DT::renderDataTable({
    if(input$value=="0"||input$i=="0"||input$k=="0"){
      
      as.data.table(paste("Preencha os valores da coluna lateral"))
      
      
    }else{
      
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
      A <- round(A,2)
      J <- round(J,2)
      D <- round(D,2)
      P <- round(P,2)
      
      Epoca <- seq(1:input$k)
      
      result <- data.frame(
        "Amortização"=c("-", rep(A,input$k), sum(rep(A,input$k))), 
        "Juros"=c("-", J, sum(J)), 
        "Prestação"=c("-", P, sum(P)), 
        "Divida"=c(D0, D, "Total"))
      result <- as.data.table(result)
      DT::datatable(result, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
      ))
      
    }
    
    
  })
  
  output$msg <-renderText({
    cat("A taxa de juros utilizada é  =", input$i,"%")
  })
  
  {output$resultadoJS <- renderText({
    input$tJS*(input$iJS/100)*input$cJS
  })}
  
  {output$resultadoJC <- renderText({
    input$cJC*((input$iJC/100)+1)^input$tJC
  })}
  
  output$resultadoPR <- DT::renderDataTable({
    
    if(input$valuep=="0"||input$ip=="0"||input$kp=="0"){
      
      as.data.table(paste("Preencha os valores da coluna lateral"))
      
      
    }else{
      
      D <- matrix(nrow = input$kp, ncol = 1)
      A <- matrix(nrow = input$kp, ncol = 1)
      J <- matrix(nrow = input$kp, ncol = 1)
      P <- input$valuep*(((1+(input$ip/100))^input$kp)*(input$ip/100))/(((1+(input$ip/100))^input$kp)-1)
      D0 <- input$valuep
      J[1]<- D0*(input$ip/100)
      A[1]<- P-J[1]
      D[1]<- D0-A[1]
      for(l in 2:((input$kp))){
        J[l]=D[l-1]*(input$ip/100)
        A[l,] <- P-J[l,]
        D[l,] <- D[l-1,]-A[l,]
      }
      A <- round(A,2)
      J <- round(J,2)
      D <- round(D,2)
      P <- round(P,2)
      
      
      
      Epoca <- seq(1:input$kp)
      
      result <- data.frame("Parcela"=c(0, Epoca, "Soma"), 
                           "Prestação"=c("-", rep(P,input$kp), sum(rep(P,input$kp))), 
                           "Juros"=c("-", J, sum(J)), 
                           "Amortização"=c("-", A, sum(A)), 
                           "Divida"=c(D0, D  , "-"))
      result <- as.data.table(result)
      DT::datatable(result, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
      ))
      
    }
    
    
  })
  output$resultadoP <- renderText({
    paste("A taxa de juros utilizada  é ", input$ip,"%")
  })
  
}

shinyApp(ui, server)
