library(shiny)
library(shinythemes)
library(stringr)
library(data.table)
library(png)
library(markdown)
ui <- navbarPage("IMFAI", theme = shinytheme("sandstone"),
                 tabPanel("Inic",
                        HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/bJgFxZwu8SQ" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></center>'
                               )
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
            
                            ),
                            mainPanel(
                              textOutput("NoValue"),
                              DT::dataTableOutput("resultadoPR")
                            )
                          )
                 ),
                 tabPanel("Planjemaneto Financeiro",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("valuex",
                                           "Valor que deseja chegar",
                                           0
                                           
                              ),
                              numericInput("ix",
                                           "Rendimento (%) ( Caso não tenha, coloque 0 )",
                                           0
                              ),
                              numericInput("a0x",
                                           "Aporte inicial ",
                                           0
                              ),
                              numericInput("ax",
                                           "Aportes mensais ",
                                           0
                              ),
                            ),
                            mainPanel(
                              textOutput("Valuefinal"),
                              DT::dataTableOutput("resultadoanalise")
                            )
                          )
                 ),
                 
                 tabPanel("Analise Financeira2",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("valuexa",
                                           "Valor que deseja chegar",
                                           0
                                           
                              ),
                              numericInput("ixa",
                                           "Rendimento (%) ( Caso não tenha, coloque 0 )",
                                           0
                              ),
                              numericInput("a0xa",
                                           "Aporte inicial ",
                                           0
                              ),
                              numericInput("axa",
                                           "Tempo em meses",
                                           0
                              ),
                            ),
                            mainPanel(
                              textOutput("Valuefinal1"),
                              DT::dataTableOutput("resultadoanalise1")
                            )
                          )
                 ),
                 
                 tabPanel("Sistema Misto",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("emprestimoM",
                                           "Valor do Emprestimo",
                                           0
                                           
                              ),
                              numericInput("jurosM",
                                           "Taxa de Juros (%)",
                                           0
                              ),
                              numericInput("tempoM",
                                           "Tempo da Aplicação",
                                           0
                              ),
                              
                            ),
                            mainPanel(
                              textOutput("NoValue2"),
                              DT::dataTableOutput("resultadoMisto")
                            )
                          )
                          ),
                          
                 tabPanel("Sobre", icon = icon("comment", lib = "glyphicon"),
                          includeMarkdown("sobre.md"))
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
  output$resultadoPRx <- DT::renderDataTable({
    
    if(input$valuepx=="0"||input$ipx=="0"||input$kpx=="0"){
      
      as.data.table(paste("Preencha os valores da coluna lateral"))
      
      
    }else{
      
      Dx<- matrix(nrow = input$kp, ncol = 1)
      Ax <- matrix(nrow = input$kp, ncol = 1)
      Jx <- matrix(nrow = input$kp, ncol = 1)
      Px <- input$valuepx*(((1+(input$ip/100))^input$kp)*(input$ip/100))/(((1+(input$ip/100))^input$kp)-1)
      D0x <- input$valuepx
      Jx[1]<- D0x*(input$ip/100)
      Ax[1]<- Px-Jx[1]
      Dx[1]<- D0x-Ax[1]
      for(l in 2:((input$kp))){
        Jx[l]=Dx[l-1]*(input$ip/100)
        Ax[l,] <- Px-Jx[l,]
        Dx[l,] <- Dx[l-1,]-A[l,]
      }
      Ax <- round(A,2)
      Jx <- round(J,2)
      Dx <- round(D,2)
      Px <- round(P,2)
      
      
      
      Epoca <- seq(1:input$kpx)
      
      result <- data.frame("Parcela"=c(0, Epoca, "Soma"), 
                           "Prestação"=c("-", rep(P,input$kp), sum(rep(P,input$kpx))), 
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
  output$resultadoPx <- renderText({
    paste("A taxa de juros utilizada  é ", input$ipx,"%")
  })
  
  
  
  output$resultadoanalise <- DT::renderDataTable({
    if(input$valuex=="0"||input$ax=="0"||input$a0x=="0"){
      
      as.data.table(paste("Preencha os valores da coluna lateral"))
      
    }else{
      
      saldo <- data.frame(Saldo = input$a0x)
      
      vf <- input$a0x
      while (vf < input$valuex) {
        vf <- (vf * (1 + input$ix/100)) + input$ax
        saldo <- rbind(saldo, vf)
      }
      
      df <- data.frame(Saldo = saldo$Saldo,
                       Tempo = c(0:(nrow(saldo)-1)))
      for (i in 1:nrow(df)) {
        df$Saldo[[i]]= round(df$Saldo[[i]],2)
        
      }
      df <- as.data.table(df)
      DT::datatable(df, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
      ))
    }
  })
  
  output$resultadoanalise1 <- DT::renderDataTable({
    
    
    if(input$valuexa=="0"||input$axa=="0"||input$a0xa=="0" || input$ixa=="0"){
      
      as.data.table(paste("Preencha os valores da coluna lateral"))
      
    }else{
      aporte <-  round(((input$valuexa-input$a0xa)*input$ixa/100)/(((1+input$ixa/100)^input$axa)-1),2)
      saldo <- data.frame(Saldo = input$a0xa)
      
      vfa <- input$a0xa
      while (vfa < input$valuexa) {
        vfa <- (vfa * (1 + input$ixa/100)) + aporte
        saldo <- rbind(saldo, vfa)
      }
      
      df <- data.frame(Saldo = saldo$Saldo,
                       Tempo = c(0:(nrow(saldo)-1)),
                       Aporte = aporte)
      for (i in 1:nrow(df)) {
        df$Saldo[[i]]= round(df$Saldo[[i]],2)
        
      }
      df <- as.data.table(df)
      DT::datatable(df, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
      ))
    }
  })
  
 
  
  
  
  output$resultadoMisto <- DT::renderDataTable({

    #sac
    if(input$emprestimoM=="0"||input$jurosM=="0"||input$tempoM=="0"){
      
      as.data.table(paste("Preencha os valores da coluna lateral"))
      
      
    }else{
      
      Dsac <- matrix(nrow = input$tempoM, ncol = 1)
      Psac <- matrix(nrow = input$tempoM, ncol = 1)
      Jsac <- matrix(nrow = input$tempoM, ncol = 1)
      for(l in 1:((input$tempoM)-1)){
        Asac <- input$emprestimoM/input$tempoM
        D0 <- input$emprestimoM
        Dsac[1,] <- D0-Asac
        Dsac[l+1,] <- D[l,]-Asac
        
        Jsac[1,] <- D0*(input$jurosM/100)
        Jsac[l+1,] <- Dsac[l,]*(input$jurosM/100)
        
        Psac[l,] <- Jsac[l,]+Asac
        Psac[input$tempoM,] <- Jsac[input$tempoM,]+Asac
      }
      
      Asac <- round(Asac,2)
      Jsac <- round(Jsac,2)
      Dsac <- round(Dsac,2)
      Psac <- round(Psac,2)
      
    
      
      Dprice <- matrix(nrow = input$tempoM, ncol = 1)
      Aprice <- matrix(nrow = input$tempoM, ncol = 1)
      Jprice <- matrix(nrow = input$tempoM, ncol = 1)
      Pprice <- input$emprestimoM*(((1+(input$jurosM/100))^input$tempoM)*(input$jurosM/100))/(((1+(input$jurosM/100))^input$tempoM)-1)
      D0 <- input$emprestimoM
      Jprice[1]<- D0*(input$jurosM/100)
      Aprice[1]<- Pprice-Jprice[1]
      Dprice[1]<- D0-Aprice[1]
      for(l in 2:((input$tempoM))){
        Jprice[l]=Dprice[l-1]*(input$jurosM/100)
        Aprice[l,] <- Pprice-Jprice[l,]
        Dprice[l,] <- Dprice[l-1,]-Aprice[l,]
      }
      Aprice <- round(Aprice,2)
      Jprice <- round(Jprice,2)
      Dprice <- round(Dprice,2)
      Pprice <- round(Pprice,2)
      
      Amisto <- (Aprice + Asac)/2
      Jmisto <- (Jmisto + Jsac)/2
      Dmisto <- (Dmisto + Dsac)/2
      Pmisto <- (Pprice + Psac)/2
      
      Epoca <- seq(1:input$tempoM)
      
      resultMisto <- data.frame("Parcela"=c(0, Epoca, "Soma"), 
                           "Prestação"=c("-", rep(Pmisto,input$tempoM), sum(rep(Pmisto,input$tempoM))), 
                           "Juros"=c("-", Jmisto, sum(Jmisto)), 
                           "Amortização"=c("-", Amisto, sum(Amisto)), 
                           "Divida"=c(D0, Dmisto  , "-"))
      resultMisto <- as.data.table(resultMisto)
      
      DT::datatable(resultMisto, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
      ))
      
    }
  })
      
  
  
}

shinyApp(ui, server)