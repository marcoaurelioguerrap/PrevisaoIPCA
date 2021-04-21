library(shiny)


# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Previsão de Inflação Usando Modelos de Alta Dimensionalidade"),
    navbarPage(" ",
               tabPanel("Variáveis Utilizadas",
                        column(width=2),
                        fluidRow(column(
                                   
                                   br(),
                                   p(" Aplicação de modelos de alta dimensionalidade a previsão de inflação, baseado em Garcia et. al 2017 e Medeiros et. al 2019", 
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   br(),
                                   
                                   width=8),
                                 ),
                        
                        hr(),
                        tags$style(".fa-database {color:#E87722}"),
                        h3(p(em("Variavéis Utilizadas "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                        fluidRow(column(DT::dataTableOutput("RawData"),
                                        width = 12)),
                        
                        hr(),
                        p(em("Por"),br("Marco Aurélio Guerra"),style="text-align:center; font-family: times"),
                        column(width=2)
               ),
               
               tabPanel( 'Variáveis sem transformações', 
                         
                         # Cabeçalho dos gráficos
                         hr(),
                         tags$style(".fa-chart-pie {color:#E87722}"),
                         h3(p(em("Histograma e Scatter-plot "),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                         tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                         tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}")),
                         
                         br(),
                         
                         sidebarLayout(
                           
                           # Sidebar panel for inputs ----
                           sidebarPanel(
                             varSelectInput("variable_nao_transf", "Variável:", df.final['2003/2020',]),
                             
                             
                           ),
                           
                           # Main panel for displaying outputs ----
                           mainPanel(
                             
                             fluidRow(
                               column(br(),plotOutput("data_nao_transf"),br(),width=6,style="border:1px solid black"),
                               column(br(),plotOutput("scatterplot_nao_transf"),br(),width=6,style="border: 1px solid black;border-left: none"),
                               column(br(),plotOutput("plota_serie_nao_transf"),br(),width=6,style="border: 1px solid black;border-left: none")
                             )
                             
                             
                           )
                         ),
                         
                         hr(),
                         tags$style(".glyphicon-folder-open {color:#E87722}"),
                         h3(p(em("Teste de estacionariedade  "),icon("folder-open",lib = "glyphicon"),style="color:black;text-align:center")),
                         br(),
                         verbatimTextOutput("Testando_nao_transf")
                         
                         
                         
                         
               ),
               
               tabPanel( 'Variáveis após a transformação diff-log', 
                         
                         # Cabeçalho dos gráficos
                         hr(),
                         tags$style(".fa-chart-pie {color:#E87722}"),
                         h3(p(em("Histograma e Scatter-plot "),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                         tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                         tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}")),
                         
                         br(),
                         
                         sidebarLayout(
                           
                           # Sidebar panel for inputs ----
                           sidebarPanel(
                             varSelectInput("variable", "Variável:", dados.transformados),
                             
                             
                             ),
                           
                           # Main panel for displaying outputs ----
                           mainPanel(
                             
                             fluidRow(
                               column(br(),plotOutput("data"),br(),width=6,style="border:1px solid black"),
                               column(br(),plotOutput("scatterplot"),br(),width=6,style="border: 1px solid black;border-left: none"),
                               column(br(),plotOutput("plota_serie"),br(),width=6,style="border: 1px solid black;border-left: none")
                             )
                             
                             
                           )
                         ),
                         
                         hr(),
                         tags$style(".glyphicon-folder-open {color:#E87722}"),
                         h3(p(em("Teste de estacionariedade  "),icon("folder-open",lib = "glyphicon"),style="color:black;text-align:center")),
                         br(),
                         verbatimTextOutput("Testando")
                         
                             
                             
                           
                         ),
               
               tabPanel("Resultados", 
                        
                        tableOutput("tabela"),
                        br(),
                        
                        
                        
                        ),
               
               tabPanel("Previsões",
                        column(width=2),
                        fluidRow(column(
                          
                          br(),
                          p(" Previsões do IPCA ( para diferentes horizontes ) Rolling Windows de modelos selecionados", 
                            style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                          br(),
                          
                          width=8),
                        ),
                        hr(),
                        sidebarLayout(
                          
                          # Sidebar panel for inputs ----
                          sidebarPanel(
                            varSelectInput("modelos", "modelo:", todas_previsoes_por_horizonte[[1]]),
                            varSelectInput("modelos2", "modelo:", todas_previsoes_por_horizonte[[1]])
                            
                          ),
                          
                          # Main panel for displaying outputs ----
                          mainPanel(
                            
                            fluidRow(
                              column(br(),plotOutput("previsoes"),br(),width=12,style="border:1px solid black")
                              
                            )
                            
                            
                          )
                        )
                        
                        
                        ))
)

mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

variaveis <- read.csv('Variaveis.csv',sep = ';',encoding = 'UTF-8')

colnames(variaveis) <- c('Código/font','Nome da Variavél','Unidade','Tipo')

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  #########

  #data.table::setDT(as.data.frame(df.final[complete.cases(df.final),]),keep.rownames = TRUE)[]
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      variaveis
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=43,
                   initComplete = DT::JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = colnames(variaveis)
    ))
  
  
  variavel_analisada <- reactive({
    
    attach(dados.transformados)
    
    input$variaviavel_para_analise
    
    
    
  })  
  
  
  output$data <- renderPlot({
    
    ggplot(dados.transformados, aes(!!input$variable)) + 
      geom_histogram(fill="grey55",aes(y=..density..),lwd=0.8)+geom_density(color="black",
                                                                               alpha=0.3,fill="grey48",lty=1)+
      # adciona média
      geom_vline(aes(xintercept = mean(!!input$variable, na.rm = TRUE),col='red' ))+
      
      geom_text(aes(label=round(mean(!!input$variable, na.rm = TRUE),1),y=0,x=mean(!!input$variable, na.rm = TRUE)),
                vjust=-1,col='orange',size=5) +
    
      labs(title = paste(input$variable, "\n histograma"),x=input$variable,y="Densidade")+
      theme(plot.title = element_text(size=15, face="bold.italic",hjust=0.5),
            axis.title.x = element_text( size=13, face="bold"),
            axis.title.y = element_text( size=13, face="bold"))
  })
  
  output$scatterplot <- renderPlot({
    
    ggplot(dados.transformados, aes(y = IPCA. , x = !!input$variable)) + 
      geom_point() +
      
      #geom_text(aes(label=round(mean(!!input$variable, na.rm = TRUE),1),y=0,x=mean(!!input$variable, na.rm = TRUE)),
      #          vjust=-1,col='orange',size=5) + 
      geom_abline(colour = 'red') +
      
      labs(title = paste(input$variable, "\n ScatterPlot"),x=input$variable,y="IPCA")+
      theme(plot.title = element_text(size=15, face="bold.italic",hjust=0.5),
            axis.title.x = element_text( size=13, face="bold"),
            axis.title.y = element_text( size=13, face="bold"))
  })
  
  output$Testando <- renderPrint({
    
    #shapiro.test(as.double(dados.transformados[,1]))
    aTSA::adf.test(as.ts(dados.transformados[,as.character(input$variable)]))
    #print(head(dados.transformados[,as.character(input$variable)])) 
    
  })
  
  
  output$plota_serie<- renderPlot({
    
    autoplot(dados.transformados[,as.character(input$variable)], geom = "line") +
      labs(title = paste(input$variable),x=index(dados.transformados[,as.character(input$variable)]),y="Data")+
      theme(plot.title = element_text(size=15, face="bold.italic",hjust=0.5),
            axis.title.x = element_text( size=13, face="bold"),
            axis.title.y = element_text( size=13, face="bold"))
    
  })
  
  
  output$data_nao_transf  <- renderPlot({
    
    ggplot(df.final['2003/2020',], aes(!!input$variable_nao_transf)) + 
      geom_histogram(fill="grey55",aes(y=..density..),lwd=0.8)+geom_density(color="black",
                                                                            alpha=0.3,fill="grey48",lty=1)+
      # adciona média
      geom_vline(aes(xintercept = mean(!!input$variable_nao_transf, na.rm = TRUE),col='red' ))+
      
      geom_text(aes(label=round(mean(!!input$variable_nao_transf, na.rm = TRUE),1),y=0,x=mean(!!input$variable_nao_transf, na.rm = TRUE)),
                vjust=-1,col='orange',size=5) +
      
      labs(title = paste(input$variable_nao_transf, "\n histograma"),x=input$variable,y="Densidade")+
      theme(plot.title = element_text(size=15, face="bold.italic",hjust=0.5),
            axis.title.x = element_text( size=13, face="bold"),
            axis.title.y = element_text( size=13, face="bold"))
  })
  
  output$scatterplot_nao_transf <- renderPlot({
    
    ggplot(df.final['2003/2020',], aes(y = IPCA. , x = !!input$variable_nao_transf)) + 
      geom_point() +
      
      #geom_text(aes(label=round(mean(!!input$variable_nao_transf, na.rm = TRUE),1),y=0,x=mean(!!input$variable, na.rm = TRUE)),
      #          vjust=-1,col='orange',size=5) + 
      geom_abline(colour = 'red') +
      
      labs(title = paste(input$variable_nao_transf, "\n ScatterPlot"),x=input$variable_nao_transf,y="IPCA")+
      theme(plot.title = element_text(size=15, face="bold.italic",hjust=0.5),
            axis.title.x = element_text( size=13, face="bold"),
            axis.title.y = element_text( size=13, face="bold"))
  })
  
  output$plota_serie_nao_transf<- renderPlot({
    
    autoplot(df.final['2003/2020',as.character(input$variable_nao_transf)], geom = "line") +
      labs(title = paste(input$variable_nao_transf),x=index(df.final['2003/2020',as.character(input$variable_nao_transf)]),y="Data")+
      theme(plot.title = element_text(size=15, face="bold.italic",hjust=0.5),
            axis.title.x = element_text( size=13, face="bold"),
            axis.title.y = element_text( size=13, face="bold"))
    
  })
  
  
  output$Testando_nao_transf <- renderPrint({
    
    #shapiro.test(as.double(dados.transformados[,1]))
    aTSA::adf.test(as.ts(df.final['2003/2020',as.character(input$variable_nao_transf)]))
    #print(head(dados.transformados[,as.character(input$variable)])) 
    
  })
  

  output$Conclusion1 <- renderText({
    
    if(testanalitico()$p.value < 0.05){mensaje="We reject the null hypothesis, the variable is not normally distributed"}else{mensaje="We keep the null hypothesis, the variable is normally distributed"}
    mensaje
    
  })
  
  output$tabela <- function(){
  
    kbl(tabela.erros , 
        caption = "Tabela - Indicadores (RMSE e MAE) por horizonte para estratégias selecionadas",
        align = "c", digits = 2, format = "html",
        col.names = c('modelo',paste0('t + ',1:13)), escape = F ) %>%
      kable_classic(full_width = F, html_font = "Cambria") %>%
      column_spec(1, bold = FALSE) %>%
      collapse_rows(columns = 1, valign = "top") %>%
      footnote(general = "Os números em parentesis são os valores do erro médio absoluto o restante são as raize quadrática média dos erros",
               general_title = "Nota: "
      ) 
  
  }
  
  
  
  output$previsoes <- renderPlot({
    ggplot() + 
      geom_line(data = df_empilhado_todos_horizontes_previsoes_ipca[which(df_empilhado_todos_horizontes_previsoes_ipca$ind == 'IPCA.' |
                                                                            df_empilhado_todos_horizontes_previsoes_ipca$ind == as.character(input$modelos) | 
                                                                            df_empilhado_todos_horizontes_previsoes_ipca$ind == as.character(input$modelos2)),],
                aes(x = Index , y= values , color = ind)) + facet_wrap(~horizonte)
    
  })
  
  
  #### FIM #####

  
}


