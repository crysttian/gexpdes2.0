# Define server logic ----
server <- function(input, output) {
  
  dados <- NULL
  
  #Escondendo a tab de Analise de Regressao
  hideTab(inputId="tabs_analise", target="Análise de Regressão", session = getDefaultReactiveDomain())
  
  #Selecionando as guias na interface
  observeEvent(input$ir_inicio, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelInicio')
    
  })
  
  observeEvent(input$ir_inicio_origem, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelInicio')
    
  })
  
  observeEvent(input$ir_inicio_croqui, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelInicio')
    
  })
  
  observeEvent(input$ir_inicio_analise, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelInicio')
    
    #Escondendo a tab de Analise de Regressao
    hideTab(inputId="tabs_analise", target="Análise de Regressão", session = getDefaultReactiveDomain())
    
  })  
  
  observeEvent(input$ir_inicio_ajuda, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelInicio')
    
  }) 
  
  observeEvent(input$ir_inicio_equipe, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelInicio')
    
  }) 
  
  
  observeEvent(input$ir_dados, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelDados')
    
    if(input$radio_origem=="arquivo"){
      
      updateTabsetPanel(inputId = "tabs_importar",selected = 'panel_importar')
      
      updateTabsetPanel(inputId = "painel_dados",selected = 'tab_arquivo')
      
    }
    
    if(input$radio_origem=="labestdata"){
      
      updateTabsetPanel(inputId = "tabs_importar",selected = 'panel_labestdata')
      
      updateTabsetPanel(inputId = "painel_dados",selected = 'tab_labestData')
     
    }
    
    if(input$radio_origem=="gerar"){
      
      updateTabsetPanel(inputId = "tabs_importar",selected = 'panel_gerar')
      
    }    
    
  })
  
  observeEvent(input$ir_dados_delineamento, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelDados')
    
  })  
  
  observeEvent(input$ir_origem, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelOrigem')
    
  })  
  
  observeEvent(input$ir_origem_dados, {
    
    #reset dos inputfile
    shinyjs::reset("arquivo_csv")
    shinyjs::reset("arquivo_excel")
    shinyjs::reset("arquivo_ods")
    shinyjs::reset("arquivo_lab")

    if(!is.null(dados)){
      
      dados <<- NULL
      
    }
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelOrigem')
    
  }) 
  
  observeEvent(input$ir_delineamento, {

    if(is.null(dados)){
      
      shinyalert("Oops!", "Selecione uma base de dados", type = "error")
      
    }else{

      updateTabsetPanel(inputId = "tabs",selected = 'panelDelineamento')
      
      #DIC
      updateSelectInput(input = "trat_DIC", choices = colnames(dados))
      updateSelectInput(input = "resp_DIC", choices = colnames(dados))
      
      #DBC
      updateSelectInput(input = "trat_DBC", choices = colnames(dados))
      updateSelectInput(input = "bloco_DBC", choices = colnames(dados))
      updateSelectInput(input = "resp_DBC", choices = colnames(dados))
      
      #DQL
      updateSelectInput(input = "trat_DQL", choices = colnames(dados))
      updateSelectInput(input = "linha_DQL", choices = colnames(dados))
      updateSelectInput(input = "coluna_DQL", choices = colnames(dados))
      updateSelectInput(input = "resp_DQL", choices = colnames(dados))
      
      #FAIXAS
      updateSelectInput(input = "fator1_faixas", choices = colnames(dados))
      updateSelectInput(input = "fator2_faixas", choices = colnames(dados))
      updateSelectInput(input = "bloco_faixas", choices = colnames(dados))
      updateSelectInput(input = "resp_faixas", choices = colnames(dados))     
      
      #FAT2.DIC
      updateSelectInput(input = "fator1_FAT2_DIC", choices = colnames(dados))
      updateSelectInput(input = "fator2_FAT2_DIC", choices = colnames(dados))
      updateSelectInput(input = "resp_FAT2_DIC", choices = colnames(dados))
      
      #FAT2.DBC
      updateSelectInput(input = "fator1_FAT2_DBC", choices = colnames(dados))
      updateSelectInput(input = "fator2_FAT2_DBC", choices = colnames(dados))
      updateSelectInput(input = "bloco_FAT2_DBC", choices = colnames(dados))
      updateSelectInput(input = "resp_FAT2_DBC", choices = colnames(dados))
      
      #FAT3.DIC
      updateSelectInput(input = "fator1_FAT3_DIC", choices = colnames(dados))
      updateSelectInput(input = "fator2_FAT3_DIC", choices = colnames(dados))
      updateSelectInput(input = "fator3_FAT3_DIC", choices = colnames(dados))
      updateSelectInput(input = "resp_FAT3_DIC", choices = colnames(dados))
      
      #FAT3.DBC
      updateSelectInput(input = "fator1_FAT3_DBC", choices = colnames(dados))
      updateSelectInput(input = "fator2_FAT3_DBC", choices = colnames(dados))
      updateSelectInput(input = "fator3_FAT3_DBC", choices = colnames(dados))
      updateSelectInput(input = "bloco_FAT3_DBC", choices = colnames(dados))
      updateSelectInput(input = "resp_FAT3_DBC", choices = colnames(dados))
      
      #FAT2AD_DIC
      updateSelectInput(input = "fator1_FAT2AD_DIC", choices = colnames(dados))
      updateSelectInput(input = "fator2_FAT2AD_DIC", choices = colnames(dados))
      updateSelectInput(input = "repet_FAT2AD_DIC", choices = colnames(dados))
      updateSelectInput(input = "resp_FAT2AD_DIC", choices = colnames(dados))
      
      #FAT2AD_DBC
      updateSelectInput(input = "fator1_FAT2AD_DBC", choices = colnames(dados))
      updateSelectInput(input = "fator2_FAT2AD_DBC", choices = colnames(dados))
      updateSelectInput(input = "bloco_FAT2AD_DBC", choices = colnames(dados))
      updateSelectInput(input = "resp_FAT2AD_DBC", choices = colnames(dados))
      
      #FAT2AD2_DIC
      updateSelectInput(input = "fator1_FAT2AD2_DIC", choices = colnames(dados))
      updateSelectInput(input = "fator2_FAT2AD2_DIC", choices = colnames(dados))
      updateSelectInput(input = "repet_FAT2AD2_DIC", choices = colnames(dados))
      updateSelectInput(input = "resp_FAT2AD2_DIC", choices = colnames(dados))
      
      #FAT2AD2_DBC
      updateSelectInput(input = "fator1_FAT2AD2_DBC", choices = colnames(dados))
      updateSelectInput(input = "fator2_FAT2AD2_DBC", choices = colnames(dados))
      updateSelectInput(input = "bloco_FAT2AD2_DBC", choices = colnames(dados))
      updateSelectInput(input = "resp_FAT2AD2_DBC", choices = colnames(dados))      
      
      #FAT3AD_DIC
      updateSelectInput(input = "fator1_FAT3AD_DIC", choices = colnames(dados))
      updateSelectInput(input = "fator2_FAT3AD_DIC", choices = colnames(dados))
      updateSelectInput(input = "fator3_FAT3AD_DIC", choices = colnames(dados))
      updateSelectInput(input = "repet_FAT3AD_DIC", choices = colnames(dados))
      updateSelectInput(input = "resp_FAT3AD_DIC", choices = colnames(dados))
      
      #FAT3AD_DBC
      updateSelectInput(input = "fator1_FAT3AD_DBC", choices = colnames(dados))
      updateSelectInput(input = "fator2_FAT3AD_DBC", choices = colnames(dados))
      updateSelectInput(input = "fator3_FAT3AD_DBC", choices = colnames(dados))
      updateSelectInput(input = "bloco_FAT3AD_DBC", choices = colnames(dados))
      updateSelectInput(input = "resp_FAT3AD_DBC", choices = colnames(dados))
      
      #PSUB_DIC
      updateSelectInput(input = "fator1_PSUB_DIC", choices = colnames(dados))
      updateSelectInput(input = "fator2_PSUB_DIC", choices = colnames(dados))
      updateSelectInput(input = "repet_PSUB_DIC", choices = colnames(dados))
      updateSelectInput(input = "resp_PSUB_DIC", choices = colnames(dados))
      
      #PSUB_DBC
      updateSelectInput(input = "fator1_PSUB_DBC", choices = colnames(dados))
      updateSelectInput(input = "fator2_PSUB_DBC", choices = colnames(dados))
      updateSelectInput(input = "bloco_PSUB_DBC", choices = colnames(dados))
      updateSelectInput(input = "resp_PSUB_DBC", choices = colnames(dados))
      
    }
    
  })  
  
  observeEvent(input$ir_delineamento_analise, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelDelineamento')
    
    #Escondendo a tab de Analise de Regressao
    hideTab(inputId="tabs_analise", target="Análise de Regressão", session = getDefaultReactiveDomain())
    
  })   

  #### Funcao importada do pacote expdes.pt para desativar a opcao dev.new()
  graficos_reg = function (a, grau = 1, mod = TRUE, main = " ", sub = " ", xlab = "Niveis (X)", 
                           ylab = "Resposta (Y)", pch = 19, xlim = NULL, ylim = NULL, 
                           bty = "o") 
  {
    a <- a$reg
    xob <- as.numeric(as.vector(a$"Quadro de medias"[, 1]))
    x <- seq(min(xob), max(xob), by = 0.1)
    if (grau == 1 && is.numeric(a$"Coeficientes reta")) {
      #dev.new()
      b0 <- a$"Coeficientes reta"[1]
      b1 <- a$"Coeficientes reta"[2]
      y <- b0 + b1 * x
      yob <- as.numeric(as.vector(a$"Quadro de medias"[, 2]))
      if (is.null(ylim) == TRUE) 
        ylim = c(min(y, yob), max(y, yob))
      plot(x, y, "l", main = main, sub = sub, bty = bty, xlab = xlab, 
           ylab = ylab, xlim = xlim, ylim = ylim)
      if (mod == TRUE) 
        mtext(paste("y =", round(b0, 3), "+", round(b1, 3), 
                    "x  ", " R^2 = ", round(a$"R2 reta" * 100, 2), 
                    "%"), side = 3)
      points(xob, yob, pch = pch)
    }
    if (grau == 2 && is.numeric(a$"Coeficientes parabola")) {
      #dev.new()
      b0 <- a$"Coeficientes parabola"[1]
      b1 <- a$"Coeficientes parabola"[2]
      b2 <- a$"Coeficientes parabola"[3]
      y <- b0 + b1 * x + b2 * x^2
      yob <- as.numeric(as.vector(a$"Quadro de medias"[, 2]))
      if (is.null(ylim) == TRUE) 
        ylim = c(min(y, yob), max(y, yob))
      plot(x, y, "l", main = main, sub = sub, bty = bty, xlab = xlab, 
           ylab = ylab, xlim = xlim, ylim = ylim)
      if (mod == TRUE) 
        mtext(paste("y = ", round(b0, 3), "+", round(b1, 
                                                     3), "x+", round(b2, 3), "x^2  ", " R^2 = ", round(a$"R2 parabola" * 
                                                                                                         100, 2), "%"), side = 3)
      points(xob, yob, pch = pch)
    }
    if (grau == 3 && is.numeric(a$"Coeficientes cubica")) {
      #dev.new()
      b0 <- a$"Coeficientes cubica"[1]
      b1 <- a$"Coeficientes cubica"[2]
      b2 <- a$"Coeficientes cubica"[3]
      b3 <- a$"Coeficientes cubica"[4]
      y <- b0 + b1 * x + b2 * x^2 + b3 * x^3
      yob <- as.numeric(as.vector(a$"Quadro de medias"[, 2]))
      if (is.null(ylim) == TRUE) 
        ylim = c(min(y, yob), max(y, yob))
      plot(x, y, "l", main = main, sub = sub, bty = bty, xlab = xlab, 
           ylab = ylab, xlim = xlim, ylim = ylim)
      if (mod == TRUE) 
        mtext(paste("y = ", round(b0, 3), "+", round(b1, 
                                                     3), "x+", round(b2, 3), "x^2+", round(b3, 3), 
                    "x^3  ", " R^2 = ", round(a$"R2 cubica" * 100, 
                                              2), "%"), side = 3)
      points(xob, yob, pch = pch)
    }
    if (grau == "pot" && is.numeric(a$"Coeficientes modelo potencia")) {
      if (is.numeric(a$"Coeficientes modelo potencia") == FALSE) {
        print(a$"AIC modelo potencia")
      }
      #dev.new()
      b0 <- a$"Coeficientes modelo potencia"[1]
      b1 <- a$"Coeficientes modelo potencia"[2]
      y <- b0 * x^b1
      yob <- as.numeric(as.vector(a$"Quadro de medias"[, 2]))
      if (is.null(ylim) == TRUE) 
        ylim = c(min(y, yob), max(y, yob))
      plot(x, y, "l", main = main, sub = sub, bty = bty, xlab = xlab, 
           ylab = ylab, xlim = xlim, ylim = ylim)
      if (mod == TRUE) 
        mtext(paste("y = ", round(b0, 3), "* x^", round(b1, 
                                                        3), " R^2 aprox = ", round(a$"R2 aprox modelo potencia" * 
                                                                                     100, 2), "%"), side = 3)
      points(xob, yob, pch = pch)
    }
    if (grau == "exp" && is.numeric(a$"Coeficientes modelo exponencial")) {
      if (is.numeric(a$"Coeficientes modelo exponencial") == 
          FALSE) {
        print(a$"AIC modelo exponencial")
      }
      #dev.new()
      b0 <- a$"Coeficientes modelo exponencial"[1]
      b1 <- a$"Coeficientes modelo exponencial"[2]
      y <- b0 * exp(b1 * x)
      yob <- as.numeric(as.vector(a$"Quadro de medias"[, 2]))
      if (is.null(ylim) == TRUE) 
        ylim = c(min(y, yob), max(y, yob))
      plot(x, y, "l", main = main, sub = sub, bty = bty, xlab = xlab, 
           ylab = ylab, xlim = xlim, ylim = ylim)
      if (mod == TRUE) 
        mtext(paste("y = ", round(b0, 3), "* exp(", round(b1, 
                                                          3), "x)   R^2 aprox = ", round(a$"R2 aprox modelo exponencial" * 
                                                                                           100, 2), "%"), side = 3)
      points(xob, yob, pch = pch)
    }
    if (grau == "log" && is.numeric(a$"Coeficientes modelo logistico")) {
      if (is.numeric(a$"Coeficientes modelo logistico") == 
          FALSE) {
        print(a$"AIC modelo logistico")
      }
      #dev.new()
      b0 <- a$"Coeficientes modelo logistico"[1]
      b1 <- a$"Coeficientes modelo logistico"[2]
      b2 <- a$"Coeficientes modelo logistico"[3]
      y <- b0/(1 + exp(b1 - (b2 * x)))
      yob <- as.numeric(as.vector(a$"Quadro de medias"[, 2]))
      if (is.null(ylim) == TRUE) 
        ylim = c(min(y, yob), max(y, yob))
      plot(x, y, "l", main = main, sub = sub, bty = bty, xlab = xlab, 
           ylab = ylab, xlim = xlim, ylim = ylim)
      if (mod == TRUE) 
        mtext(paste("y = ", round(b0, 3), "/(1+exp(", round(b1, 
                                                            3), "-(", round(b2, 3), "*x)))   R^2 aprox = ", 
                    round(a$"R2 aprox modelo logistico" * 100, 2), 
                    "%"), side = 3)
      points(xob, yob, pch = pch)
    }
    if (grau == "gomp" && is.numeric(a$"Coeficientes modelo Gompertz")) {
      if (is.numeric(a$"Coeficientes modelo Gompertz") == FALSE) {
        print(a$"AIC modelo Gompertz")
      }
      #dev.new()
      b0 <- a$"Coeficientes modelo Gompertz"[1]
      b1 <- a$"Coeficientes modelo Gompertz"[2]
      b2 <- a$"Coeficientes modelo Gompertz"[3]
      y <- b0 * exp(-exp(b1 - (b2 * x)))
      yob <- as.numeric(as.vector(a$"Quadro de medias"[, 2]))
      if (is.null(ylim) == TRUE) 
        ylim = c(min(y, yob), max(y, yob))
      plot(x, y, "l", main = main, sub = sub, bty = bty, xlab = xlab, 
           ylab = ylab, xlim = xlim, ylim = ylim)
      if (mod == TRUE) 
        mtext(paste("y = ", round(b0, 3), "*exp(-exp(", round(b1, 
                                                              3), "-(", round(b2, 3), "*x)))   R^2 aprox = ", 
                    round(a$"R2 aprox modelo Gompertz" * 100, 2), 
                    "%"), side = 3)
      points(xob, yob, pch = pch)
    }
  }
  ###
  
  #Funcoes para calcular os delineamentos
  
  #DIC
  atualizarDIC = function(){
    
    output$saida_analise <- renderPrint({
      
      if(!is.null(dados)){
        
       modelo_dic = dic(trat = eval(parse(text = paste("dados$",input$trat_DIC,sep=""))),
                    resp = eval(parse(text = paste("dados$",input$resp_DIC,sep=""))), 
                    quali = eval(parse(text = input$quali_DIC)),
                    mcomp = input$mcomp_analise_DIC, 
                    nl = eval(parse(text = input$nl_analise_DIC)),
                    hvar = input$hvar_analise_DIC,
                    sigT = input$sigT_analise_DIC,
                    sigF = input$sigF_analise_DIC,
                    unfold = switch(input$unfold_analise_DIC, "Nenhum"=NULL,input$unfold_analise_DIC))
       
        #se for para fazer a análise de regressão 
        if(eval(parse(text = input$quali_DIC))==FALSE){
          
          showTab(inputId="tabs_analise", target="Análise de Regressão", session = getDefaultReactiveDomain())
          
          output$analise_regressao_dic_graf1 = renderPlot({
            
            graficos_reg(
              modelo_dic,
              grau = 1,
              mod = TRUE,
              main = " ",
              sub = " ",
              xlab = paste("Niveis(",input$trat_DIC,")",sep=""),
              ylab = paste("Resposta (",input$resp_DIC,")",sep=""),
              pch = 19,
              xlim = NULL,
              ylim = NULL,
              bty = "o"
            )
            
          })
          
          output$analise_regressao_dic_graf2 = renderPlot({
            
            graficos_reg(
              modelo_dic,
              grau = 2,
              mod = TRUE,
              main = " ",
              sub = " ",
              xlab = paste("Niveis(",input$trat_DIC,")",sep=""),
              ylab = paste("Resposta (",input$resp_DIC,")",sep=""),
              pch = 19,
              xlim = NULL,
              ylim = NULL,
              bty = "o"
            )
            
          })
          
          output$analise_regressao_dic_graf3 = renderPlot({
            
            graficos_reg(
              modelo_dic,
              grau = 3,
              mod = TRUE,
              main = " ",
              sub = " ",
              xlab = paste("Niveis(",input$trat_DIC,")",sep=""),
              ylab = paste("Resposta (",input$resp_DIC,")",sep=""),
              pch = 19,
              xlim = NULL,
              ylim = NULL,
              bty = "o"
            )
            
          })
          
        }
        
       
       
      }
    })
    
    output$analise_residuo = renderPlot({
      
      modelo_dic = dic(trat = eval(parse(text = paste("dados$",input$trat_DIC,sep=""))),
                         resp = eval(parse(text = paste("dados$",input$resp_DIC,sep=""))), 
                         quali = eval(parse(text = input$quali_DIC)),
                         mcomp = input$mcomp_analise_DIC, 
                         nl = eval(parse(text = input$nl_analise_DIC)),
                         hvar = input$hvar_analise_DIC,
                         sigT = input$sigT_analise_DIC,
                         sigF = input$sigF_analise_DIC,
                         unfold = switch(input$unfold_analise_DIC, "Nenhum"=NULL,input$unfold_analise_DIC))
      
      plotres(modelo_dic)
      
      })

     output$analise_boxplot_dic = plotly::renderPlotly({
      
      if(eval(parse(text = input$quali_DIC))==FALSE){
        
        showTab(inputId="tabs_analise", target="Análise de Regressão", session = getDefaultReactiveDomain())
      }
      
      figura = plot_ly(dados, 
              y=eval(parse(text = paste("dados$",input$resp_DIC,sep=""))), 
              x=eval(parse(text = paste("dados$",input$trat_DIC,sep=""))),
              boxpoints = "all",
              type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Tratamento:",input$trat_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_DIC)))
      
      figura
      
    })
    
  }
  
  #DBC
  atualizarDBC = function(){
    
    output$saida_analise <- renderPrint({

      if(!is.null(dados)){
        
        modelo_dbc = dbc(trat = eval(parse(text = paste("dados$",input$trat_DBC,sep=""))),
            bloco = eval(parse(text = paste("dados$",input$bloco_DBC,sep=""))),
            resp = eval(parse(text = paste("dados$",input$resp_DBC,sep=""))), 
            quali = eval(parse(text = input$quali_DBC)),
            mcomp = input$mcomp_analise_DBC, 
            nl = eval(parse(text = input$nl_analise_DBC)),
            hvar = input$hvar_analise_DBC,
            sigT = input$sigT_analise_DBC,
            sigF = input$sigF_analise_DBC,
            unfold = switch(input$unfold_analise_DBC, "Nenhum"=NULL,input$unfold_analise_DBC))
        
        if(eval(parse(text = input$quali_DBC))==FALSE){
          
          showTab(inputId="tabs_analise", target="Análise de Regressão", session = getDefaultReactiveDomain())
          
          print("reg")
          print(input$quali_DBC)
          
          output$analise_regressao_dbc_graf1 = renderPlot({
            
            graficos_reg(
              modelo_dbc,
              grau = 1,
              mod = TRUE,
              main = " ",
              sub = " ",
              xlab = paste("Niveis(",input$trat_DBC,")",sep=""),
              ylab = paste("Resposta (",input$resp_DBC,")",sep=""),
              pch = 19,
              xlim = NULL,
              ylim = NULL,
              bty = "o"
            )
            
          })
          
          output$analise_regressao_dbc_graf2 = renderPlot({
            
            graficos_reg(
              modelo_dbc,
              grau = 2,
              mod = TRUE,
              main = " ",
              sub = " ",
              xlab = paste("Niveis(",input$trat_DBC,")",sep=""),
              ylab = paste("Resposta (",input$resp_DBC,")",sep=""),
              pch = 19,
              xlim = NULL,
              ylim = NULL,
              bty = "o"
            )
            
          })
          
          output$analise_regressao_dbc_graf3 = renderPlot({
            
            graficos_reg(
              modelo_dbc,
              grau = 3,
              mod = TRUE,
              main = " ",
              sub = " ",
              xlab = paste("Niveis(",input$trat_DBC,")",sep=""),
              ylab = paste("Resposta (",input$resp_DBC,")",sep=""),
              pch = 19,
              xlim = NULL,
              ylim = NULL,
              bty = "o"
            )
            
          })
          
        }
        
      }
    })
    
    output$analise_residuo = renderPlot({
      
      modelo_dbc = dbc(trat = eval(parse(text = paste("dados$",input$trat_DBC,sep=""))),
                         bloco = eval(parse(text = paste("dados$",input$bloco_DBC,sep=""))),
                         resp = eval(parse(text = paste("dados$",input$resp_DBC,sep=""))), 
                         quali = eval(parse(text = input$quali_DBC)),
                         mcomp = input$mcomp_analise_DBC, 
                         nl = eval(parse(text = input$nl_analise_DBC)),
                         hvar = input$hvar_analise_DBC,
                         sigT = input$sigT_analise_DBC,
                         sigF = input$sigF_analise_DBC,
                         unfold = switch(input$unfold_analise_DBC, "Nenhum"=NULL,input$unfold_analise_DBC))
      
      plotres(modelo_dbc)
      
      })
   
    output$analise_boxplot_dbc = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$trat_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Tratamento:",input$trat_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_DBC)))
      
      figura
      
    })
     
    output$analise_boxplot_bloco_dbc = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$bloco_DBC,sep=""))),
                       boxpoints = "all",
                       type="box",
                       color=I("blue")) %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Bloco:",input$bloco_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_DBC)))
      
      figura
      
    })
    
  }

  #DQL
  atualizarDQL = function(){
    
    output$saida_analise <- renderPrint({
      
      if(!is.null(dados)){
        
        modelo_dql = dql(trat = eval(parse(text = paste("dados$",input$trat_DQL,sep=""))),
            linha = eval(parse(text = paste("dados$",input$linha_DQL,sep=""))),
            coluna = eval(parse(text = paste("dados$",input$coluna_DQL,sep=""))),
            resp = eval(parse(text = paste("dados$",input$resp_DQL,sep=""))), 
            quali = eval(parse(text = input$quali_DQL)),
            mcomp = input$mcomp_analise_DQL, 
            sigT = input$sigT_analise_DQL,
            sigF = input$sigF_analise_DQL,
            unfold = switch(input$unfold_analise_DQL, "Nenhum"=NULL,input$unfold_analise_DQL))
        
        if(eval(parse(text = input$quali_DQL))==FALSE){
          
          showTab(inputId="tabs_analise", target="Análise de Regressão", session = getDefaultReactiveDomain())

          output$analise_regressao_dql_graf1 = renderPlot({
            
            graficos_reg(
              modelo_dql,
              grau = 1,
              mod = TRUE,
              main = " ",
              sub = " ",
              xlab = paste("Niveis(",input$trat_DQL,")",sep=""),
              ylab = paste("Resposta (",input$resp_DQL,")",sep=""),
              pch = 19,
              xlim = NULL,
              ylim = NULL,
              bty = "o"
            )
            
          })
          
          output$analise_regressao_dql_graf2 = renderPlot({
            
            graficos_reg(
              modelo_dql,
              grau = 2,
              mod = TRUE,
              main = " ",
              sub = " ",
              xlab = paste("Niveis(",input$trat_DQL,")",sep=""),
              ylab = paste("Resposta (",input$resp_DQL,")",sep=""),
              pch = 19,
              xlim = NULL,
              ylim = NULL,
              bty = "o"
            )
            
          })
          
          output$analise_regressao_dql_graf3 = renderPlot({
            
            graficos_reg(
              modelo_dql,
              grau = 3,
              mod = TRUE,
              main = " ",
              sub = " ",
              xlab = paste("Niveis(",input$trat_DQL,")",sep=""),
              ylab = paste("Resposta (",input$resp_DQL,")",sep=""),
              pch = 19,
              xlim = NULL,
              ylim = NULL,
              bty = "o"
            )
            
          })
          
        }
      }
    })
    
    output$analise_residuo = renderPlot({
      
      modelo_dql = dql(trat = eval(parse(text = paste("dados$",input$trat_DQL,sep=""))),
                         linha = eval(parse(text = paste("dados$",input$linha_DQL,sep=""))),
                         coluna = eval(parse(text = paste("dados$",input$coluna_DQL,sep=""))),
                         resp = eval(parse(text = paste("dados$",input$resp_DQL,sep=""))), 
                         quali = eval(parse(text = input$quali_DQL)),
                         mcomp = input$mcomp_analise_DQL, 
                         sigT = input$sigT_analise_DQL,
                         sigF = input$sigF_analise_DQL,
                         unfold = switch(input$unfold_analise_DQL, "Nenhum"=NULL,input$unfold_analise_DQL))
      
      plotres(modelo_dql)
      
      })
    
    output$analise_boxplot_dql = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_DQL,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$trat_DQL,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Tratamento:",input$trat_DQL,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_DQL)))
      
      figura
      
    })
    
    output$analise_boxplot_linha_dql = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_DQL,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$linha_DQL,sep=""))),
                       boxpoints = "all",
                       type="box",
                       color=I("blue")) %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Linha:",input$linha_DQL,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_DQL)))
      
      figura
      
    })
    
    output$analise_boxplot_coluna_dql = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_DQL,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$coluna_DQL,sep=""))),
                       boxpoints = "all",
                       type="box",
                       color=I("magenta")) %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Coluna:",input$coluna_DQL,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_DQL)))
      
      figura
      
    })
    
  }
  
  #FAIXAS
  atualizarfaixas = function(){
    
    output$saida_analise <- renderPrint({
      
      if(!is.null(dados)){
        faixas(fator1 = eval(parse(text = paste("dados$",input$fator1_faixas,sep=""))),
            fator2 = eval(parse(text = paste("dados$",input$fator2_faixas,sep=""))),
            bloco = eval(parse(text = paste("dados$",input$bloco_faixas,sep=""))),
            resp = eval(parse(text = paste("dados$",input$resp_faixas,sep=""))), 
            quali = c(eval(parse(text = input$quali_fator1_faixas)),eval(parse(text = input$quali_fator2_faixas))),
            mcomp = input$mcomp_analise_faixas,
            fac.names = c(input$fac.names_faixas_f1,input$fac.names_faixas_f2),
            sigT = input$sigT_analise_faixas,
            sigF = input$sigF_analise_faixas,
            unfold = switch(input$unfold_analise_faixas, "Nenhum"=NULL,input$unfold_analise_faixas))
      }
    })
    
    output$analise_residuo = renderPlot({
      
      modelo_faixas = faixas(fator1 = eval(parse(text = paste("dados$",input$fator1_faixas,sep=""))),
                               fator2 = eval(parse(text = paste("dados$",input$fator2_faixas,sep=""))),
                               bloco = eval(parse(text = paste("dados$",input$bloco_faixas,sep=""))),
                               resp = eval(parse(text = paste("dados$",input$resp_faixas,sep=""))), 
                               quali = c(eval(parse(text = input$quali_fator1_faixas)),eval(parse(text = input$quali_fator2_faixas))),
                               mcomp = input$mcomp_analise_faixas,
                               fac.names = c(input$fac.names_faixas_f1,input$fac.names_faixas_f2),
                               sigT = input$sigT_analise_faixas,
                               sigF = input$sigF_analise_faixas,
                               unfold = switch(input$unfold_analise_faixas, "Nenhum"=NULL,input$unfold_analise_faixas))
      
      plotres(modelo_faixas)
      
      })
    
    output$analise_boxplot_faixas_fator1 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_faixas,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator1_faixas,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$trat_faixas,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_faixas)))
      
      figura
      
    })
    
    output$analise_boxplot_faixas_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_faixas,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_faixas,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_faixas,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_faixas)))
      
      figura
      
    })

    output$analise_boxplot_faixas_bloco = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_faixas,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$bloco_faixas,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Blocos:",input$bloco_faixas,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_faixas)))
      
      figura
      
    })
    
  }
  
  #FAT2_DIC
  atualizarFAT2_DIC = function(){

    output$saida_analise <- renderPrint({
      
      if(!is.null(dados)){
        fat2.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2_DIC,sep=""))),
               fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2_DIC,sep=""))),
               resp = eval(parse(text = paste("dados$",input$resp_FAT2_DIC,sep=""))), 
               quali = c(eval(parse(text = input$quali_fator1_FAT2_DIC)),eval(parse(text = input$quali_fator2_FAT2_DIC))),
               mcomp = input$mcomp_analise_FAT2_DIC,
               fac.names = c(input$fac.names_FAT2_DIC_f1,input$fac.names_FAT2_DIC_f2),
               sigT = input$sigT_analise_FAT2_DIC,
               sigF = input$sigF_analise_FAT2_DIC,
               unfold = switch(input$unfold_analise_FAT2_DIC, "Nenhum"=NULL,input$unfold_analise_FAT2_DIC)
        )
      }
    })
    
    output$analise_residuo = renderPlot({
      
      modelo_fat2.dic = fat2.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2_DIC,sep=""))),
                                 fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2_DIC,sep=""))),
                                 resp = eval(parse(text = paste("dados$",input$resp_FAT2_DIC,sep=""))), 
                                 quali = c(eval(parse(text = input$quali_fator1_FAT2_DIC)),eval(parse(text = input$quali_fator2_FAT2_DIC))),
                                 mcomp = input$mcomp_analise_FAT2_DIC,
                                 fac.names = c(input$fac.names_FAT2_DIC_f1,input$fac.names_FAT2_DIC_f2),
                                 sigT = input$sigT_analise_FAT2_DIC,
                                 sigF = input$sigF_analise_FAT2_DIC,
                                 unfold = switch(input$unfold_analise_FAT2_DIC, "Nenhum"=NULL,input$unfold_analise_FAT2_DIC))
      
      plotres(modelo_fat2.dic)
      
    })
    
    output$analise_boxplot_FAT2_DIC_fator1 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator1_FAT2_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator1_FAT2_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2_DIC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT2_DIC_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_FAT2_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_FAT2_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2_DIC)))
      
      figura
      
    })
    
    output$analise_interacao_FAT2_DIC_fator1 = renderPlot({
    
        interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT2_DIC,sep=""))),
                         trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT2_DIC,sep=""))),
                         response = eval(parse(text = paste("dados$",input$resp_FAT2_DIC,sep=""))),
                         trace.label = input$fator2_FAT2_DIC,
                         axes = FALSE,
                         xlab = paste("Fator:",input$fator1_FAT2_DIC,sep=" "),
                         ylab = paste("Média da Variável Resposta:",input$resp_FAT2_DIC,sep=" "))
        axis(side = 1)
        axis(side = 2)
        
    })    
    
    output$analise_interacao_FAT2_DIC_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT2_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT2_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2_DIC,sep=""))),
                       trace.label = input$fator1_FAT2_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT2_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    }) 
    
  }

  #FAT2_DBC
  atualizarFAT2_DBC = function(){
    
    output$saida_analise <- renderPrint({
      
      if(!is.null(dados)){
        fat2.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2_DBC,sep=""))),
                 fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2_DBC,sep=""))),
                 bloco = eval(parse(text = paste("dados$",input$bloco_FAT2_DBC,sep=""))),
                 resp = eval(parse(text = paste("dados$",input$resp_FAT2_DBC,sep=""))), 
                 quali = c(eval(parse(text = input$quali_fator1_FAT2_DBC)),eval(parse(text = input$quali_fator2_FAT2_DBC))),
                 mcomp = input$mcomp_analise_FAT2_DBC,
                 fac.names = c(input$fac.names_FAT2_DBC_f1,input$fac.names_FAT2_DBC_f2),
                 sigT = input$sigT_analise_FAT2_DBC,
                 sigF = input$sigF_analise_FAT2_DBC,
                 unfold = switch(input$unfold_analise_FAT2_DBC, "Nenhum"=NULL,input$unfold_analise_FAT2_DBC)
        )
      }
    })
    
    output$analise_residuo = renderPlot({
      
      modelo_fat2.dbc = fat2.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2_DBC,sep=""))),
                                 fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2_DBC,sep=""))),
                                 bloco = eval(parse(text = paste("dados$",input$bloco_FAT2_DBC,sep=""))),
                                 resp = eval(parse(text = paste("dados$",input$resp_FAT2_DBC,sep=""))), 
                                 quali = c(eval(parse(text = input$quali_fator1_FAT2_DBC)),eval(parse(text = input$quali_fator2_FAT2_DBC))),
                                 mcomp = input$mcomp_analise_FAT2_DBC,
                                 fac.names = c(input$fac.names_FAT2_DBC_f1,input$fac.names_FAT2_DBC_f2),
                                 sigT = input$sigT_analise_FAT2_DBC,
                                 sigF = input$sigF_analise_FAT2_DBC,
                                 unfold = switch(input$unfold_analise_FAT2_DBC, "Nenhum"=NULL,input$unfold_analise_FAT2_DBC))
      
      plotres(modelo_fat2.dbc)
      
    })
    
    output$analise_boxplot_FAT2_DBC_fator1 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator1_FAT2_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator1_FAT2_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2_DBC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT2_DBC_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_FAT2_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_FAT2_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2_DBC)))
      
      figura
      
    })

    output$analise_boxplot_FAT2_DBC_bloco = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$bloco_FAT2_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$bloco_FAT2_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2_DBC)))
      
      figura
      
    })    
      
    output$analise_interacao_FAT2_DBC_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT2_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT2_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2_DBC,sep=""))),
                       trace.label = input$fator2_FAT2_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT2_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_FAT2_DBC_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT2_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT2_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2_DBC,sep=""))),
                       trace.label = input$fator1_FAT2_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT2_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    }) 
      
  }
  
  #FAT3_DIC
  atualizarFAT3_DIC = function(){
    
    output$saida_analise <- renderPrint({
      
      if(!is.null(dados)){
        fat3.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT3_DIC,sep=""))),
                 fator2 = eval(parse(text = paste("dados$",input$fator2_FAT3_DIC,sep=""))),
                 fator3 = eval(parse(text = paste("dados$",input$fator3_FAT3_DIC,sep=""))),
                 resp = eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))), 
                 quali = c(eval(parse(text = input$quali_fator1_FAT3_DIC)),eval(parse(text = input$quali_fator2_FAT3_DIC)), eval(parse(text = input$quali_fator3_FAT3_DIC))),
                 mcomp = input$mcomp_analise_FAT3_DIC,
                 fac.names = c(input$fac.names_FAT3_DIC_f1,input$fac.names_FAT3_DIC_f2,input$fac.names_FAT3_DIC_f3),
                 sigT = input$sigT_analise_FAT3_DIC,
                 sigF = input$sigF_analise_FAT3_DIC,
                 unfold = switch(input$unfold_analise_FAT3_DIC, "Nenhum"=NULL,input$unfold_analise_FAT3_DIC)
        )
      }
    })
    
    output$analise_residuo = renderPlot({
      
      modelo_fat3.dic = fat3.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT3_DIC,sep=""))),
                                 fator2 = eval(parse(text = paste("dados$",input$fator2_FAT3_DIC,sep=""))),
                                 fator3 = eval(parse(text = paste("dados$",input$fator3_FAT3_DIC,sep=""))),
                                 resp = eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))), 
                                 quali = c(eval(parse(text = input$quali_fator1_FAT3_DIC)),eval(parse(text = input$quali_fator2_FAT3_DIC)), eval(parse(text = input$quali_fator3_FAT3_DIC))),
                                 mcomp = input$mcomp_analise_FAT3_DIC,
                                 fac.names = c(input$fac.names_FAT3_DIC_f1,input$fac.names_FAT3_DIC_f2,input$fac.names_FAT3_DIC_f3),
                                 sigT = input$sigT_analise_FAT3_DIC,
                                 sigF = input$sigF_analise_FAT3_DIC,
                                 unfold = switch(input$unfold_analise_FAT3_DIC, "Nenhum"=NULL,input$unfold_analise_FAT3_DIC))
      
      plotres(modelo_fat3.dic)
      
    })
    
    output$analise_boxplot_FAT3_DIC_fator1 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator1_FAT3_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator1_FAT3_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3_DIC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT3_DIC_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_FAT3_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_FAT3_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3_DIC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT3_DIC_fator3 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator3_FAT3_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator3_FAT3_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3_DIC)))
      
      figura
      
    })
    
    output$analise_interacao_FAT3_DIC_fator1_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT3_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT3_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))),
                       trace.label = input$fator2_FAT3_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT3_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3_DIC_fator1_fator3 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT3_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator3_FAT3_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))),
                       trace.label = input$fator3_FAT3_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT3_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_FAT3_DIC_fator2_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT3_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT3_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))),
                       trace.label = input$fator1_FAT3_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT3_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3_DIC_fator2_fator3 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT3_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator3_FAT3_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))),
                       trace.label = input$fator3_FAT3_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT3_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })       
    
    output$analise_interacao_FAT3_DIC_fator3_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator3_FAT3_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT3_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))),
                       trace.label = input$fator1_FAT3_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator3_FAT3_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3_DIC_fator3_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator3_FAT3_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT3_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))),
                       trace.label = input$fator2_FAT3_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator3_FAT3_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
  }
  
  #FAT3_DBC
  atualizarFAT3_DBC = function(){
    
    output$saida_analise <- renderPrint({
      
      if(!is.null(dados)){
        fat3.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT3_DBC,sep=""))),
                 fator2 = eval(parse(text = paste("dados$",input$fator2_FAT3_DBC,sep=""))),
                 fator3 = eval(parse(text = paste("dados$",input$fator3_FAT3_DBC,sep=""))),
                 bloco = eval(parse(text = paste("dados$",input$bloco_FAT3_DBC,sep=""))),
                 resp = eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))), 
                 quali = c(eval(parse(text = input$quali_fator1_FAT3_DBC)),eval(parse(text = input$quali_fator2_FAT3_DBC)), eval(parse(text = input$quali_fator3_FAT3_DBC))),
                 mcomp = input$mcomp_analise_FAT3_DBC,
                 fac.names = c(input$fac.names_FAT3_DBC_f1,input$fac.names_FAT3_DBC_f2,input$fac.names_FAT3_DBC_f3),
                 sigT = input$sigT_analise_FAT3_DBC,
                 sigF = input$sigF_analise_FAT3_DBC,
                 unfold = switch(input$unfold_analise_FAT3_DBC, "Nenhum"=NULL,input$unfold_analise_FAT3_DBC)
        )
      }
    })
    
    output$analise_residuo = renderPlot({
      
      modelo_fat3.dbc = fat3.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT3_DBC,sep=""))),
                                 fator2 = eval(parse(text = paste("dados$",input$fator2_FAT3_DBC,sep=""))),
                                 fator3 = eval(parse(text = paste("dados$",input$fator3_FAT3_DBC,sep=""))),
                                 bloco = eval(parse(text = paste("dados$",input$bloco_FAT3_DBC,sep=""))),
                                 resp = eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))), 
                                 quali = c(eval(parse(text = input$quali_fator1_FAT3_DBC)),eval(parse(text = input$quali_fator2_FAT3_DBC)), eval(parse(text = input$quali_fator3_FAT3_DBC))),
                                 mcomp = input$mcomp_analise_FAT3_DBC,
                                 fac.names = c(input$fac.names_FAT3_DBC_f1,input$fac.names_FAT3_DBC_f2,input$fac.names_FAT3_DBC_f3),
                                 sigT = input$sigT_analise_FAT3_DBC,
                                 sigF = input$sigF_analise_FAT3_DBC,
                                 unfold = switch(input$unfold_analise_FAT3_DBC, "Nenhum"=NULL,input$unfold_analise_FAT3_DBC))
      
      plotres(modelo_fat3.dbc)
      
    })
    
    output$analise_boxplot_FAT3_DBC_fator1 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator1_FAT3_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator1_FAT3_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3_DBC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT3_DBC_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_FAT3_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_FAT3_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3_DBC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT3_DBC_fator3 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator3_FAT3_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator3_FAT3_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3_DBC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT3_DBC_bloco = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$bloco_FAT3_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$bloco_FAT3_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3_DBC)))
      
      figura
      
    })
    
    output$analise_interacao_FAT3_DBC_fator1_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT3_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT3_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))),
                       trace.label = input$fator2_FAT3_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT3_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3_DBC_fator1_fator3 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT3_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator3_FAT3_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))),
                       trace.label = input$fator3_FAT3_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT3_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_FAT3_DBC_fator2_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT3_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT3_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))),
                       trace.label = input$fator1_FAT3_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT3_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3_DBC_fator2_fator3 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT3_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator3_FAT3_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))),
                       trace.label = input$fator3_FAT3_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT3_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })       
    
    output$analise_interacao_FAT3_DBC_fator3_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator3_FAT3_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT3_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))),
                       trace.label = input$fator1_FAT3_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator3_FAT3_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3_DBC_fator3_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator3_FAT3_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT3_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))),
                       trace.label = input$fator2_FAT3_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator3_FAT3_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })  
    
  }
  
  #PSUB_DIC
  atualizarPSUB_DIC = function(){
    
    output$saida_analise <- renderPrint({
      
      if(!is.null(dados)){
        psub2.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_PSUB_DIC,sep=""))),
                 fator2 = eval(parse(text = paste("dados$",input$fator2_PSUB_DIC,sep=""))),
                 repet = eval(parse(text = paste("dados$",input$repet_PSUB_DIC,sep=""))),
                 resp = eval(parse(text = paste("dados$",input$resp_PSUB_DIC,sep=""))), 
                 quali = c(eval(parse(text = input$quali_fator1_PSUB_DIC)),eval(parse(text = input$quali_fator2_PSUB_DIC))),
                 mcomp = input$mcomp_analise_PSUB_DIC,
                 fac.names = c(input$fac.names_PSUB_DIC_f1,input$fac.names_PSUB_DIC_f2),
                 sigT = input$sigT_analise_PSUB_DIC,
                 sigF = input$sigF_analise_PSUB_DIC,
                 unfold = switch(input$unfold_analise_PSUB_DIC, "Nenhum"=NULL,input$unfold_analise_PSUB_DIC)
        )
      }
    })
    
    output$analise_boxplot_PSUB_DIC_fator1 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_PSUB_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator1_PSUB_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator1_PSUB_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$fator1_PSUB_DIC)))
      
      figura
      
    })
    
    output$analise_boxplot_PSUB_DIC_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_PSUB_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_PSUB_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_PSUB_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$fator2_PSUB_DIC)))
      
      figura
      
    })
    
    output$analise_interacao_PSUB_DIC_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_PSUB_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_PSUB_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_PSUB_DIC,sep=""))),
                       trace.label = input$fator2_PSUB_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_PSUB_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_PSUB_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_PSUB_DIC_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_PSUB_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_PSUB_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_PSUB_DIC,sep=""))),
                       trace.label = input$fator1_PSUB_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_PSUB_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_PSUB_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
  }

  #PSUB_DBC
  atualizarPSUB_DBC = function(){
 
    output$saida_analise <- renderPrint({
      
      if(!is.null(dados)){
        psub2.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_PSUB_DBC,sep=""))),
                  fator2 = eval(parse(text = paste("dados$",input$fator2_PSUB_DBC,sep=""))),
                  bloco = eval(parse(text = paste("dados$",input$bloco_PSUB_DBC,sep=""))),
                  resp = eval(parse(text = paste("dados$",input$resp_PSUB_DBC,sep=""))), 
                  quali = c(eval(parse(text = input$quali_fator1_PSUB_DBC)),eval(parse(text = input$quali_fator2_PSUB_DBC))),
                  mcomp = input$mcomp_analise_PSUB_DBC,
                  fac.names = c(input$fac.names_PSUB_DBC_f1,input$fac.names_PSUB_DBC_f2),
                  sigT = input$sigT_analise_PSUB_DBC,
                  sigF = input$sigF_analise_PSUB_DBC,
                  unfold = switch(input$unfold_analise_PSUB_DBC, "Nenhum"=NULL,input$unfold_analise_PSUB_DBC)
        )
      }
    })

    output$analise_boxplot_PSUB_DBC_fator1 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_PSUB_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator1_PSUB_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator1_PSUB_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_PSUB_DBC)))
      
      figura
      
    })
    
    output$analise_boxplot_PSUB_DBC_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_PSUB_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_PSUB_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_PSUB_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_PSUB_DBC)))
      
      figura
      
    })
    
    output$analise_boxplot_PSUB_DBC_bloco = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_PSUB_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$bloco_PSUB_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$bloco_PSUB_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_PSUB_DBC)))
      
      figura
      
    })
    
    output$analise_interacao_PSUB_DBC_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_PSUB_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_PSUB_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_PSUB_DBC,sep=""))),
                       trace.label = input$fator2_PSUB_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_PSUB_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_PSUB_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_PSUB_DBC_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_PSUB_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_PSUB_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_PSUB_DBC,sep=""))),
                       trace.label = input$fator1_PSUB_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_PSUB_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_PSUB_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
  }  
  
  #FAT2AD_DIC
  atualizarFAT2AD_DIC = function(){
    
    if(input$tipo_delineamento=='fat2.ad.dic'){
    
      resposta_adicional_FAT2AD_DIC = NULL
      
      if(!is.null(input$respAd_FAT2AD_DIC$datapath)){
      
        resposta_adicional_FAT2AD_DIC = read.csv2(input$respAd_FAT2AD_DIC$datapath)
      
        print(resposta_adicional_FAT2AD_DIC)
        
        temp = resposta_adicional_FAT2AD_DIC

        colnames(temp) = c("Tratamento Adicional")

        output$tabela_adicional_delineamento = renderDataTable(temp)
        
        resposta_adicional_FAT2AD_DIC = resposta_adicional_FAT2AD_DIC[,1]
        
      }else{
        
        shinyalert("Oops!", "Selecione um arquivo CVS (coluna única e símbolo decimal a vírgula) com os dados do Tratamento Adicional", type = "error")
        
      }
      
    }
      
    output$saida_analise <- renderPrint({
      
      if((!is.null(dados))&&(!is.null(resposta_adicional_FAT2AD_DIC))){
        fat2.ad.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2AD_DIC,sep=""))),
                 fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2AD_DIC,sep=""))),
                 repet = eval(parse(text = paste("dados$",input$repet_FAT2AD_DIC,sep=""))),
                 resp = eval(parse(text = paste("dados$",input$resp_FAT2AD_DIC,sep=""))), 
                 respAd = resposta_adicional_FAT2AD_DIC,
                 quali = c(eval(parse(text = input$quali_fator1_FAT2AD_DIC)),eval(parse(text = input$quali_fator2_FAT2AD_DIC))),
                 mcomp = input$mcomp_analise_FAT2AD_DIC,
                 fac.names = c(input$fac.names_FAT2AD_DIC_f1,input$fac.names_FAT2AD_DIC_f2),
                 sigT = input$sigT_analise_FAT2AD_DIC,
                 sigF = input$sigF_analise_FAT2AD_DIC,
                 unfold = switch(input$unfold_analise_FAT2AD_DIC, "Nenhum"=NULL,input$unfold_analise_FAT2AD_DIC)
        )
      }
    })
    
    output$analise_boxplot_FAT2AD_DIC_fator1 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2AD_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator1_FAT2AD_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator1_FAT2AD_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD_DIC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT2AD_DIC_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2AD_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_FAT2AD_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_FAT2AD_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD_DIC)))
      
      figura
      
    })

    output$analise_interacao_FAT2AD_DIC_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT2AD_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT2AD_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2AD_DIC,sep=""))),
                       trace.label = input$fator2_FAT2AD_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT2AD_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2AD_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_FAT2AD_DIC_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT2AD_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT2AD_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2AD_DIC,sep=""))),
                       trace.label = input$fator1_FAT2AD_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT2AD_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2AD_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    }) 
    
  }
   
  #FAT2AD_DBC
  atualizarFAT2AD_DBC = function(){
    
    if(input$tipo_delineamento=='fat2.ad.dbc'){
      
      resposta_adicional_FAT2AD_DBC = NULL
      
      if(!is.null(input$respAd_FAT2AD_DBC$datapath)){
        
        resposta_adicional_FAT2AD_DBC = read.csv2(input$respAd_FAT2AD_DBC$datapath)
        
        temp = resposta_adicional_FAT2AD_DBC
        
        colnames(temp) = c("Tratamento Adicional")
        
        output$tabela_adicional_delineamento = renderDataTable(temp)
        
        resposta_adicional_FAT2AD_DBC = resposta_adicional_FAT2AD_DBC[,1]
        
      }else{
        
        shinyalert("Oops!", "Selecione um arquivo CVS (coluna única e símbolo decimal a vírgula) com os dados do Tratamento Adicional", type = "error")
        
      }
      
    }
    
    output$saida_analise <- renderPrint({
      
      if((!is.null(dados))&&(!is.null(resposta_adicional_FAT2AD_DBC))){
        fat2.ad.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2AD_DBC,sep=""))),
                    fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2AD_DBC,sep=""))),
                    bloco = eval(parse(text = paste("dados$",input$bloco_FAT2AD_DBC,sep=""))),
                    resp = eval(parse(text = paste("dados$",input$resp_FAT2AD_DBC,sep=""))), 
                    respAd = resposta_adicional_FAT2AD_DBC,
                    quali = c(eval(parse(text = input$quali_fator1_FAT2AD_DBC)),eval(parse(text = input$quali_fator2_FAT2AD_DBC))),
                    mcomp = input$mcomp_analise_FAT2AD_DBC,
                    fac.names = c(input$fac.names_FAT2AD_DBC_f1,input$fac.names_FAT2AD_DBC_f2),
                    sigT = input$sigT_analise_FAT2AD_DBC,
                    sigF = input$sigF_analise_FAT2AD_DBC,
                    unfold = switch(input$unfold_analise_FAT2AD_DBC, "Nenhum"=NULL,input$unfold_analise_FAT2AD_DBC)
        )
      }
    })
    
    output$analise_boxplot_FAT2AD_DBC_fator1 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2AD_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator1_FAT2AD_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator1_FAT2AD_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD_DBC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT2AD_DBC_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2AD_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_FAT2AD_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_FAT2AD_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD_DBC)))
      
      figura
      
    })
    
    
    output$analise_boxplot_FAT2AD_DBC_bloco = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2AD_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$bloco_FAT2AD_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$bloco_FAT2AD_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD_DBC)))
      
      figura
      
    })
    
    output$analise_interacao_FAT2AD_DBC_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT2AD_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT2AD_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2AD_DBC,sep=""))),
                       trace.label = input$fator2_FAT2AD_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT2AD_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2AD_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_FAT2AD_DBC_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT2AD_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT2AD_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2AD_DBC,sep=""))),
                       trace.label = input$fator1_FAT2AD_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT2AD_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2AD_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    }) 
    
  }
   
  #FAT2AD2_DIC
  atualizarFAT2AD2_DIC = function(){
    
      resposta_adicional1_FAT2AD2_DIC = NULL
    
      resposta_adicional2_FAT2AD2_DIC = NULL
    
      if((input$tipo_delineamento=='fat2.ad2.dic')&&(!is.null(input$respAd1_FAT2AD2_DIC))&&(!is.null(input$respAd2_FAT2AD2_DIC))){
        
        tempAd1 = input$respAd1_FAT2AD2_DIC
        
        extAd1 = tools::file_ext(tempAd1$datapath)
        
        #Arquivo Adicional 1
        if(extAd1=="csv"){
          
          resposta_adicional1_FAT2AD2_DIC = read.csv2(input$respAd1_FAT2AD2_DIC$datapath)
          #print(resposta_adicional1_FAT2AD2_DIC)
          tempAd1 = resposta_adicional1_FAT2AD2_DIC
          
          colnames(tempAd1) = c("Tratamento Adicional 1")
          
          output$tabela_adicional1_delineamento_dic = renderDataTable(tempAd1)
        
          resposta_adicional1_FAT2AD2_DIC = resposta_adicional1_FAT2AD2_DIC[,1]
          
        }else{
          
          shinyalert("Oops!", "Selecione os arquivo CVS (coluna única e símbolo decimal a vírgula) com os dados do Tratamento Adicional 1!", type = "error")
          
        }
        
        #Arquivo Adicional 2
        
        tempAd2 = input$respAd2_FAT2AD2_DIC
        
        extAd2 = tools::file_ext(tempAd2$datapath)
        
        if(extAd2=="csv"){
          
          resposta_adicional2_FAT2AD2_DIC = read.csv2(input$respAd2_FAT2AD2_DIC$datapath)
          
          print(resposta_adicional2_FAT2AD2_DIC)
          
          tempAd2 = resposta_adicional2_FAT2AD2_DIC
          
          colnames(tempAd2) = c("Tratamento Adicional 2")
          
          output$tabela_adicional2_delineamento_dic = renderDataTable(tempAd2)
          
          resposta_adicional2_FAT2AD2_DIC = resposta_adicional2_FAT2AD2_DIC[,1]
          
        }else{
          
          shinyalert("Oops!", "Selecione os arquivo CVS (coluna única e símbolo decimal a vírgula) com os dados do Tratamento Adicional 2!", type = "error")
          
        }
      
        output$analise_boxplot_FAT2AD2_DIC_fator1 = plotly::renderPlotly({
          
          figura = plot_ly(dados, 
                           y=eval(parse(text = paste("dados$",input$resp_FAT2AD2_DIC,sep=""))), 
                           x=eval(parse(text = paste("dados$",input$fator1_FAT2AD2_DIC,sep=""))),
                           boxpoints = "all",
                           type="box") %>% 
            layout(
              title = "",
              xaxis = list(title = paste("Fator:",input$fator1_FAT2AD2_DIC,sep=" ")),
              yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD2_DIC)))
          
          figura
          
        })
        
        output$analise_boxplot_FAT2AD2_DIC_fator2 = plotly::renderPlotly({
          
          figura = plot_ly(dados, 
                           y=eval(parse(text = paste("dados$",input$resp_FAT2AD2_DIC,sep=""))), 
                           x=eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DIC,sep=""))),
                           boxpoints = "all",
                           type="box") %>% 
            layout(
              title = "",
              xaxis = list(title = paste("Fator:",input$fator2_FAT2AD2_DIC,sep=" ")),
              yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD2_DIC)))
          
          figura
          
        })
        
      }

    output$saida_analise <- renderPrint({
      
      if((!is.null(dados))&&(!is.null(resposta_adicional1_FAT2AD2_DIC))&&(!is.null(resposta_adicional2_FAT2AD2_DIC))){
        fat2.ad2.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2AD2_DIC,sep=""))),
                    fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DIC,sep=""))),
                    repet = eval(parse(text = paste("dados$",input$repet_FAT2AD2_DIC,sep=""))),
                    resp = eval(parse(text = paste("dados$",input$resp_FAT2AD2_DIC,sep=""))), 
                    respAd1 = resposta_adicional1_FAT2AD2_DIC,
                    respAd2 = resposta_adicional2_FAT2AD2_DIC,
                    quali = c(eval(parse(text = input$quali_fator1_FAT2AD2_DIC)),eval(parse(text = input$quali_fator2_FAT2AD2_DIC))),
                    mcomp = input$mcomp_analise_FAT2AD2_DIC,
                    fac.names = c(input$fac.names_FAT2AD_DIC_f1,input$fac.names_FAT2AD2_DIC_f2),
                    sigT = input$sigT_analise_FAT2A2D_DIC,
                    sigF = input$sigF_analise_FAT2AD2_DIC,
                    unfold = switch(input$unfold_analise_FAT2AD2_DIC, "Nenhum"=NULL,input$unfold_analise_FAT2AD2_DIC)
        )
      }
    })
    
    output$analise_interacao_FAT2AD2_DIC_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT2AD2_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2AD2_DIC,sep=""))),
                       trace.label = input$fator2_FAT2AD2_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT2AD2_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2AD2_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_FAT2AD2_DIC_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT2AD2_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2AD2_DIC,sep=""))),
                       trace.label = input$fator1_FAT2AD2_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT2AD2_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2AD2_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    }) 
    
  }
  
  #FAT2AD2_DBC
  atualizarFAT2AD2_DBC = function(){
    
    resposta_adicional1_FAT2AD2_DBC = NULL
    
    resposta_adicional2_FAT2AD2_DBC = NULL
    
    if((input$tipo_delineamento=='fat2.ad2.dbc')&&(!is.null(input$respAd1_FAT2AD2_DBC))&&(!is.null(input$respAd2_FAT2AD2_DBC))){
      
      tempAd1_dbc = input$respAd1_FAT2AD2_DBC
      
      extAd1_dbc = tools::file_ext(tempAd1_dbc$datapath)
      
      #Arquivo Adicional 1
      if(extAd1_dbc=="csv"){
        
        resposta_adicional1_FAT2AD2_DBC = read.csv2(input$respAd1_FAT2AD2_DBC$datapath)
        
        tempAd1_dbc = resposta_adicional1_FAT2AD2_DBC
        
        colnames(tempAd1_dbc) = c("Tratamento Adicional 1")
        
        output$tabela_adicional1_delineamento_dbc = renderDataTable(tempAd1_dbc)
        
        resposta_adicional1_FAT2AD2_DBC = resposta_adicional1_FAT2AD2_DBC[,1]
        
      }else{
        
        shinyalert("Oops!", "Selecione os arquivo CVS (coluna única e símbolo decimal a vírgula) com os dados do Tratamento Adicional 1!", type = "error")
        
      }
      
      #Arquivo Adicional 2
      
      tempAd2_dbc = input$respAd2_FAT2AD2_DBC
      
      extAd2_dbc = tools::file_ext(tempAd2_dbc$datapath)
      
      if(extAd2_dbc=="csv"){
        
        resposta_adicional2_FAT2AD2_DBC = read.csv2(input$respAd2_FAT2AD2_DBC$datapath)
        
        tempAd2_dbc = resposta_adicional2_FAT2AD2_DBC
        
        colnames(tempAd2_dbc) = c("Tratamento Adicional 2")
        
        output$tabela_adicional2_delineamento_dbc = renderDataTable(tempAd2_dbc)
        
        resposta_adicional2_FAT2AD2_DBC = resposta_adicional2_FAT2AD2_DBC[,1]
        
      }else{
        
        shinyalert("Oops!", "Selecione os arquivo CVS (coluna única e símbolo decimal a vírgula) com os dados do Tratamento Adicional 2!", type = "error")
        
      }
      
      output$analise_boxplot_FAT2AD2_DBC_fator1 = plotly::renderPlotly({
        
        figura = plot_ly(dados, 
                         y=eval(parse(text = paste("dados$",input$resp_FAT2AD2_DBC,sep=""))), 
                         x=eval(parse(text = paste("dados$",input$fator1_FAT2AD2_DBC,sep=""))),
                         boxpoints = "all",
                         type="box") %>% 
          layout(
            title = "",
            xaxis = list(title = paste("Fator:",input$fator1_FAT2AD2_DBC,sep=" ")),
            yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD2_DBC)))
        
        figura
        
      })
      
      output$analise_boxplot_FAT2AD2_DBC_fator2 = plotly::renderPlotly({
        
        figura = plot_ly(dados, 
                         y=eval(parse(text = paste("dados$",input$resp_FAT2AD2_DBC,sep=""))), 
                         x=eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DBC,sep=""))),
                         boxpoints = "all",
                         type="box") %>% 
          layout(
            title = "",
            xaxis = list(title = paste("Fator:",input$fator2_FAT2AD2_DBC,sep=" ")),
            yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD2_DBC)))
        
        figura
        
      })
      
      output$analise_boxplot_FAT2AD2_DBC_bloco = plotly::renderPlotly({
        
        figura = plot_ly(dados, 
                         y=eval(parse(text = paste("dados$",input$resp_FAT2AD2_DBC,sep=""))), 
                         x=eval(parse(text = paste("dados$",input$bloco_FAT2AD2_DBC,sep=""))),
                         boxpoints = "all",
                         type="box") %>% 
          layout(
            title = "",
            xaxis = list(title = paste("Fator:",input$bloco_FAT2AD2_DBC,sep=" ")),
            yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD2_DBC)))
        
        figura
        
      })
      
    }
    
    output$saida_analise <- renderPrint({
      
      if((!is.null(dados))&&(!is.null(resposta_adicional1_FAT2AD2_DBC))&&(!is.null(resposta_adicional2_FAT2AD2_DBC))){
        
        fat2.ad2.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2AD2_DBC,sep=""))),
                     fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DBC,sep=""))),
                     bloco = eval(parse(text = paste("dados$",input$bloco_FAT2AD2_DBC,sep=""))),
                     resp = eval(parse(text = paste("dados$",input$resp_FAT2AD2_DBC,sep=""))), 
                     respAd1 = resposta_adicional1_FAT2AD2_DBC,
                     respAd2 = resposta_adicional2_FAT2AD2_DBC,
                     quali = c(eval(parse(text = input$quali_fator1_FAT2AD2_DBC)),eval(parse(text = input$quali_fator2_FAT2AD2_DBC))),
                     mcomp = input$mcomp_analise_FAT2AD2_DBC,
                     fac.names = c(input$fac.names_FAT2AD2_DBC_f1,input$fac.names_FAT2AD2_DBC_f2),
                     sigT = input$sigT_analise_FAT2AD2_DBC,
                     sigF = input$sigF_analise_FAT2AD2_DBC,
                     unfold = switch(input$unfold_analise_FAT2AD2_DBC, "Nenhum"=NULL,input$unfold_analise_FAT2AD2_DBC)
        )
      }
    })
    
    output$analise_boxplot_FAT2AD2_DBC_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2AD2_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_FAT2AD2_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT2AD2_DBC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT2AD2_DBC_bloco = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT2AD2_DBC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$bloco_FAT2AD2_DBC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$bloco_FAT2AD2_DBC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FATAD22_DBC)))
      
      figura
      
    })    
    
    output$analise_interacao_FAT2AD2_DBC_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT2AD2_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2_DBC,sep=""))),
                       trace.label = input$fator2_FAT2AD2_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT2AD2_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2AD2_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_FAT2AD2_DBC_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT2AD2_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT2AD2_DBC,sep=""))),
                       trace.label = input$fator1_FAT2AD2_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT2AD2_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT2AD2_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    }) 
    
  }
  
  #FAT3AD_DIC
  atualizarFAT3AD_DIC = function(){
    
    if(input$tipo_delineamento=='fat3.ad.dic'){
      
      resposta_adicional_FAT3AD_DIC = NULL
      
      if(!is.null(input$respAd_FAT3AD_DIC$datapath)){
        
        resposta_adicional_FAT3AD_DIC = read.csv2(input$respAd_FAT3AD_DIC$datapath)
        
        temp_dic_fat3 = resposta_adicional_FAT3AD_DIC
        
        colnames(temp_dic_fat3) = c("Tratamento Adicional")
        
        output$tabela_adicional_delineamento_dic_fat3 = renderDataTable(temp_dic_fat3)
        
        resposta_adicional_FAT3AD_DIC = resposta_adicional_FAT3AD_DIC[,1]
        
      }else{
        
        shinyalert("Oops!", "Selecione um arquivo CVS (coluna única e símbolo decimal a vírgula) com os dados do Tratamento Adicional", type = "error")
        
      }
      
    }
    
    output$saida_analise <- renderPrint({
      
      if((!is.null(dados))&&(!is.null(resposta_adicional_FAT3AD_DIC))){
        fat3.ad.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DIC,sep=""))),
                    fator2 = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DIC,sep=""))),
                    fator3 = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DIC,sep=""))),
                    repet = eval(parse(text = paste("dados$",input$repet_FAT3AD_DIC,sep=""))),
                    resp = eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))), 
                    respAd = resposta_adicional_FAT3AD_DIC,
                    quali = c(eval(parse(text = input$quali_fator1_FAT3AD_DIC)),eval(parse(text = input$quali_fator2_FAT3AD_DIC)),eval(parse(text = input$quali_fator3_FAT3AD_DIC))),
                    mcomp = input$mcomp_analise_FAT3AD_DIC,
                    fac.names = c(input$fac.names_FAT3AD_DIC_f1,input$fac.names_FAT3AD_DIC_f2,input$fac.names_FAT3AD_DIC_f3),
                    sigT = input$sigT_analise_FAT3AD_DIC,
                    sigF = input$sigF_analise_FAT3AD_DIC,
                    unfold = switch(input$unfold_analise_FAT3AD_DIC, "Nenhum"=NULL,input$unfold_analise_FAT3AD_DIC)
        )
      }
    })
    
    output$analise_boxplot_FAT3AD_DIC_fator1 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator1_FAT3AD_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator1_FAT3AD_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3AD_DIC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT3AD_DIC_fator2 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator2_FAT3AD_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator2_FAT3AD_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3AD_DIC)))
      
      figura
      
    })
    
    output$analise_boxplot_FAT3AD_DIC_fator3 = plotly::renderPlotly({
      
      figura = plot_ly(dados, 
                       y=eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))), 
                       x=eval(parse(text = paste("dados$",input$fator3_FAT3AD_DIC,sep=""))),
                       boxpoints = "all",
                       type="box") %>% 
        layout(
          title = "",
          xaxis = list(title = paste("Fator:",input$fator3_FAT3AD_DIC,sep=" ")),
          yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3AD_DIC)))
      
      figura
      
    })
    
    output$analise_interacao_FAT3AD_DIC_fator1_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))),
                       trace.label = input$fator2_FAT3AD_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT3AD_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3AD_DIC_fator1_fator3 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))),
                       trace.label = input$fator3_FAT3AD_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT3AD_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_FAT3AD_DIC_fator2_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))),
                       trace.label = input$fator1_FAT3AD_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT3AD_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3AD_DIC_fator2_fator3 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))),
                       trace.label = input$fator3_FAT3AD_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT3AD_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })       
    
    output$analise_interacao_FAT3AD_DIC_fator3_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))),
                       trace.label = input$fator1_FAT3AD_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator3_FAT3AD_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3AD_DIC_fator3_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DIC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DIC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))),
                       trace.label = input$fator2_FAT3AD_DIC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator3_FAT3AD_DIC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DIC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
  }
  
  #FAT3AD_DBC
  atualizarFAT3AD_DBC = function(){
    
    if(input$tipo_delineamento=='fat3.ad.dbc'){
      
      resposta_adicional_FAT3AD_DBC = NULL
      
      if(!is.null(input$respAd_FAT3AD_DBC$datapath)){
        
        resposta_adicional_FAT3AD_DBC = read.csv2(input$respAd_FAT3AD_DBC$datapath)
        
        temp_dbc_fat3 = resposta_adicional_FAT3AD_DBC
        
        colnames(temp_dbc_fat3) = c("Tratamento Adicional")
        
        output$tabela_adicional_delineamento_dbc_fat3 = renderDataTable(temp_dbc_fat3)
        
        resposta_adicional_FAT3AD_DBC = resposta_adicional_FAT3AD_DBC[,1]
        
      }else{
        
        shinyalert("Oops!", "Selecione um arquivo CVS (coluna única e símbolo decimal a vírgula) com os dados do Tratamento Adicional", type = "error")
        
      }
      
      output$analise_boxplot_FAT3AD_DBC_fator1 = plotly::renderPlotly({
        
        figura = plot_ly(dados, 
                         y=eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))), 
                         x=eval(parse(text = paste("dados$",input$fator1_FAT3AD_DBC,sep=""))),
                         boxpoints = "all",
                         type="box") %>% 
          layout(
            title = "",
            xaxis = list(title = paste("Fator:",input$fator1_FAT3AD_DBC,sep=" ")),
            yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3AD_DBC)))
        
        figura
        
      })
      
      output$analise_boxplot_FAT3AD_DBC_fator2 = plotly::renderPlotly({
        
        figura = plot_ly(dados, 
                         y=eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))), 
                         x=eval(parse(text = paste("dados$",input$fator2_FAT3AD_DBC,sep=""))),
                         boxpoints = "all",
                         type="box") %>% 
          layout(
            title = "",
            xaxis = list(title = paste("Fator:",input$fator2_FAT3AD_DBC,sep=" ")),
            yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3AD_DBC)))
        
        figura
        
      })
      
      output$analise_boxplot_FAT3AD_DBC_fator3 = plotly::renderPlotly({
        
        figura = plot_ly(dados, 
                         y=eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))), 
                         x=eval(parse(text = paste("dados$",input$fator3_FAT3AD_DBC,sep=""))),
                         boxpoints = "all",
                         type="box") %>% 
          layout(
            title = "",
            xaxis = list(title = paste("Fator:",input$fator3_FAT3AD_DBC,sep=" ")),
            yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3AD_DBC)))
        
        figura
        
      })
      
      output$analise_boxplot_FAT3AD_DBC_bloco = plotly::renderPlotly({
        
        figura = plot_ly(dados, 
                         y=eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))), 
                         x=eval(parse(text = paste("dados$",input$bloco_FAT3AD_DBC,sep=""))),
                         boxpoints = "all",
                         type="box") %>% 
          layout(
            title = "",
            xaxis = list(title = paste("Fator:",input$bloco_FAT3AD_DBC,sep=" ")),
            yaxis = list(title = paste("Variável Resposta:",input$resp_FAT3AD_DBC)))
        
        figura
        
      })
      
    }
    
    output$saida_analise <- renderPrint({
      
      if((!is.null(dados))&&(!is.null(resposta_adicional_FAT3AD_DBC))){
        fat3.ad.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DBC,sep=""))),
                    fator2 = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DBC,sep=""))),
                    fator3 = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DBC,sep=""))),
                    bloco = eval(parse(text = paste("dados$",input$bloco_FAT3AD_DBC,sep=""))),
                    resp = eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))), 
                    respAd = resposta_adicional_FAT3AD_DBC,
                    quali = c(eval(parse(text = input$quali_fator1_FAT3AD_DBC)),eval(parse(text = input$quali_fator2_FAT3AD_DBC)),eval(parse(text = input$quali_fator3_FAT3AD_DBC))),
                    mcomp = input$mcomp_analise_FAT3AD_DBC,
                    fac.names = c(input$fac.names_FAT3AD_DBC_f1,input$fac.names_FAT3AD_DBC_f2,input$fac.names_FAT3AD_DBC_f3),
                    sigT = input$sigT_analise_FAT3AD_DBC,
                    sigF = input$sigF_analise_FAT3AD_DBC,
                    unfold = switch(input$unfold_analise_FAT3AD_DBC, "Nenhum"=NULL,input$unfold_analise_FAT3AD_DBC)
        )
      }
    })
    
    output$analise_interacao_FAT3AD_DBC_fator1_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))),
                       trace.label = input$fator2_FAT3AD_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT3AD_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3AD_DBC_fator1_fator3 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))),
                       trace.label = input$fator3_FAT3AD_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator1_FAT3AD_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })    
    
    output$analise_interacao_FAT3AD_DBC_fator2_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))),
                       trace.label = input$fator1_FAT3AD_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT3AD_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3AD_DBC_fator2_fator3 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))),
                       trace.label = input$fator3_FAT3AD_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator2_FAT3AD_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })       
    
    output$analise_interacao_FAT3AD_DBC_fator3_fator1 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))),
                       trace.label = input$fator1_FAT3AD_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator3_FAT3AD_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
    output$analise_interacao_FAT3AD_DBC_fator3_fator2 = renderPlot({
      
      interaction.plot(x.factor = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DBC,sep=""))),
                       trace.factor = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DBC,sep=""))),
                       response = eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))),
                       trace.label = input$fator2_FAT3AD_DBC,
                       axes = FALSE,
                       xlab = paste("Fator:",input$fator3_FAT3AD_DBC,sep=" "),
                       ylab = paste("Média da Variável Resposta:",input$resp_FAT3AD_DBC,sep=" "))
      axis(side = 1)
      axis(side = 2)
      
    })
    
  }
  
  observeEvent(input$ir_analise, {
  
    updateTabsetPanel(inputId = "tabs",selected = 'panelAnalise')
    
    switch (input$tipo_delineamento,
      "dic" = {
        atualizarDIC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_DIC')  
        },
      "dbc" = {atualizarDBC()
               updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_DBC')
        },
      "dql" = {atualizarDQL()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_DQL')
        },
      "faixas" = {atualizarfaixas()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_Faixas')
        },
      "fat2.dic" = {atualizarFAT2_DIC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_FAT2_DIC')
        },
      "fat2.dbc" = {atualizarFAT2_DBC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_FAT2_DBC')
        },
      "fat3.dic" = {atualizarFAT3_DIC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_FAT3_DIC')
        },
      "fat3.dbc" = {atualizarFAT3_DBC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_FAT3_DBC')
      },
      "psub2.dic" = {atualizarPSUB_DIC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_PSUB_DIC')
      },
      "psub2.dbc" = {atualizarPSUB_DBC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_PSUB_DBC')
      },
      "fat2.ad.dic" = {atualizarFAT2AD_DIC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_FAT2AD_DIC')
      },
      "fat2.ad.dbc" = {atualizarFAT2AD_DBC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_FAT2AD_DBC')
      },
      "fat2.ad2.dic" = {
        atualizarFAT2AD2_DIC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_FAT2AD2_DIC')
      },
      "fat2.ad2.dbc" = {
        atualizarFAT2AD2_DBC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_FAT2AD2_DBC')
      },
      "fat3.ad.dic" = {
        atualizarFAT3AD_DIC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_FAT3AD_DIC')
      },
      "fat3.ad.dbc" = {
        atualizarFAT3AD_DBC()
        updateTabsetPanel(inputId = "painel_opcao_delineamento_analise",selected = 'tabs_analise_FAT3AD_DBC')
      }
      
      
    )
      
  })    

    #DIC
    observeEvent(input$mcomp_analise_DIC, {atualizarDIC()})
    observeEvent(input$nl_analise_DIC, {atualizarDIC()})
    observeEvent(input$hvar_analise_DIC, {atualizarDIC()})
    observeEvent(input$sigT_analise_DIC, {atualizarDIC()})
    observeEvent(input$sigF_analise_DIC, {atualizarDIC()})
    observeEvent(input$unfold_analise_DIC, {atualizarDIC()})    
  
    #DBC
    observeEvent(input$mcomp_analise_DBC, {atualizarDBC()})
    observeEvent(input$nl_analise_DBC, {atualizarDBC()})
    observeEvent(input$hvar_analise_DBC, {atualizarDBC()})
    observeEvent(input$sigT_analise_DBC, {atualizarDBC()})
    observeEvent(input$sigF_analise_DBC, {atualizarDBC()})
    observeEvent(input$unfold_analise_DBC, {atualizarDBC()})

    #DQL
    observeEvent(input$mcomp_analise_DQL, {atualizarDQL()})
    observeEvent(input$sigT_analise_DQL, {atualizarDQL()})
    observeEvent(input$sigF_analise_DQL, {atualizarDQL()})
    observeEvent(input$unfold_analise_DQL, {atualizarDQL()})
    
    #Faixas
    observeEvent(input$mcomp_analise_faixas, {atualizarfaixas()})
    observeEvent(input$fac.names_faixas_f1, {atualizarfaixas()})
    observeEvent(input$fac.names_faixas_f2, {atualizarfaixas()})
    observeEvent(input$sigT_analise_faixas, {atualizarfaixas()})
    observeEvent(input$sigF_analise_faixas, {atualizarfaixas()})
    observeEvent(input$unfold_analise_faixas, {atualizarfaixas()})

    #FAT2_DIC
    observeEvent(input$mcomp_analise_FAT2_DIC, {atualizarFAT2_DIC()})
    observeEvent(input$fac.names_FAT2_DIC_f1, {atualizarFAT2_DIC()})
    observeEvent(input$fac.names_FAT2_DIC_f2, {atualizarFAT2_DIC()})
    observeEvent(input$sigT_analise_FAT2_DIC, {atualizarFAT2_DIC()})
    observeEvent(input$sigF_analise_FAT2_DIC, {atualizarFAT2_DIC()})
    observeEvent(input$unfold_analise_FAT2_DIC, {atualizarFAT2_DIC()})
    
    #FAT2_DBC
    observeEvent(input$mcomp_analise_FAT2_DBC, {atualizarFAT2_DBC()})
    observeEvent(input$fac.names_FAT2_DBC_f1, {atualizarFAT2_DBC()})
    observeEvent(input$fac.names_FAT2_DBC_f2, {atualizarFAT2_DBC()})
    observeEvent(input$sigT_analise_FAT2_DBC, {atualizarFAT2_DBC()})
    observeEvent(input$sigF_analise_FAT2_DBC, {atualizarFAT2_DBC()})
    observeEvent(input$unfold_analise_FAT2_DBC, {atualizarFAT2_DBC()})
    
    #FAT3_DIC
    observeEvent(input$mcomp_analise_FAT3_DIC, {atualizarFAT3_DIC()})
    observeEvent(input$fac.names_FAT3_DIC_f1, {atualizarFAT3_DIC()})
    observeEvent(input$fac.names_FAT3_DIC_f2, {atualizarFAT3_DIC()})
    observeEvent(input$fac.names_FAT3_DIC_f3, {atualizarFAT3_DIC()})
    observeEvent(input$sigT_analise_FAT3_DIC, {atualizarFAT3_DIC()})
    observeEvent(input$sigF_analise_FAT3_DIC, {atualizarFAT3_DIC()})
    observeEvent(input$unfold_analise_FAT3_DIC, {atualizarFAT3_DIC()})
    
    #FAT3_DBC
    observeEvent(input$mcomp_analise_FAT3_DBC, {atualizarFAT3_DBC()})
    observeEvent(input$fac.names_FAT3_DBC_f1, {atualizarFAT3_DBC()})
    observeEvent(input$fac.names_FAT3_DBC_f2, {atualizarFAT3_DBC()})
    observeEvent(input$fac.names_FAT3_DBC_f3, {atualizarFAT3_DBC()})
    observeEvent(input$sigT_analise_FAT3_DBC, {atualizarFAT3_DBC()})
    observeEvent(input$sigF_analise_FAT3_DBC, {atualizarFAT3_DBC()})
    observeEvent(input$unfold_analise_FAT3_DBC, {atualizarFAT3_DBC()})
    
    #PSUB_DIC
    observeEvent(input$mcomp_analise_PSUB_DIC, {atualizarPSUB_DIC()})
    observeEvent(input$fac.names_PSUB_DIC_f1, {atualizarPSUB_DIC()})
    observeEvent(input$fac.names_PSUB_DIC_f2, {atualizarPSUB_DIC()})
    observeEvent(input$sigT_analise_PSUB_DIC, {atualizarPSUB_DIC()})
    observeEvent(input$sigF_analise_PSUB_DIC, {atualizarPSUB_DIC()})
    observeEvent(input$unfold_analise_PSUB_DIC, {atualizarPSUB_DIC()})
    
    #PSUB_DBC
    observeEvent(input$mcomp_analise_PSUB_DBC, {atualizarPSUB_DBC()})
    observeEvent(input$fac.names_PSUB_DBC_f1, {atualizarPSUB_DBC()})
    observeEvent(input$fac.names_PSUB_DBC_f2, {atualizarPSUB_DBC()})
    observeEvent(input$sigT_analise_PSUB_DBC, {atualizarPSUB_DBC()})
    observeEvent(input$sigF_analise_PSUB_DBC, {atualizarPSUB_DBC()})
    observeEvent(input$unfold_analise_PSUB_DBC, {atualizarPSUB_DBC()})
    
    #FAT2AD_DIC
    observeEvent(input$respAd_FAT2AD_DIC, {atualizarFAT2AD_DIC()})
    observeEvent(input$mcomp_analise_FAT2AD_DIC, {atualizarFAT2AD_DIC()})
    observeEvent(input$fac.names_FAT2AD_DIC_f1, {atualizarFAT2AD_DIC()})
    observeEvent(input$fac.names_FAT2AD_DIC_f2, {atualizarFAT2AD_DIC()})
    observeEvent(input$sigT_analise_FAT2AD_DIC, {atualizarFAT2AD_DIC()})
    observeEvent(input$sigF_analise_FAT2AD_DIC, {atualizarFAT2AD_DIC()})
    observeEvent(input$unfold_analise_FAT2AD_DIC, {atualizarFAT2AD_DIC()})
   
    #FAT2AD_DBC
    observeEvent(input$respAd_FAT2AD_DBC, {atualizarFAT2AD_DBC()})
    observeEvent(input$mcomp_analise_FAT2AD_DBC, {atualizarFAT2AD_DBC()})
    observeEvent(input$fac.names_FAT2AD_DBC_f1, {atualizarFAT2AD_DBC()})
    observeEvent(input$fac.names_FAT2AD_DBC_f2, {atualizarFAT2AD_DBC()})
    observeEvent(input$sigT_analise_FAT2AD_DBC, {atualizarFAT2AD_DBC()})
    observeEvent(input$sigF_analise_FAT2AD_DBC, {atualizarFAT2AD_DBC()})
    observeEvent(input$unfold_analise_FAT2AD_DBC, {atualizarFAT2AD_DBC()})  
    
    #FAT2AD2_DIC
    observeEvent(input$respAd1_FAT2AD2_DIC, {atualizarFAT2AD2_DIC()})
    observeEvent(input$respAd2_FAT2AD2_DIC, {atualizarFAT2AD2_DIC()})
    observeEvent(input$mcomp_analise_FAT2AD2_DIC, {atualizarFAT2AD2_DIC()})
    observeEvent(input$fac.names_FAT2AD2_DIC_f1, {atualizarFAT2AD2_DIC()})
    observeEvent(input$fac.names_FAT2AD2_DIC_f2, {atualizarFAT2AD2_DIC()})
    observeEvent(input$sigT_analise_FAT2AD2_DIC, {atualizarFAT2AD2_DIC()})
    observeEvent(input$sigF_analise_FAT2AD2_DIC, {atualizarFAT2AD2_DIC()})
    observeEvent(input$unfold_analise_FAT2AD2_DIC, {atualizarFAT2AD2_DIC()})
    
    #FAT2AD2_DBC
    observeEvent(input$respAd1_FAT2AD2_DBC, {atualizarFAT2AD2_DBC()})
    observeEvent(input$respAd2_FAT2AD2_DBC, {atualizarFAT2AD2_DBC()})
    observeEvent(input$mcomp_analise_FAT2AD2_DBC, {atualizarFAT2AD2_DBC()})
    observeEvent(input$fac.names_FAT2AD2_DBC_f1, {atualizarFAT2AD2_DBC()})
    observeEvent(input$fac.names_FAT2AD2_DBC_f2, {atualizarFAT2AD2_DBC()})
    observeEvent(input$sigT_analise_FAT2AD2_DBC, {atualizarFAT2AD2_DBC()})
    observeEvent(input$sigF_analise_FAT2AD2_DBC, {atualizarFAT2AD2_DBC()})
    observeEvent(input$unfold_analise_FAT2AD2_DBC, {atualizarFAT2AD2_DBC()})
    
    #FAT3AD_DIC
    observeEvent(input$respAd_FAT3AD_DIC, {atualizarFAT3AD_DIC()})
    observeEvent(input$mcomp_analise_FAT3AD_DIC, {atualizarFAT3AD_DIC()})
    observeEvent(input$fac.names_FAT3AD_DIC_f1, {atualizarFAT3AD_DIC()})
    observeEvent(input$fac.names_FAT3AD_DIC_f2, {atualizarFAT3AD_DIC()})
    observeEvent(input$sigT_analise_FAT3AD_DIC, {atualizarFAT3AD_DIC()})
    observeEvent(input$sigF_analise_FAT3AD_DIC, {atualizarFAT3AD_DIC()})
    observeEvent(input$unfold_analise_FAT3AD_DIC, {atualizarFAT3AD_DIC()})
    
    #FAT3AD_DIC
    observeEvent(input$respAd_FAT3AD_DBC, {atualizarFAT3AD_DBC()})
    observeEvent(input$mcomp_analise_FAT3AD_DBC, {atualizarFAT3AD_DBC()})
    observeEvent(input$fac.names_FAT3AD_DBC_f1, {atualizarFAT3AD_DBC()})
    observeEvent(input$fac.names_FAT3AD_DBC_f2, {atualizarFAT3AD_DBC()})
    observeEvent(input$sigT_analise_FAT3AD_DBC, {atualizarFAT3AD_DBC()})
    observeEvent(input$sigF_analise_FAT3AD_DBC, {atualizarFAT3AD_DBC()})
    observeEvent(input$unfold_analise_FAT3AD_DBC, {atualizarFAT3AD_DBC()})
    
  observeEvent(input$ir_ajuda, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelAjuda')
    
  })    
  
  observeEvent(input$ir_site, {
    
    browseURL("https://github.com/crysttian/gexpdes2.0")
        
  }) 
  
  observeEvent(input$ir_croqui, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelCroqui')
    
  }) 
  
  observeEvent(input$ir_equipe, {
    
    updateTabsetPanel(inputId = "tabs",selected = 'panelEquipe')
    
  }) 
  
  observeEvent(input$ir_fechar, {
    
    stopApp()
    
  }) 
  
  observeEvent(input$ir_fechar_fim, {
    
    stopApp()
    
  }) 
  
  observeEvent(input$radio_importar, {
    
    if(input$radio_importar=="csv"){
      
      updateTabsetPanel(inputId = "tabs_importar_arquivo",selected = 'panel_csv')
      
    }
    
    if(input$radio_importar=="excel"){
      
      updateTabsetPanel(inputId = "tabs_importar_arquivo",selected = 'panel_excel')
      
    }

    if(input$radio_importar=="ods"){
      
      updateTabsetPanel(inputId = "tabs_importar_arquivo",selected = 'panel_ods')
      
    }
    
  }) 
  
  funcao_arquivo_csv = function(){
    
    dados <<- read.csv(arquivo$datapath,sep = input$radio_arquivo_csv_separador,dec = input$radio_arquivo_csv_decimal)
    
    output$tabela =  renderDataTable(dados,options = list(pageLength = 10))
    
    output$tabela_str <- renderPrint({str(dados)})
    
    output$tabela_delineamento =  renderDataTable(summary(dados),options = list(pageLength = 10))
    
    
  }
  
  observeEvent(input$arquivo_csv, {
    
    arquivo <<- input$arquivo_csv
    
    ext <- tools::file_ext(arquivo$datapath)
    
    if(ext!="csv"){
      
      shinyalert("Oops!", "Selecione um arquivo do tipo CSV!", type = "error")
      
    }else{
      
      funcao_arquivo_csv()
      
    }
    
  })      
  
  observeEvent(input$radio_arquivo_csv_separador, {
    
    if(!is.null(arquivo)){
      
      funcao_arquivo_csv()
      
    }
    
  },ignoreInit = TRUE)
  
  observeEvent(input$radio_arquivo_csv_decimal, {
    
    if(!is.null(arquivo)){
      
      funcao_arquivo_csv()
      
    }
    
  },ignoreInit = TRUE)
  
  observeEvent(input$arquivo_excel, {
    
    arquivo <<- input$arquivo_excel
    
    ext <- tools::file_ext(arquivo$datapath)
    
    if((ext!="xls")&&(ext!="xlsx")){
      
      shinyalert("Oops!", "Selecione um arquivo do Excel (xls ou xlsx)!", type = "error")
      
    }else{
      
      arquivo <<- input$arquivo_excel
      
      planilhas = excel_sheets(arquivo$datapath)
      
      dados <<- read_excel(arquivo$datapath,sheet = planilhas[1])
      
      updateSelectInput(inputId = "indice_excel",choices = planilhas)
      
      output$tabela =  renderDataTable(dados,options = list(pageLength = 10))
      
      output$tabela_str <- renderPrint({str(dados)})
      
      output$tabela_delineamento =  renderDataTable(summary(dados),options = list(pageLength = 10))
      
    }
    
  })     
  
  observeEvent(input$indice_excel, {
    
    arquivo <<- input$arquivo_excel
    
    dados <<- read_excel(arquivo$datapath,sheet = input$indice_excel)
    
    output$tabela =  renderDataTable(dados,options = list(pageLength = 10))
    
    output$tabela_str <- renderPrint({str(dados)})
    
    output$tabela_delineamento =  renderDataTable(summary(dados),options = list(pageLength = 10))
    
  },ignoreInit = TRUE) 

  observeEvent(input$arquivo_ods, {
    
    arquivo <<- input$arquivo_ods
    
    ext <- tools::file_ext(arquivo$datapath)
    
    if(ext!="ods"){
      
      shinyalert("Oops!", "Selecione um arquivo do OpenOffice ou LibreOffice (ods)!", type = "error")
      
    }else{
      
      arquivo <<- input$arquivo_ods
      
      planilhas = list_ods_sheets(arquivo$datapath)
      
      dados <<- read_ods(arquivo$datapath,sheet = planilhas[1])
      
      updateSelectInput(inputId = "indice_ods",choices = planilhas)
      
      output$tabela =  renderDataTable(dados,options = list(pageLength = 10))
      
      output$tabela_str <- renderPrint({str(dados)})
      
      output$tabela_delineamento =  renderDataTable(summary(dados),options = list(pageLength = 10))
      
    }
    
  })     
  
  observeEvent(input$indice_ods, {
    
    arquivo <<- input$arquivo_ods
    
    dados <<- read_ods(arquivo$datapath,sheet = input$indice_ods)
    
    output$tabela =  renderDataTable(dados,options = list(pageLength = 10))
    
    output$tabela_str <- renderPrint({str(dados)})
    
    output$tabela_delineamento =  renderDataTable(summary(dados),options = list(pageLength = 10))
    
  },ignoreInit = TRUE) 
  
  observeEvent(input$arquivo_lab,{
    
    if(input$arquivo_lab!="Seleciona um arquivo"){
      
      arq_temporario = tempfile()
      
      links = tools::findHTMLlinks()
      
      pkgRdDB = tools:::fetchRdDB(file.path(find.package('labestData'), 'help', 'labestData'))
    
      force(links)
      
      tools::Rd2HTML(pkgRdDB[[input$arquivo_lab]],arq_temporario, package = 'labestData',
                     links, no_links = is.null(links))
      
      
      output$labestData_informacao <- renderUI(includeHTML(arq_temporario))
      
      dados <<- eval(parse(text = paste("labestData::",input$arquivo_lab,sep="")))
      
      output$tabela_labestData =  renderDataTable(as.data.frame(dados),options = list(pageLength = 10))
      
      output$tabela_str <- renderPrint({str(dados)})
      
      output$tabela_delineamento =  renderDataTable(summary(dados),options = list(pageLength = 10))
      
    }
    
  })
  
  observeEvent(input$tipo_delineamento,{

    switch (input$tipo_delineamento,
            "dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_DIC"),
            "dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_DBC"),
            "dql" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_DQL"),
            "faixas" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_Faixas"),
            "fat2.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_FAT2_DIC"),
            "fat2.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_FAT2_DBC"),
            "fat3.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_FAT3_DIC"),
            "fat3.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_FAT3_DBC"),
            "fat2.ad.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_FAT2AD_DIC"),
            "fat2.ad.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_FAT2AD_DBC"),
            "fat2.ad2.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_FAT2AD2_DIC"),
            "fat2.ad2.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_FAT2AD2_DBC"),
            "fat3.ad.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_FAT3AD_DIC"),
            "fat3.ad.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_FAT3AD_DBC"),
            "psub2.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_PSUB_DIC"),
            "psub2.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento',selected = "tabs_PSUB_DBC")
    )   
    
  })
  
  observeEvent(input$tipo_delineamento_croqui,{

    switch (input$tipo_delineamento_croqui,
            "dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_DIC_croqui"),
            "dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_DBC_croqui"),
            "dql" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_DQL_croqui"),
            "faixas" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_Faixas_croqui"),
            "fat2.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_FAT2_DIC_croqui"),
            "fat2.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_FAT2_DBC_croqui"),
            "fat3.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_FAT3_DIC_croqui"),
            "fat3.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_FAT3_DBC_croqui"),
            "fat2.ad.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_FAT2AD_DIC_croqui"),
            "fat2.ad.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_FAT2AD_DBC_croqui"),
            "fat2.ad2.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_FAT2AD2_DIC_croqui"),
            "fat2.ad2.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_FAT2AD2_DBC_croqui"),
            "fat3.ad.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_FAT3AD_DIC_croqui"),
            "fat3.ad.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_FAT3AD_DBC_croqui"),
            "psub2.dic" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_PSUB_DIC_croqui"),
            "psub2.dbc" = updateTabsetPanel(inputId = 'painel_opcao_delineamento_croqui',selected = "tabs_PSUB_DBC_croqui")
    )   
    
  })

  gerar_croqui_DIC = function(){
    
    output$saida_croqui <- renderPrint({

      switch(input$croqui_numeracao_trat,
             
             "maiuscula" = {
               trt_croqui = LETTERS[1:eval(input$croqui_n_trat)]
             },
             
             "minuscula" = {
               trt_croqui = letters[1:eval(input$croqui_n_trat)]
             },
             
             "arabico" = {
               trt_croqui = 1:eval(input$croqui_n_trat)
             },
             
             "romano" = {
               trt_croqui = as.roman(1:eval(input$croqui_n_trat))
             }
      )
      
      trt_croqui = paste(input$croqui_prefixo_trat,trt_croqui,sep="")
      
      r_croqui = eval(input$croqui_rep_trat)
      
      croqui = agricolae::design.crd(trt = trt_croqui,r = r_croqui)
      
      colnames(croqui$book) = c("Código da Parcela", "Repetição", "Tratamento")
      
      croqui$book
          
    })
    
    output$grafico_croqui <- renderPlot({
      
      switch(input$croqui_numeracao_trat,

             "maiuscula" = {
               trt_croqui = LETTERS[1:eval(input$croqui_n_trat)]
             },

             "minuscula" = {
               trt_croqui = letters[1:eval(input$croqui_n_trat)]
             },

             "arabico" = {
               trt_croqui = 1:eval(input$croqui_n_trat)
             },

             "romano" = {
               trt_croqui = as.roman(1:eval(input$croqui_n_trat))
             }
      )

      trt_croqui = paste(input$croqui_prefixo_trat,trt_croqui,sep="")

      r_croqui = eval(input$croqui_rep_trat)

      croqui = agricolae::design.crd(trt = trt_croqui,r = r_croqui)

      plot_design_crd(design =  croqui, 
                      ncols = eval(input$croqui_n_trat),
                      nrows = eval(input$croqui_rep_trat),
                      factor_name = "trt_croqui"
                     )
      
    })
    
  }

  gerar_croqui_DBC = function(){
    
    output$saida_croqui <- renderPrint({
      
      switch(input$croqui_numeracao_trat_DBC,
             
             "maiuscula" = {
               trt_croqui = LETTERS[1:eval(input$croqui_n_trat_DBC)]
             },
             
             "minuscula" = {
               trt_croqui = letters[1:eval(input$croqui_n_trat_DBC)]
             },
             
             "arabico" = {
               trt_croqui = 1:eval(input$croqui_n_trat_DBC)
             },
             
             "romano" = {
               trt_croqui = as.roman(1:eval(input$croqui_n_trat_DBC))
             }
      )
      
      trt_croqui = paste(input$croqui_prefixo_trat_DBC,trt_croqui,sep="")
      
      bloco_croqui = eval(input$croqui_bloco_DBC)
      
      croqui = agricolae::design.rcbd(trt = trt_croqui,r = bloco_croqui)
      
      colnames(croqui$book) = c("Código da Parcela", "Bloco", "Tratamento")
      
      croqui$book
      
    })
    
    output$grafico_croqui <- renderPlot({
      
      switch(input$croqui_numeracao_trat_DBC,
             
             "maiuscula" = {
               trt_croqui = LETTERS[1:eval(input$croqui_n_trat_DBC)]
             },
             
             "minuscula" = {
               trt_croqui = letters[1:eval(input$croqui_n_trat_DBC)]
             },
             
             "arabico" = {
               trt_croqui = 1:eval(input$croqui_n_trat_DBC)
             },
             
             "romano" = {
               trt_croqui = as.roman(1:eval(input$croqui_n_trat_DBC))
             }
      )
      
      Tratamento = paste(input$croqui_prefixo_trat_DBC,trt_croqui,sep="")
      
      Bloco = eval(input$croqui_bloco_DBC)
      
      croqui = agricolae::design.rcbd(trt = Tratamento,r = Bloco)

      plot_rcdb(design =  croqui,factor_name = "Tratamento",y="block")
      
    })
    
  }  

  gerar_croqui_DQL = function(){
    
    output$saida_croqui <- renderPrint({
      
      switch(input$croqui_numeracao_trat_DQL,
             
             "maiuscula" = {
               trt_croqui = LETTERS[1:eval(input$croqui_n_trat_DQL)]
             },
             
             "minuscula" = {
               trt_croqui = letters[1:eval(input$croqui_n_trat_DQL)]
             },
             
             "arabico" = {
               trt_croqui = 1:eval(input$croqui_n_trat_DQL)
             },
             
             "romano" = {
               trt_croqui = as.roman(1:eval(input$croqui_n_trat_DQL))
             }
      )
      
      trt_croqui = paste(input$croqui_prefixo_trat_DQL,trt_croqui,sep="")
      
      croqui = agricolae::design.lsd(trt = trt_croqui)
      
      colnames(croqui$book) = c("Código da Parcela", "Linha", "Coluna","Tratamento")
      
      croqui$book
      
    })
    
    output$grafico_croqui <- renderPlot({
      
      switch(input$croqui_numeracao_trat_DQL,
             
             "maiuscula" = {
               trt_croqui = LETTERS[1:eval(input$croqui_n_trat_DQL)]
             },
             
             "minuscula" = {
               trt_croqui = letters[1:eval(input$croqui_n_trat_DQL)]
             },
             
             "arabico" = {
               trt_croqui = 1:eval(input$croqui_n_trat_DQL)
             },
             
             "romano" = {
               trt_croqui = as.roman(1:eval(input$croqui_n_trat_DQL))
             }
      )
      
      Tratamento = paste(input$croqui_prefixo_trat_DQL,trt_croqui,sep="")
      
      croqui = agricolae::design.lsd(trt = Tratamento)
      
      plot_latin_square(design =  croqui,factor_name = "Tratamento")
      
    })
    
  }  

  gerar_croqui_FAIXAS = function(){
    
    output$saida_croqui <- renderPrint({
      
      switch(input$croqui_numeracao_trat1_FAIXAS,
             
             "maiuscula" = {
               trt_croqui_trat1 = LETTERS[1:eval(input$croqui_n_trat1_FAIXAS)]
             },
             
             "minuscula" = {
               trt_croqui_trat1 = letters[1:eval(input$croqui_n_tra1t_FAIXAS)]
             },
             
             "arabico" = {
               trt_croqui_trat1 = 1:eval(input$croqui_n_trat1_FAIXAS)
             },
             
             "romano" = {
               trt_croqui_trat1 = as.roman(1:eval(input$croqui_n_trat1_FAIXAS))
             }
      )
      
      trt1_croqui = paste(input$croqui_prefixo_trat1_FAIXAS,trt_croqui_trat1,sep="")
      
      switch(input$croqui_numeracao_trat2_FAIXAS,
             
             "maiuscula" = {
               trt_croqui_trat2 = LETTERS[1:eval(input$croqui_n_trat2_FAIXAS)]
             },
             
             "minuscula" = {
               trt_croqui_trat2 = letters[1:eval(input$croqui_n_trat2_FAIXAS)]
             },
             
             "arabico" = {
               trt_croqui_trat2 = 1:eval(input$croqui_n_trat2_FAIXAS)
             },
             
             "romano" = {
               trt_croqui_trat2 = as.roman(1:eval(input$croqui_n_trat2_FAIXAS))
             }
      )
      
      trt2_croqui = paste(input$croqui_prefixo_trat2_FAIXAS,trt_croqui_trat2,sep="")
      
      bloco_croqui = eval(input$croqui_blocos_FAIXAS)
      
      croqui = agricolae::design.strip(trt1 = trt1_croqui,trt2 = trt2_croqui, r = bloco_croqui)
      
      colnames(croqui$book) = c("Código da Parcela", "Bloco","Fator 1", "Fator 2")
      
      croqui$book
      
    })
    
    output$grafico_croqui <- renderPlot({
      
      switch(input$croqui_numeracao_trat1_FAIXAS,
             
             "maiuscula" = {
               trt_croqui_trat1 = LETTERS[1:eval(input$croqui_n_trat1_FAIXAS)]
             },
             
             "minuscula" = {
               trt_croqui_trat1 = letters[1:eval(input$croqui_n_tra1t_FAIXAS)]
             },
             
             "arabico" = {
               trt_croqui_trat1 = 1:eval(input$croqui_n_trat1_FAIXAS)
             },
             
             "romano" = {
               trt_croqui_trat1 = as.roman(1:eval(input$croqui_n_trat1_FAIXAS))
             }
      )
      
      Fator_1 = paste(input$croqui_prefixo_trat1_FAIXAS,trt_croqui_trat1,sep="")
      
      switch(input$croqui_numeracao_trat2_FAIXAS,
             
             "maiuscula" = {
               trt_croqui_trat2 = LETTERS[1:eval(input$croqui_n_trat2_FAIXAS)]
             },
             
             "minuscula" = {
               trt_croqui_trat2 = letters[1:eval(input$croqui_n_trat2_FAIXAS)]
             },
             
             "arabico" = {
               trt_croqui_trat2 = 1:eval(input$croqui_n_trat2_FAIXAS)
             },
             
             "romano" = {
               trt_croqui_trat2 = as.roman(1:eval(input$croqui_n_trat2_FAIXAS))
             }
      )
      
      Fator_2 = paste(input$croqui_prefixo_trat2_FAIXAS,trt_croqui_trat2,sep="")
      
      bloco_croqui = eval(input$croqui_blocos_FAIXAS)
      
      croqui = agricolae::design.strip(trt1 = Fator_1,trt2 = Fator_2, r = bloco_croqui)
      
      plot_strip(design =  croqui,factor_name_1 = "Fator_1", factor_name_2 = "Fator_2")
      
    })
    
  }    
  
  gerar_croqui_FAT2_DIC = function(){
    
    output$saida_croqui <- renderPrint({
      
      fator1_croqui = eval(input$croqui_n_fator1_FAT2_DIC)
      
      fator2_croqui = eval(input$croqui_n_fator2_FAT2_DIC)
      
      r_croqui = eval(input$croqui_rep_fator_FAT2_DIC)
      
      croqui = agricolae::design.ab(trt = c(fator1_croqui,fator2_croqui),r = r_croqui,design = 'crd')
      
      colnames(croqui$book) = c("Código da Parcela", "Repetição", "Fator A", "Fator B")
      
      croqui$book
      
    })
    
    output$grafico_croqui <- renderPlot({
      
      fator1_croqui = eval(input$croqui_n_fator1_FAT2_DIC)
      
      fator2_croqui = eval(input$croqui_n_fator2_FAT2_DIC)
      
      r_croqui = eval(input$croqui_rep_fator_FAT2_DIC)
      
      croqui = agricolae::design.ab(trt = c(fator1_croqui,fator2_croqui),r = r_croqui,design = 'crd')
      
      croqui$book
      
      plot_design.factorial_crd(design = croqui,nrows = r_croqui, ncols = fator1_croqui * fator2_croqui)
      
    })
    
  }

  gerar_croqui_FAT3_DIC = function(){
    
    output$saida_croqui <- renderPrint({
      
      fator1_croqui = eval(input$croqui_n_fator1_FAT3_DIC)
      
      fator2_croqui = eval(input$croqui_n_fator2_FAT3_DIC)
      
      fator3_croqui = eval(input$croqui_n_fator3_FAT3_DIC)
      
      r_croqui = eval(input$croqui_rep_fator_FAT3_DIC)
      
      croqui = agricolae::design.ab(trt = c(fator1_croqui,fator2_croqui,fator3_croqui),r = r_croqui,design = 'crd')
      
      colnames(croqui$book) = c("Código da Parcela", "Repetição", "Fator A", "Fator B", "Fator C")
      
      croqui$book
      
    })
    
    output$grafico_croqui <- renderPlot({
      
      fator1_croqui = eval(input$croqui_n_fator1_FAT3_DIC)
      
      fator2_croqui = eval(input$croqui_n_fator2_FAT3_DIC)
      
      fator3_croqui = eval(input$croqui_n_fator3_FAT3_DIC)
      
      r_croqui = eval(input$croqui_rep_fator_FAT2_DIC)
      
      croqui = agricolae::design.ab(trt = c(fator1_croqui,fator2_croqui,fator3_croqui),r = r_croqui,design = 'crd')
      
      croqui$book
      
      plot_design.factorial_crd(design = croqui,nrows = r_croqui, ncols = fator1_croqui * fator2_croqui * fator3_croqui)
      
    })
    
  }  
  
  gerar_croqui_FAT2_DBC = function(){
    
    output$saida_croqui <- renderPrint({
      
      fator1_croqui = eval(input$croqui_n_fator1_FAT2_DBC)
      
      fator2_croqui = eval(input$croqui_n_fator2_FAT2_DBC)
      
      blocos_croqui = eval(input$croqui_bloco_fator_FAT2_DBC)
      
      croqui = agricolae::design.ab(trt = c(fator1_croqui,fator2_croqui),r = blocos_croqui,design = 'rcbd')
      
      colnames(croqui$book) = c("Código da Parcela", "Bloco", "Fator A", "Fator B")
      
      croqui$book
      
    })
    
    output$grafico_croqui <- renderPlot({
      
      fator1_croqui = eval(input$croqui_n_fator1_FAT2_DBC)
      
      fator2_croqui = eval(input$croqui_n_fator2_FAT2_DBC)
      
      blocos_croqui = eval(input$croqui_bloco_fator_FAT2_DBC)
      
      croqui = agricolae::design.ab(trt = c(fator1_croqui,fator2_croqui), r = blocos_croqui,design = 'rcbd')
      
      plot_design.factorial_rcbd(design = croqui)
      
    })
    
  }

  gerar_croqui_FAT3_DBC = function(){
    
    output$saida_croqui <- renderPrint({
      
      fator1_croqui = eval(input$croqui_n_fator1_FAT3_DBC)
      
      fator2_croqui = eval(input$croqui_n_fator2_FAT3_DBC)
      
      fator3_croqui = eval(input$croqui_n_fator3_FAT3_DBC)
      
      blocos_croqui = eval(input$croqui_bloco_fator_FAT3_DBC)
      
      croqui = agricolae::design.ab(trt = c(fator1_croqui,fator2_croqui,fator3_croqui),r = blocos_croqui,design = 'rcbd')
      
      colnames(croqui$book) = c("Código da Parcela", "Bloco", "Fator A", "Fator B","Fator C")
      
      croqui$book
      
    })
    
    output$grafico_croqui <- renderPlot({
      
      fator1_croqui = eval(input$croqui_n_fator1_FAT3_DBC)
      
      fator2_croqui = eval(input$croqui_n_fator2_FAT3_DBC)
      
      fator3_croqui = eval(input$croqui_n_fator3_FAT3_DBC)
      
      blocos_croqui = eval(input$croqui_bloco_fator_FAT3_DBC)
      
      croqui = agricolae::design.ab(trt = c(fator1_croqui,fator2_croqui,fator3_croqui),r = blocos_croqui,design = 'rcbd')
      
      plot_design.factorial_rcbd(design = croqui)
      
    })
    
  }  
  
  gerar_croqui_PSUB_DIC = function(){
    
    output$saida_croqui <- renderPrint({
      
      switch(input$croqui_numeracao_trat1_PSUB_DIC,
             
             "maiuscula" = {
               trt_croqui_trat1 = LETTERS[1:eval(input$croqui_n_trat1_PSUB_DIC)]
             },
             
             "minuscula" = {
               trt_croqui_trat1 = letters[1:eval(input$croqui_n_tra1t_PSUB_DIC)]
             },
             
             "arabico" = {
               trt_croqui_trat1 = 1:eval(input$croqui_n_trat1_PSUB_DIC)
             },
             
             "romano" = {
               trt_croqui_trat1 = as.roman(1:eval(input$croqui_n_trat1_PSUB_DIC))
             }
      )
      
      trt1_croqui = paste(input$croqui_prefixo_trat1_PSUB_DIC,trt_croqui_trat1,sep="")
      
      switch(input$croqui_numeracao_trat2_PSUB_DIC,
             
             "maiuscula" = {
               trt_croqui_trat2 = LETTERS[1:eval(input$croqui_n_trat2_PSUB_DIC)]
             },
             
             "minuscula" = {
               trt_croqui_trat2 = letters[1:eval(input$croqui_n_trat2_PSUB_DIC)]
             },
             
             "arabico" = {
               trt_croqui_trat2 = 1:eval(input$croqui_n_trat2_PSUB_DIC)
             },
             
             "romano" = {
               trt_croqui_trat2 = as.roman(1:eval(input$croqui_n_trat2_PSUB_DIC))
             }
      )
      
      trt2_croqui = paste(input$croqui_prefixo_trat2_PSUB_DIC,trt_croqui_trat2,sep="")
      
      r_croqui = eval(input$croqui_rep_PSUB_DIC)
      
      croqui = agricolae::design.split(trt1 = trt1_croqui,trt2 = trt2_croqui, r = r_croqui, design = "crd")
      
      croqui$book
      
    })
    
    output$grafico_croqui <- renderPlot({
      
      switch(input$croqui_numeracao_trat1_PSUB_DIC,
             
             "maiuscula" = {
               trt_croqui_trat1 = LETTERS[1:eval(input$croqui_n_trat1_PSUB_DIC)]
             },
             
             "minuscula" = {
               trt_croqui_trat1 = letters[1:eval(input$croqui_n_tra1t_PSUB_DIC)]
             },
             
             "arabico" = {
               trt_croqui_trat1 = 1:eval(input$croqui_n_trat1_PSUB_DIC)
             },
             
             "romano" = {
               trt_croqui_trat1 = as.roman(1:eval(input$croqui_n_trat1_PSUB_DIC))
             }
      )
      
      Fator_1 = paste(input$croqui_prefixo_trat1__PSUB_DIC,trt_croqui_trat1,sep="")
      
      switch(input$croqui_numeracao_trat2_PSUB_DIC,
             
             "maiuscula" = {
               trt_croqui_trat2 = LETTERS[1:eval(input$croqui_n_trat2_PSUB_DIC)]
             },
             
             "minuscula" = {
               trt_croqui_trat2 = letters[1:eval(input$croqui_n_trat2_PSUB_DIC)]
             },
             
             "arabico" = {
               trt_croqui_trat2 = 1:eval(input$croqui_n_trat2_PSUB_DIC)
             },
             
             "romano" = {
               trt_croqui_trat2 = as.roman(1:eval(input$croqui_n_trat2_PSUB_DIC))
             }
      )
      
      Fator_2 = paste(input$croqui_prefixo_trat2_PSUB_DIC,trt_croqui_trat2,sep="")
      
      r_croqui = eval(input$croqui_rep_PSUB_DIC)
      
      croqui = agricolae::design.split(trt1 = Fator_1,trt2 = Fator_2, r = r_croqui, design = "crd")
      
      print(croqui)
      
      plot_split_crd(design =  croqui,
                     factor_name_1 = "Fator_1", 
                     factor_name_2 = "Fator_2", 
                     ncols = eval(input$croqui_n_trat1_PSUB_DIC)+1,
                     nrows = 100)
      
    })
    
  }   

  gerar_croqui_PSUB_DBC = function(){
    
    output$saida_croqui <- renderPrint({
      
      switch(input$croqui_numeracao_trat1_PSUB_DBC,
             
             "maiuscula" = {
               trt_croqui_trat1 = LETTERS[1:eval(input$croqui_n_trat1_PSUB_DBC)]
             },
             
             "minuscula" = {
               trt_croqui_trat1 = letters[1:eval(input$croqui_n_tra1t_PSUB_DBC)]
             },
             
             "arabico" = {
               trt_croqui_trat1 = 1:eval(input$croqui_n_trat1_PSUB_DBC)
             },
             
             "romano" = {
               trt_croqui_trat1 = as.roman(1:eval(input$croqui_n_trat1_PSUB_DBC))
             }
      )
      
      trt1_croqui = paste(input$croqui_prefixo_trat1_PSUB_DBC,trt_croqui_trat1,sep="")
      
      switch(input$croqui_numeracao_trat2_PSUB_DBC,
             
             "maiuscula" = {
               trt_croqui_trat2 = LETTERS[1:eval(input$croqui_n_trat2_PSUB_DBC)]
             },
             
             "minuscula" = {
               trt_croqui_trat2 = letters[1:eval(input$croqui_n_trat2_PSUB_DBC)]
             },
             
             "arabico" = {
               trt_croqui_trat2 = 1:eval(input$croqui_n_trat2_PSUB_DBC)
             },
             
             "romano" = {
               trt_croqui_trat2 = as.roman(1:eval(input$croqui_n_trat2_PSUB_DBC))
             }
      )
      
      trt2_croqui = paste(input$croqui_prefixo_trat2_PSUB_DBC,trt_croqui_trat2,sep="")
      
      bloco_croqui = eval(input$croqui_bloco_PSUB_DBC)
      
      croqui = agricolae::design.split(trt1 = trt1_croqui,trt2 = trt2_croqui, r = bloco_croqui, design = "rcbd")
      
      croqui$book
      
    })
    
    output$grafico_croqui <- renderPlot({
      
      switch(input$croqui_numeracao_trat1_PSUB_DBC,
             
             "maiuscula" = {
               trt_croqui_trat1 = LETTERS[1:eval(input$croqui_n_trat1_PSUB_DBC)]
             },
             
             "minuscula" = {
               trt_croqui_trat1 = letters[1:eval(input$croqui_n_tra1t_PSUB_DBC)]
             },
             
             "arabico" = {
               trt_croqui_trat1 = 1:eval(input$croqui_n_trat1_PSUB_DBC)
             },
             
             "romano" = {
               trt_croqui_trat1 = as.roman(1:eval(input$croqui_n_trat1_PSUB_DBC))
             }
      )
      
      Fator_1 = paste(input$croqui_prefixo_trat1__PSUB_DBC,trt_croqui_trat1,sep="")
      
      switch(input$croqui_numeracao_trat2_PSUB_DBC,
             
             "maiuscula" = {
               trt_croqui_trat2 = LETTERS[1:eval(input$croqui_n_trat2_PSUB_DBC)]
             },
             
             "minuscula" = {
               trt_croqui_trat2 = letters[1:eval(input$croqui_n_trat2_PSUB_DBC)]
             },
             
             "arabico" = {
               trt_croqui_trat2 = 1:eval(input$croqui_n_trat2_PSUB_DBC)
             },
             
             "romano" = {
               trt_croqui_trat2 = as.roman(1:eval(input$croqui_n_trat2_PSUB_DBC))
             }
      )
      
      Fator_2 = paste(input$croqui_prefixo_trat2_PSUB_DBC,trt_croqui_trat2,sep="")
      
      bloco_croqui = eval(input$croqui_bloco_PSUB_DBC)
      
      croqui = agricolae::design.split(trt1 = Fator_1,trt2 = Fator_2, r = bloco_croqui, design = "rcbd")
      
      print(croqui)
      
      plot_split_rcbd(design =  croqui,
                     factor_name_1 = "Fator_1", 
                     factor_name_2 = "Fator_2")
      
    })
    
  }
              
  observeEvent(input$gerar_croqui, {
    
    switch (input$tipo_delineamento_croqui,
            "dic" = {gerar_croqui_DIC()},
            "dbc" = {gerar_croqui_DBC()},
            "dql" = {gerar_croqui_DQL()},
            "faixas" = {gerar_croqui_FAIXAS()},
            "fat2.dic" = {gerar_croqui_FAT2_DIC()},
            "fat2.dbc" = {gerar_croqui_FAT2_DBC()},
            "fat3.dic" = {gerar_croqui_FAT3_DIC()},
            "fat3.dbc" = {gerar_croqui_FAT3_DBC()},
            "psub2.dic" = {gerar_croqui_PSUB_DIC()},
            "psub2.dbc" = {gerar_croqui_PSUB_DBC()}
    )
    
  })
  
  #Relatorios
  
  output$relatorio <- downloadHandler(
    
    filename = function(){paste("relatorio-", Sys.Date(), ".txt", sep="")},
    
    content = function(file){
      
      saida <- capture.output(
        
        switch (input$tipo_delineamento,
                "dic" = {
                  
                  dic(trat = eval(parse(text = paste("dados$",input$trat_DIC,sep=""))),
                      resp = eval(parse(text = paste("dados$",input$resp_DIC,sep=""))), 
                      quali = eval(parse(text = input$quali_DIC)),
                      mcomp = input$mcomp_analise_DIC, 
                      nl = eval(parse(text = input$nl_analise_DIC)),
                      hvar = input$hvar_analise_DIC,
                      sigT = input$sigT_analise_DIC,
                      sigF = input$sigF_analise_DIC,
                      unfold = switch(input$unfold_analise_DIC, "Nenhum"=NULL,input$unfold_analise_DIC)) 
                },
                
                "dbc" = {
                  
                  dbc(trat = eval(parse(text = paste("dados$",input$trat_DBC,sep=""))),
                      bloco = eval(parse(text = paste("dados$",input$bloco_DBC,sep=""))),
                      resp = eval(parse(text = paste("dados$",input$resp_DBC,sep=""))), 
                      quali = eval(parse(text = input$quali_DBC)),
                      mcomp = input$mcomp_analise_DBC, 
                      nl = eval(parse(text = input$nl_analise_DBC)),
                      hvar = input$hvar_analise_DBC,
                      sigT = input$sigT_analise_DBC,
                      sigF = input$sigF_analise_DBC,
                      unfold = switch(input$unfold_analise_DBC, "Nenhum"=NULL,input$unfold_analise_DBC))
                },
                
                "dql" = {
                  
                  dql(trat = eval(parse(text = paste("dados$",input$trat_DQL,sep=""))),
                      linha = eval(parse(text = paste("dados$",input$linha_DQL,sep=""))),
                      coluna = eval(parse(text = paste("dados$",input$coluna_DQL,sep=""))),
                      resp = eval(parse(text = paste("dados$",input$resp_DQL,sep=""))), 
                      quali = eval(parse(text = input$quali_DQL)),
                      mcomp = input$mcomp_analise_DQL, 
                      sigT = input$sigT_analise_DQL,
                      sigF = input$sigF_analise_DQL,
                      unfold = switch(input$unfold_analise_DQL, "Nenhum"=NULL,input$unfold_analise_DQL))
                  
                },
                
                "faixas" = {
                  
                  faixas(fator1 = eval(parse(text = paste("dados$",input$fator1_faixas,sep=""))),
                         fator2 = eval(parse(text = paste("dados$",input$fator2_faixas,sep=""))),
                         bloco = eval(parse(text = paste("dados$",input$bloco_faixas,sep=""))),
                         resp = eval(parse(text = paste("dados$",input$resp_faixas,sep=""))), 
                         quali = c(eval(parse(text = input$quali_fator1_faixas)),eval(parse(text = input$quali_fator2_faixas))),
                         mcomp = input$mcomp_analise_faixas,
                         fac.names = c(input$fac.names_faixas_f1,input$fac.names_faixas_f2),
                         sigT = input$sigT_analise_faixas,
                         sigF = input$sigF_analise_faixas,
                         unfold = switch(input$unfold_analise_faixas, "Nenhum"=NULL,input$unfold_analise_faixas)
                         )
                  
                },
                "fat2.dic" = {
                  
                  fat2.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2_DIC,sep=""))),
                           fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2_DIC,sep=""))),
                           resp = eval(parse(text = paste("dados$",input$resp_FAT2_DIC,sep=""))), 
                           quali = c(eval(parse(text = input$quali_fator1_FAT2_DIC)),eval(parse(text = input$quali_fator2_FAT2_DIC))),
                           mcomp = input$mcomp_analise_FAT2_DIC,
                           fac.names = c(input$fac.names_FAT2_DIC_f1,input$fac.names_FAT2_DIC_f2),
                           sigT = input$sigT_analise_FAT2_DIC,
                           sigF = input$sigF_analise_FAT2_DIC,
                           unfold = switch(input$unfold_analise_FAT2_DIC, "Nenhum"=NULL,input$unfold_analise_FAT2_DIC)
                           )
                  
                },
                "fat2.dbc" = {
                  
                  fat2.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2_DBC,sep=""))),
                           fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2_DBC,sep=""))),
                           bloco = eval(parse(text = paste("dados$",input$bloco_FAT2_DBC,sep=""))),
                           resp = eval(parse(text = paste("dados$",input$resp_FAT2_DBC,sep=""))), 
                           quali = c(eval(parse(text = input$quali_fator1_FAT2_DBC)),eval(parse(text = input$quali_fator2_FAT2_DBC))),
                           mcomp = input$mcomp_analise_FAT2_DBC,
                           fac.names = c(input$fac.names_FAT2_DBC_f1,input$fac.names_FAT2_DBC_f2),
                           sigT = input$sigT_analise_FAT2_DBC,
                           sigF = input$sigF_analise_FAT2_DBC,
                           unfold = switch(input$unfold_analise_FAT2_DBC, "Nenhum"=NULL,input$unfold_analise_FAT2_DBC)
                           )
                  
                },
                "fat3.dic" = {
                  
                  fat3.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT3_DIC,sep=""))),
                           fator2 = eval(parse(text = paste("dados$",input$fator2_FAT3_DIC,sep=""))),
                           fator3 = eval(parse(text = paste("dados$",input$fator3_FAT3_DIC,sep=""))),
                           resp = eval(parse(text = paste("dados$",input$resp_FAT3_DIC,sep=""))), 
                           quali = c(eval(parse(text = input$quali_fator1_FAT3_DIC)),eval(parse(text = input$quali_fator2_FAT3_DIC)), eval(parse(text = input$quali_fator3_FAT3_DIC))),
                           mcomp = input$mcomp_analise_FAT3_DIC,
                           fac.names = c(input$fac.names_FAT3_DIC_f1,input$fac.names_FAT3_DIC_f2,input$fac.names_FAT3_DIC_f3),
                           sigT = input$sigT_analise_FAT3_DIC,
                           sigF = input$sigF_analise_FAT3_DIC,
                           unfold = switch(input$unfold_analise_FAT3_DIC, "Nenhum"=NULL,input$unfold_analise_FAT3_DIC)
                  )
                  
                },
                "fat3.dbc" = {
                 
                  fat3.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT3_DBC,sep=""))),
                           fator2 = eval(parse(text = paste("dados$",input$fator2_FAT3_DBC,sep=""))),
                           fator3 = eval(parse(text = paste("dados$",input$fator3_FAT3_DBC,sep=""))),
                           bloco = eval(parse(text = paste("dados$",input$bloco_FAT3_DBC,sep=""))),
                           resp = eval(parse(text = paste("dados$",input$resp_FAT3_DBC,sep=""))), 
                           quali = c(eval(parse(text = input$quali_fator1_FAT3_DBC)),eval(parse(text = input$quali_fator2_FAT3_DBC)), eval(parse(text = input$quali_fator3_FAT3_DBC))),
                           mcomp = input$mcomp_analise_FAT3_DBC,
                           fac.names = c(input$fac.names_FAT3_DBC_f1,input$fac.names_FAT3_DBC_f2,input$fac.names_FAT3_DBC_f3),
                           sigT = input$sigT_analise_FAT3_DBC,
                           sigF = input$sigF_analise_FAT3_DBC,
                           unfold = switch(input$unfold_analise_FAT3_DBC, "Nenhum"=NULL,input$unfold_analise_FAT3_DBC)
                           )
                  
                },
                "psub2.dic" = {
                  
                  psub2.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_PSUB_DIC,sep=""))),
                            fator2 = eval(parse(text = paste("dados$",input$fator2_PSUB_DIC,sep=""))),
                            repet = eval(parse(text = paste("dados$",input$repet_PSUB_DIC,sep=""))),
                            resp = eval(parse(text = paste("dados$",input$resp_PSUB_DIC,sep=""))), 
                            quali = c(eval(parse(text = input$quali_fator1_PSUB_DIC)),eval(parse(text = input$quali_fator2_PSUB_DIC))),
                            mcomp = input$mcomp_analise_PSUB_DIC,
                            fac.names = c(input$fac.names_PSUB_DIC_f1,input$fac.names_PSUB_DIC_f2),
                            sigT = input$sigT_analise_PSUB_DIC,
                            sigF = input$sigF_analise_PSUB_DIC,
                            unfold = switch(input$unfold_analise_PSUB_DIC, "Nenhum"=NULL,input$unfold_analise_PSUB_DIC)
                            )
                  
                },
                "psub2.dbc" = {
                  
                  psub2.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_PSUB_DBC,sep=""))),
                            fator2 = eval(parse(text = paste("dados$",input$fator2_PSUB_DBC,sep=""))),
                            bloco = eval(parse(text = paste("dados$",input$bloco_PSUB_DBC,sep=""))),
                            resp = eval(parse(text = paste("dados$",input$resp_PSUB_DBC,sep=""))), 
                            quali = c(eval(parse(text = input$quali_fator1_PSUB_DBC)),eval(parse(text = input$quali_fator2_PSUB_DBC))),
                            mcomp = input$mcomp_analise_PSUB_DBC,
                            fac.names = c(input$fac.names_PSUB_DBC_f1,input$fac.names_PSUB_DBC_f2),
                            sigT = input$sigT_analise_PSUB_DBC,
                            sigF = input$sigF_analise_PSUB_DBC,
                            unfold = switch(input$unfold_analise_PSUB_DBC, "Nenhum"=NULL,input$unfold_analise_PSUB_DBC)
                            )
                  
                },
                "fat2.ad.dic" = {
                
                  fat2.ad.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2AD_DIC,sep=""))),
                              fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2AD_DIC,sep=""))),
                              repet = eval(parse(text = paste("dados$",input$repet_FAT2AD_DIC,sep=""))),
                              resp = eval(parse(text = paste("dados$",input$resp_FAT2AD_DIC,sep=""))), 
                              respAd = resposta_adicional_FAT2AD_DIC,
                              quali = c(eval(parse(text = input$quali_fator1_FAT2AD_DIC)),eval(parse(text = input$quali_fator2_FAT2AD_DIC))),
                              mcomp = input$mcomp_analise_FAT2AD_DIC,
                              fac.names = c(input$fac.names_FAT2AD_DIC_f1,input$fac.names_FAT2AD_DIC_f2),
                              sigT = input$sigT_analise_FAT2AD_DIC,
                              sigF = input$sigF_analise_FAT2AD_DIC,
                              unfold = switch(input$unfold_analise_FAT2AD_DIC, "Nenhum"=NULL,input$unfold_analise_FAT2AD_DIC)
                              )
                  
                },
                "fat2.ad.dbc" = {
                
                  fat2.ad.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2AD_DBC,sep=""))),
                              fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2AD_DBC,sep=""))),
                              bloco = eval(parse(text = paste("dados$",input$bloco_FAT2AD_DBC,sep=""))),
                              resp = eval(parse(text = paste("dados$",input$resp_FAT2AD_DBC,sep=""))), 
                              respAd = resposta_adicional_FAT2AD_DBC,
                              quali = c(eval(parse(text = input$quali_fator1_FAT2AD_DBC)),eval(parse(text = input$quali_fator2_FAT2AD_DBC))),
                              mcomp = input$mcomp_analise_FAT2AD_DBC,
                              fac.names = c(input$fac.names_FAT2AD_DBC_f1,input$fac.names_FAT2AD_DBC_f2),
                              sigT = input$sigT_analise_FAT2AD_DBC,
                              sigF = input$sigF_analise_FAT2AD_DBC,
                              unfold = switch(input$unfold_analise_FAT2AD_DBC, "Nenhum"=NULL,input$unfold_analise_FAT2AD_DBC)
                              )  
                  
                },
                "fat2.ad2.dic" = {
                
                  fat2.ad2.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2AD2_DIC,sep=""))),
                               fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DIC,sep=""))),
                               repet = eval(parse(text = paste("dados$",input$repet_FAT2AD2_DIC,sep=""))),
                               resp = eval(parse(text = paste("dados$",input$resp_FAT2AD2_DIC,sep=""))), 
                               respAd1 = resposta_adicional1_FAT2AD2_DIC,
                               respAd2 = resposta_adicional2_FAT2AD2_DIC,
                               quali = c(eval(parse(text = input$quali_fator1_FAT2AD2_DIC)),eval(parse(text = input$quali_fator2_FAT2AD2_DIC))),
                               mcomp = input$mcomp_analise_FAT2AD2_DIC,
                               fac.names = c(input$fac.names_FAT2AD_DIC_f1,input$fac.names_FAT2AD2_DIC_f2),
                               sigT = input$sigT_analise_FAT2A2D_DIC,
                               sigF = input$sigF_analise_FAT2AD2_DIC,
                               unfold = switch(input$unfold_analise_FAT2AD2_DIC, "Nenhum"=NULL,input$unfold_analise_FAT2AD2_DIC)
                               )
                },
                "fat2.ad2.dbc" = {
                
                  fat2.ad2.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT2AD2_DBC,sep=""))),
                               fator2 = eval(parse(text = paste("dados$",input$fator2_FAT2AD2_DBC,sep=""))),
                               bloco = eval(parse(text = paste("dados$",input$bloco_FAT2AD2_DBC,sep=""))),
                               resp = eval(parse(text = paste("dados$",input$resp_FAT2AD2_DBC,sep=""))), 
                               respAd1 = resposta_adicional1_FAT2AD2_DBC,
                               respAd2 = resposta_adicional2_FAT2AD2_DBC,
                               quali = c(eval(parse(text = input$quali_fator1_FAT2AD2_DBC)),eval(parse(text = input$quali_fator2_FAT2AD2_DBC))),
                               mcomp = input$mcomp_analise_FAT2AD2_DBC,
                               fac.names = c(input$fac.names_FAT2AD2_DBC_f1,input$fac.names_FAT2AD2_DBC_f2),
                               sigT = input$sigT_analise_FAT2AD2_DBC,
                               sigF = input$sigF_analise_FAT2AD2_DBC,
                               unfold = switch(input$unfold_analise_FAT2AD2_DBC, "Nenhum"=NULL,input$unfold_analise_FAT2AD2_DBC)
                  )
                  
                },
                "fat3.ad.dic" = {
                  
                  fat3.ad.dic(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DIC,sep=""))),
                              fator2 = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DIC,sep=""))),
                              fator3 = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DIC,sep=""))),
                              repet = eval(parse(text = paste("dados$",input$repet_FAT3AD_DIC,sep=""))),
                              resp = eval(parse(text = paste("dados$",input$resp_FAT3AD_DIC,sep=""))), 
                              respAd = resposta_adicional_FAT3AD_DIC,
                              quali = c(eval(parse(text = input$quali_fator1_FAT3AD_DIC)),eval(parse(text = input$quali_fator2_FAT3AD_DIC)),eval(parse(text = input$quali_fator3_FAT3AD_DIC))),
                              mcomp = input$mcomp_analise_FAT3AD_DIC,
                              fac.names = c(input$fac.names_FAT3AD_DIC_f1,input$fac.names_FAT3AD_DIC_f2,input$fac.names_FAT3AD_DIC_f3),
                              sigT = input$sigT_analise_FAT3AD_DIC,
                              sigF = input$sigF_analise_FAT3AD_DIC,
                              unfold = switch(input$unfold_analise_FAT3AD_DIC, "Nenhum"=NULL,input$unfold_analise_FAT3AD_DIC)
                  )
                  
                },
                "fat3.ad.dbc" = {
                  
                  fat3.ad.dbc(fator1 = eval(parse(text = paste("dados$",input$fator1_FAT3AD_DBC,sep=""))),
                              fator2 = eval(parse(text = paste("dados$",input$fator2_FAT3AD_DBC,sep=""))),
                              fator3 = eval(parse(text = paste("dados$",input$fator3_FAT3AD_DBC,sep=""))),
                              bloco = eval(parse(text = paste("dados$",input$bloco_FAT3AD_DBC,sep=""))),
                              resp = eval(parse(text = paste("dados$",input$resp_FAT3AD_DBC,sep=""))), 
                              respAd = resposta_adicional_FAT3AD_DBC,
                              quali = c(eval(parse(text = input$quali_fator1_FAT3AD_DBC)),eval(parse(text = input$quali_fator2_FAT3AD_DBC)),eval(parse(text = input$quali_fator3_FAT3AD_DBC))),
                              mcomp = input$mcomp_analise_FAT3AD_DBC,
                              fac.names = c(input$fac.names_FAT3AD_DBC_f1,input$fac.names_FAT3AD_DBC_f2,input$fac.names_FAT3AD_DBC_f3),
                              sigT = input$sigT_analise_FAT3AD_DBC,
                              sigF = input$sigF_analise_FAT3AD_DBC,
                              unfold = switch(input$unfold_analise_FAT3AD_DBC, "Nenhum"=NULL,input$unfold_analise_FAT3AD_DBC)
                  )
                  
                }
        )
      )
      
      writeLines(saida,file)
      
    }
    
  )

}