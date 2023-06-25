#Limpando o ambiente global
rm(list=ls())

#Pacotes utilizados
library(shiny)
library(shinyalert)
library(shinyjs)
library(readxl)
library(readODS)
library(labestData)
library(ExpDes.pt)
library(plotly)
library(agricolae)
library(agricolaeplotr)

#Variaveis globais
dados <<- NULL

# Definições da UI ----

ui <- fluidPage(

  shinyjs::useShinyjs(),

  titlePanel(div( style='text-align:center;', "GExpDes 2.0")),

  tabsetPanel(id="tabs",selected = "panelInicio",type = "hidden",

              tabPanel(title="Início",value = "panelInicio",

                       sidebarLayout(

                         sidebarPanel(title="",

                                      fluidRow(

                                        align = "center",

                                        verticalLayout(

                                          actionButton("ir_origem", "Iniciar a análise", icon= icon("flag-checkered"),class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_croqui", "Croqui Experimental", icon= icon("puzzle-piece"),class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_ajuda", "Ajuda", icon=icon("question-circle",verify_fa = FALSE), class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_site", "Site do GExpDes", icon=icon("globe",verify_fa = FALSE), class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_equipe", "Equipe do Projeto", icon=icon("people-group"), class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_fechar", "Fechar", icon=icon("power-off"), class="btn btn-primary", width="65%")

                                        ),

                                      ),

                                      br(),

                                      h4("Grupo do GExpDes no Telegran", align = "center"),

                                      div( img(src = "telegran.png", height = 200, width = 200), style="text-align: center;")

                         ),
                         mainPanel(

                           div( img(src = "logo_v2.png", height = 417, width = 820), style="text-align: center;")

                         )
                       )
              ),

              tabPanel(title="Importação dos Dados",value = "panelOrigem",

                       sidebarLayout(

                         sidebarPanel("",

                                      fluidRow(

                                        align = "left",

                                        radioButtons("radio_origem","O Conjunto de dados será importado via:", c("Importação de arquivo de dados" = "arquivo", "Dados da Biblioteca LabestData" = "labestdata"), inline= FALSE ),

                                      ),

                                      fluidRow(

                                        align = "center",

                                        verticalLayout(

                                          actionButton("ir_dados", "Importar", icon("angle-right"), class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_inicio_origem", "Início", icon= icon("angle-left"),class="btn btn-primary", width="65%")

                                        ),

                                      ),

                         ),
                         mainPanel(

                           includeHTML("html/conjunto.html")

                         )
                       )
              ),

              tabPanel(title="Dados",value = "panelDados",

                       sidebarLayout(

                         sidebarPanel(
                                      #Selecionando o conjunto de dados

                                      tabsetPanel(
                                        id = "tabs_importar",
                                        type = "hidden",
                                        tabPanel("Importar",value = "panel_importar",

                                                 fluidRow(

                                                   align = "left",

                                                   radioButtons("radio_importar","Importar arquivo de dados no formato:", c("CSV"="csv","Excel"="excel","OpenOffice ou LibreOffice"="ods"), inline = FALSE),

                                                 ),

                                                 tabsetPanel(
                                                   id = "tabs_importar_arquivo",
                                                   type = "hidden",
                                                   tabPanel("CVS",value = "panel_csv",

                                                            fileInput(inputId='arquivo_csv', label="Selecionar o arquivo",accept = ".csv"),

                                                            radioButtons("radio_arquivo_csv_separador","Separador de valores", c("Vírgula"=",","Ponto e vírgula"=";","Tabulação"="\t"), selected=";", inline= FALSE ),

                                                            radioButtons("radio_arquivo_csv_decimal","Separador decimal", c("Vírgula"=",","Ponto"="."),  selected=",", inline= FALSE )

                                                   ),
                                                   tabPanel("Excel",value = "panel_excel",

                                                            fileInput(inputId='arquivo_excel', label="Selecionar o arquivo",accept = c(".xls",".xlsx")),

                                                            selectInput(inputId="indice_excel", label="Índice da Planilha", choices=NULL)

                                                   ),

                                                   tabPanel("Open",value = "panel_ods",

                                                            fileInput(inputId='arquivo_ods', label="Selecionar o arquivo", accept = ".ods"),

                                                            selectInput(inputId="indice_ods", label="Índice da Planilha", choices=NULL)

                                                   ),

                                                 ),
                                        ),
                                        tabPanel("LabestData",value = "panel_labestdata",

                                                selectInput(inputId='arquivo_lab', label="Escolha a base de dados", choices=c("Seleciona um arquivo",ls("package:labestData")))

                                                )
                                      ),

                                      fluidRow(

                                        align = "center",

                                        verticalLayout(

                                          actionButton("ir_delineamento", "Delineamento", icon("angle-right"), class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_origem_dados", "Importação dos Dados", icon= icon("angle-left"),class="btn btn-primary", width="65%")

                                        ),

                                      ),

                         ),
                         mainPanel(
                           tabsetPanel(id="painel_dados",
                                       type='hidden',
                                       tabPanel("tab_arquivo", dataTableOutput('tabela')), #verbatimTextOutput('tabela2')
                                       tabPanel("tab_labestData",

                                                tabsetPanel(

                                                  tabPanel("Descrição",  htmlOutput("labestData_informacao")),

                                                  tabPanel("Dados", dataTableOutput('tabela_labestData')),

                                                )

                                       ),

                                       tabPanel("tab_gerar", tableOutput("table")),

                                       tabPanel("tab_ajuda", includeHTML("html/ajuda_importar.html"))

                           )
                         )
                       )
              ),

              tabPanel(title="Delineamento",value = "panelDelineamento",

                       sidebarLayout(

                         sidebarPanel("Delineamento",

                                      fluidRow(

                                        align = "center",

                                        verticalLayout(
                                          fluidRow(
                                            column(12,align="left",
                                              selectInput(inputId="tipo_delineamento",
                                                          label="Escolha o Delineamento",
                                                          choices=c("Delineamento Inteiramente Casualizado - DIC"="dic",
                                                                    "Delinamento em Blocos ao Acaso - DBC"="dbc",
                                                                    "Delineamento em Quadrado Latino - DQL"="dql",
                                                                    "Experimentos em faixas"="faixas",
                                                                    "Fatorial duplo em DIC"="fat2.dic",
                                                                    "Fatorial duplo em DBC"="fat2.dbc",
                                                                    "Fatorial triplo em DIC"="fat3.dic",
                                                                    "Fatorial triplo em DBC"="fat3.dbc",
                                                                    "Fatorial duplo com um tratamento adicional em DIC"="fat2.ad.dic",
                                                                    "Fatorial duplo com um tratamento adicional em DBC"="fat2.ad.dbc",
                                                                    "Fatorial duplo em DIC com dois tratamentos adicionais"="fat2.ad2.dic",
                                                                    "Fatorial duplo em DBC com dois tratamentos adicionais"="fat2.ad2.dbc",
                                                                    "Fatorial triplo com um tratamento adicional em DIC"="fat3.ad.dic",
                                                                    "Fatorial triplo com um tratamento adicional em DBC"="fat3.ad.dbc",
                                                                    "Parcelas subdivididas em DIC"="psub2.dic",
                                                                    "Parcelas subdivididas em DBC"="psub2.dbc"),
                                                          selected="dic"),
                                            )
                                          ),

                                          tabsetPanel(
                                            id="painel_opcao_delineamento",
                                            type="hidden",
                                            tabPanel("tabs_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                             selectInput(inputId = "trat_DIC",label="Tratamento",choices=""),
                                                             radioButtons(inputId = "quali_DIC",label="A variável Tratamento é",choices=c("Qualitativa"= TRUE ,"Quantitativa"= FALSE),selected=TRUE),
                                                             selectInput(inputId = "resp_DIC",label="Resposta","")
                                                      )
                                                     )
                                                    ),
                                            tabPanel("tabs_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "trat_DBC",label="Tratamento",choices=""),
                                                              radioButtons(inputId = "quali_DBC",label="A variável Tratamento é",choices=c("Qualitativa"= TRUE ,"Quantitativa"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "bloco_DBC",label="Bloco",choices=""),
                                                              selectInput(inputId = "resp_DBC",label="Resposta",choices="")
                                                       )
                                                     )
                                                    ),
                                            tabPanel("tabs_DQL",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "trat_DQL",label="Tratamento",choices=""),
                                                              radioButtons(inputId = "quali_DQL",label="A variável Tratamento é",choices=c("Qualitativa"= TRUE ,"Quantitativa"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "linha_DQL",label="Linha",choices=""),
                                                              selectInput(inputId = "coluna_DQL",label="Coluna",choices=""),
                                                              selectInput(inputId = "resp_DQL",label="Resposta",choices="")
                                                       )
                                                     )
                                                     ),
                                            tabPanel("tabs_Faixas",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_faixas",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_faixas",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_faixas",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_faixas",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "bloco_faixas",label="Bloco",choices=""),
                                                              selectInput(inputId = "resp_faixas",label="Resposta",choices="")
                                                       )
                                                     )
                                                    ),
                                            tabPanel("tabs_FAT2_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT2_DIC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT2_DIC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT2_DIC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT2_DIC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "resp_FAT2_DIC",label="Resposta",choices="")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_FAT2_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT2_DBC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT2_DBC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT2_DBC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT2_DBC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "bloco_FAT2_DBC",label="Bloco",choices=""),
                                                              selectInput(inputId = "resp_FAT2_DBC",label="Resposta",choices="")
                                                       )
                                                      )
                                                     ),
                                            tabPanel("tabs_FAT3_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT3_DIC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT3_DIC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT3_DIC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT3_DIC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator3_FAT3_DIC",label="Fator 3",choices=""),
                                                              radioButtons(inputId = "quali_fator3_FAT3_DIC",label="O Fator 3 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "resp_FAT3_DIC",label="Resposta",choices="")
                                                       )
                                                      )
                                                     ),
                                            tabPanel("tabs_FAT3_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT3_DBC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT3_DBC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT3_DBC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT3_DBC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator3_FAT3_DBC",label="Fator 3",choices=""),
                                                              radioButtons(inputId = "quali_fator3_FAT3_DBC",label="O Fator 3 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "bloco_FAT3_DBC",label="Bloco",choices=""),
                                                              selectInput(inputId = "resp_FAT3_DBC",label="Resposta",choices="")
                                                       )
                                                     )
                                                     ),
                                            tabPanel("tabs_FAT2AD_DIC",
                                                     fluidRow(
                                                      column(12,align="left",
                                                            selectInput(inputId = "fator1_FAT2AD_DIC",label="Fator 1",choices=""),
                                                            radioButtons(inputId = "quali_fator1_FAT2AD_DIC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                            selectInput(inputId = "fator2_FAT2AD_DIC",label="Fator 2",choices=""),
                                                            radioButtons(inputId = "quali_fator2_FAT2AD_DIC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                            selectInput(inputId = "repet_FAT2AD_DIC",label="Repetição",choices=""),
                                                            selectInput(inputId = "resp_FAT2AD_DIC",label="Resposta",choices=""),
                                                            #selectInput(inputId = "respAd_FAT2AD_DIC",label="Resposta do Tratamento Adicional",choices=""),
                                                            fileInput(inputId = "respAd_FAT2AD_DIC", label = "Importar arquivo com o Tratamento Adicional")
                                                      )
                                                    )
                                            ),
                                            tabPanel("tabs_FAT2AD_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT2AD_DBC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT2AD_DBC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT2AD_DBC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT2AD_DBC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "bloco_FAT2AD_DBC",label="Bloco",choices=""),
                                                              selectInput(inputId = "resp_FAT2AD_DBC",label="Resposta",choices=""),
                                                              #selectInput(inputId = "respAd_FAT2AD_DBC",label="Resposta do Tratamento Adicional",choices=""),
                                                              fileInput(inputId = "respAd_FAT2AD_DBC", label = "Importar arquivo com o Tratamento Adicional")
                                                       )
                                                      )
                                                     ),
                                            tabPanel("tabs_FAT2AD2_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT2AD2_DIC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT2AD2_DIC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT2AD2_DIC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT2AD2_DIC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "repet_FAT2AD2_DIC",label="Repetição",choices=""),
                                                              selectInput(inputId = "resp_FAT2AD2_DIC",label="Resposta",choices=""),
                                                              #selectInput(inputId = "respAd1_FAT2AD2_DIC",label="Resposta do Tratamento Adicional 1",choices=""),
                                                              fileInput(inputId = "respAd1_FAT2AD2_DIC", label = "Importar arquivo com o Tratamento Adicional 1"),
                                                              #selectInput(inputId = "respAd2_FAT2AD2_DIC",label="Resposta do Tratamento Adicional 2 ",choices="")
                                                              fileInput(inputId = "respAd2_FAT2AD2_DIC", label = "Importar arquivo com o Tratamento Adicional 2")
                                                       )
                                                     )
                                                    ),
                                            tabPanel("tabs_FAT2AD2_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT2AD2_DBC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT2AD2_DBC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT2AD2_DBC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT2AD2_DBC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "bloco_FAT2AD2_DBC",label="Bloco",choices=""),
                                                              selectInput(inputId = "resp_FAT2AD2_DBC",label="Resposta",choices=""),
                                                              fileInput(inputId = "respAd1_FAT2AD2_DBC", label = "Importar arquivo com o Tratamento Adicional 1"),
                                                              #selectInput(inputId = "respAd2_FAT2AD2_DBC",label="Resposta do Tratamento Adicional 2 ",choices=""),
                                                              fileInput(inputId = "respAd2_FAT2AD2_DBC", label = "Importar arquivo com o Tratamento Adicional 2"),
                                                       )
                                                     )
                                                   ),
                                            tabPanel("tabs_PSUB_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_PSUB_DIC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_PSUB_DIC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_PSUB_DIC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_PSUB_DIC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "repet_PSUB_DIC",label="Repetição",choices=""),
                                                              selectInput(inputId = "resp_PSUB_DIC",label="Resposta",choices="")
                                                       )
                                                     )
                                                    ),
                                            tabPanel("tabs_PSUB_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_PSUB_DBC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_PSUB_DBC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_PSUB_DBC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_PSUB_DBC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "bloco_PSUB_DBC",label="Bloco",choices=""),
                                                              selectInput(inputId = "resp_PSUB_DBC",label="Resposta",choices="")
                                                       )
                                                     )
                                                    ),
                                            tabPanel("tabs_FAT3AD_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT3AD_DIC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT3AD_DIC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT3AD_DIC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT3AD_DIC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator3_FAT3AD_DIC",label="Fator 3",choices=""),
                                                              radioButtons(inputId = "quali_fator3_FAT3AD_DIC",label="O Fator 3 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "repet_FAT3AD_DIC",label="Repetição",choices=""),
                                                              selectInput(inputId = "resp_FAT3AD_DIC",label="Resposta",choices=""),
                                                              fileInput(inputId = "respAd_FAT3AD_DIC", label = "Importar arquivo com o Tratamento Adicional")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_FAT3AD_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT3AD_DBC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT3AD_DBC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT3AD_DBC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT3AD_DBC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator3_FAT3AD_DBC",label="Fator 3",choices=""),
                                                              radioButtons(inputId = "quali_fator3_FAT3AD_DBC",label="O Fator 3 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "bloco_FAT3AD_DBC",label="Bloco",choices=""),
                                                              selectInput(inputId = "resp_FAT3AD_DBC",label="Resposta",choices=""),
                                                              fileInput(inputId = "respAd_FAT3AD_DBC", label = "Importar arquivo com o Tratamento Adicional")
                                                       )
                                                     )
                                            )

                                            ),

                                          br(),

                                          actionButton("ir_analise", "Análise", icon= icon("angle-right"),class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_dados_delineamento", "Dados", icon("angle-left"), class="btn btn-primary", width="65%")

                                        ),

                                      ),

                         ),
                         mainPanel(

                           h3("Estrutura da Base de Dados"),

                           verbatimTextOutput("tabela_str"),

                           h3("Análise Descritiva dos Dados"),

                           dataTableOutput('tabela_delineamento'),

                           hr(),

                           conditionalPanel(
                             condition = "(input.tipo_delineamento == 'fat2.ad.dic')||(input.tipo_delineamento == 'fat2.ad.dbc')",
                             h3("Análise Descritiva dos Dados do Tratamento Adicional"),
                             dataTableOutput('tabela_adicional_delineamento')
                           ),

                           conditionalPanel(
                             condition = "input.tipo_delineamento == 'fat2.ad2.dic'",
                             h3("Análise Descritiva dos Dados dos Tratamentos Adicionais"),
                             dataTableOutput('tabela_adicional1_delineamento_dic'),
                             hr(),
                             dataTableOutput('tabela_adicional2_delineamento_dic')
                           ),

                           conditionalPanel(
                             condition = "input.tipo_delineamento == 'fat2.ad2.dbc'",
                             h3("Análise Descritiva dos Dados dos Tratamentos Adicionais"),
                             dataTableOutput('tabela_adicional1_delineamento_dbc'),
                             hr(),
                             dataTableOutput('tabela_adicional2_delineamento_dbc')
                           ),

                           conditionalPanel(
                             condition = "input.tipo_delineamento == 'fat3.ad.dic'",
                             h3("Análise Descritiva dos Dados do Tratamentos Adicional"),
                             dataTableOutput('tabela_adicional_delineamento_dic_fat3')
                           ),

                           conditionalPanel(
                             condition = "input.tipo_delineamento == 'fat3.ad.dbc'",
                             h3("Análise Descritiva dos Dados dos Tratamentos Adicionais"),
                             dataTableOutput('tabela_adicional_delineamento_dbc_fat3')
                           )

                         )
                       )
              ),

              tabPanel(title="Análise",value = "panelAnalise",

                       sidebarLayout(

                         sidebarPanel(
                                      fluidRow(

                                        align = "center",

                                        verticalLayout(

                                          tabsetPanel(
                                            id="painel_opcao_delineamento_analise",
                                            type="hidden",
                                            tabPanel("tabs_analise_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                          selectInput(inputId = "mcomp_analise_DIC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                          selectInput(inputId = "hvar_analise_DIC",label="Teste de Homogeneidade",choices=c("Teste de Bartlett"="bartlett","Teste de Levene"="levene","Teste de Samiuddin"="samiuddin","Teste de O'Neill e Mathews"="oneillmathews","Teste de Layard"="layard"),selected = "bartlett"),
                                                          numericInput(inputId = "sigT_analise_DIC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                          numericInput(inputId = "sigF_analise_DIC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                          hr(),
                                                          selectInput(inputId = "nl_analise_DIC",label="Os  modelos não lineares devem ser ajustados aos fatores quantitativos:",c("Sim"=TRUE,"Não"=FALSE),selected=FALSE),
                                                          selectInput(inputId = "unfold_analise_DIC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_DBC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              selectInput(inputId = "hvar_analise_DBC",label="Teste de Homogeneidade",choices=c("Teste de O'Neill e Mathews"="oneillmathews","Teste de Han"="han","Teste de Ascombe e Tukey "="anscombetukey"),selected = "oneillmathews"),
                                                              numericInput(inputId = "sigT_analise_DBC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_DBC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              hr(),
                                                              selectInput(inputId = "nl_analise_DBC",label="Os  modelos não lineares devem ser ajustados aos fatores quantitativos:",c("Sim"=TRUE,"Não"=FALSE),selected=FALSE),
                                                              selectInput(inputId = "unfold_analise_DBC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_DQL",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_DQL",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_DQL",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_DQL",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_DQL",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_Faixas",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_faixas",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_faixas",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_faixas",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_faixas_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_faixas_f2", label="Nome do Fator 2",value="F2"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_faixas",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_FAT2_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_FAT2_DIC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_FAT2_DIC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_FAT2_DIC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_FAT2_DIC_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_FAT2_DIC_f2", label="Nome do Fator 2",value="F2"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_FAT2_DIC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_FAT2_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_FAT2_DBC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_FAT2_DBC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_FAT2_DBC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_FAT2_DBC_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_FAT2_DBC_f2", label="Nome do Fator 2",value="F2"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_FAT2_DBC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_FAT3_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_FAT3_DIC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_FAT3_DIC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_FAT3_DIC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_FAT3_DIC_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_FAT3_DIC_f2", label="Nome do Fator 2",value="F2"),
                                                              textInput(inputId = "fac.names_FAT3_DIC_f3", label="Nome do Fator 3",value="F3"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_FAT3_DIC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_FAT3_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_FAT3_DBC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_FAT3_DBC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_FAT3_DBC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_FAT3_DBC_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_FAT3_DBC_f2", label="Nome do Fator 2",value="F2"),
                                                              textInput(inputId = "fac.names_FAT3_DBC_f3", label="Nome do Fator 3",value="F3"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_FAT3_DBC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_FAT2AD_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_FAT2AD_DIC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_FAT2AD_DIC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_FAT2AD_DIC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_FAT2AD_DIC_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_FAT2AD_DIC_f2", label="Nome do Fator 2",value="F2"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_FAT2AD_DIC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_FAT2AD_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_FAT2AD_DBC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_FAT2AD_DBC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_FAT2AD_DBC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_FAT2AD_DBC_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_FAT2AD_DBC_f2", label="Nome do Fator 2",value="F2"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_FAT2AD_DBC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_FAT2AD2_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_FAT2AD2_DIC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_FAT2AD2_DIC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_FAT2AD2_DIC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_FAT2AD2_DIC_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_FAT2AD2_DIC_f2", label="Nome do Fator 2",value="F2"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_FAT2AD2_DIC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_FAT2AD2_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_FAT2AD2_DBC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_FAT2AD2_DBC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_FAT2AD2_DBC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_FAT2AD2_DBC_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_FAT2AD2_DBC_f2", label="Nome do Fator 2",value="F2"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_FAT2AD2_DBC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_PSUB_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_PSUB_DIC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_PSUB_DIC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_PSUB_DIC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_PSUB_DIC_f1", label="Nome do Fator 1 - Parcela",value="F1"),
                                                              textInput(inputId = "fac.names_PSUB_DIC_f2", label="Nome do Fator 2 - Subparcela",value="F2"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_PSUB_DIC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_PSUB_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_PSUB_DBC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_PSUB_DBC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_PSUB_DBC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_PSUB_DBC_f1", label="Nome do Fator 1 - Parcela",value="F1"),
                                                              textInput(inputId = "fac.names_PSUB_DBC_f2", label="Nome do Fator 2 - Subparcela",value="F2"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_PSUB_DBC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_analise_FAT3AD_DIC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_FAT3AD_DIC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_FAT3AD_DIC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_FAT3AD_DIC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_FAT3AD_DIC_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_FAT3AD_DIC_f2", label="Nome do Fator 2",value="F2"),
                                                              textInput(inputId = "fac.names_FAT3AD_DIC_f3", label="Nome do Fator 2",value="F3"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_FAT3AD_DIC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            ),

                                            tabPanel("tabs_analise_FAT3AD_DBC",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "mcomp_analise_FAT3AD_DBC",label="Teste de comparação múltipla",choices=c("Teste de Tukey"= "tukey", "Teste de LSD" = "lsd", "Teste de LSDB" = "lsdb", "Teste de Ducan"="ducan","Teste de SNK"="snk","Teste de Scott-Knott"="sk","Teste de comparações múltiplas bootstrap"="ccboot"," Teste de Calinski e Corsten"="ccf"),selected="tukey"),
                                                              numericInput(inputId = "sigT_analise_FAT3AD_DBC",label="Significância a ser adotada pelo teste de comparacao múltipla de médias",value=0.05,min=0,max=1,step=0.01),
                                                              numericInput(inputId = "sigF_analise_FAT3AD_DBC",label="Significância a ser adotada pelo teste F da ANAVA",value=0.05,min=0,max=1,step=0.01),
                                                              textInput(inputId = "fac.names_FAT3AD_DBC_f1", label="Nome do Fator 1",value="F1"),
                                                              textInput(inputId = "fac.names_FAT3AD_DBC_f2", label="Nome do Fator 2",value="F2"),
                                                              textInput(inputId = "fac.names_FAT3AD_DBC_f3", label="Nome do Fator 2",value="F3"),
                                                              hr(),
                                                              selectInput(inputId = "unfold_analise_FAT3AD_DBC",label="Orientação dos desdobramentos",choices=c("Análises Recomendadas"="Nenhum","Somente Análise de Variância"=0,"Efeitos Simples"=1),selected = "Nenhum")
                                                       )
                                                     )
                                            )

                                          ),

                                          actionButton("ir_delineamento_analise", "Delineamento", icon("angle-left"), class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_inicio_analise", "Nova Análise", icon= icon("question-circle",verify_fa = FALSE),class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_fechar_fim", "Fechar", icon= icon("power-off"),class="btn btn-primary", width="65%")

                                        ),

                                      ),

                         ),

                         mainPanel("",
                                   tabsetPanel(id="tabs_analise",
                                     tabPanel("Análise Preliminar",

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'dic')",
                                         plotly::plotlyOutput("analise_boxplot_dic")
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'dbc')",
                                         plotly::plotlyOutput("analise_boxplot_dbc"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_bloco_dbc")
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'dql')",
                                         plotly::plotlyOutput("analise_boxplot_dql"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_linha_dql"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_coluna_dql")
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'faixas')",
                                         plotly::plotlyOutput("analise_boxplot_faixas_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_faixas_fator2"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_faixas_bloco")
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'fat2.dic')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_FAT2_DIC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT2_DIC_fator2"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_FAT2_DIC_fator1"),
                                         plotOutput("analise_interacao_FAT2_DIC_fator2"),
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'fat2.dbc')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_FAT2_DBC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT2_DBC_fator2"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT2_DBC_bloco"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_FAT2_DBC_fator1"),
                                         plotOutput("analise_interacao_FAT2_DBC_fator2")
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'fat3.dic')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_FAT3_DIC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT3_DIC_fator2"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT3_DIC_fator3"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_FAT3_DIC_fator1_fator2"),
                                         plotOutput("analise_interacao_FAT3_DIC_fator1_fator3"),
                                         plotOutput("analise_interacao_FAT3_DIC_fator2_fator1"),
                                         plotOutput("analise_interacao_FAT3_DIC_fator2_fator3"),
                                         plotOutput("analise_interacao_FAT3_DIC_fator3_fator1"),
                                         plotOutput("analise_interacao_FAT3_DIC_fator3_fator2")
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'fat3.dbc')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_FAT3_DBC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT3_DBC_fator2"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT3_DBC_fator3"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT3_DBC_bloco"),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_FAT3_DBC_fator1_fator2"),
                                         plotOutput("analise_interacao_FAT3_DBC_fator1_fator3"),
                                         plotOutput("analise_interacao_FAT3_DBC_fator2_fator1"),
                                         plotOutput("analise_interacao_FAT3_DBC_fator2_fator3"),
                                         plotOutput("analise_interacao_FAT3_DBC_fator3_fator1"),
                                         plotOutput("analise_interacao_FAT3_DBC_fator3_fator2")
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'psub2.dic')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_PSUB_DIC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_PSUB_DIC_fator2"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_PSUB_DIC_fator1"),
                                         plotOutput("analise_interacao_PSUB_DIC_fator2"),
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'psub2.dbc')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_PSUB_DBC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_PSUB_DBC_fator2"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_PSUB_DBC_bloco"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_PSUB_DBC_fator1"),
                                         plotOutput("analise_interacao_PSUB_DBC_fator2"),
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'fat2.ad.dic')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_FAT2AD_DIC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT2AD_DIC_fator2"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_FAT2AD_DIC_fator1"),
                                         plotOutput("analise_interacao_FAT2AD_DIC_fator2"),
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'fat2.ad.dbc')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_FAT2AD_DBC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT2AD_DBC_fator2"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT2AD_DBC_bloco"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_FAT2AD_DBC_fator1"),
                                         plotOutput("analise_interacao_FAT2AD_DBC_fator2")
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'fat2.ad2.dic')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_FAT2AD2_DIC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT2AD2_DIC_fator2"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_FAT2AD2_DIC_fator1"),
                                         plotOutput("analise_interacao_FAT2AD2_DIC_fator2"),
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'fat2.ad2.dbc')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_FAT2AD2_DBC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT2AD2_DBC_fator2"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT2AD2_DBC_bloco"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_FAT2AD2_DBC_fator1"),
                                         plotOutput("analise_interacao_FAT2AD2_DBC_fator2"),
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'fat3.ad.dic')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_FAT3AD_DIC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT3AD_DIC_fator2"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT3AD_DIC_fator3"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_FAT3AD_DIC_fator1_fator2"),
                                         plotOutput("analise_interacao_FAT3AD_DIC_fator1_fator3"),
                                         plotOutput("analise_interacao_FAT3AD_DIC_fator2_fator1"),
                                         plotOutput("analise_interacao_FAT3AD_DIC_fator2_fator3"),
                                         plotOutput("analise_interacao_FAT3AD_DIC_fator3_fator1"),
                                         plotOutput("analise_interacao_FAT3AD_DIC_fator3_fator2")
                                       ),

                                       conditionalPanel(
                                         condition = "(input.tipo_delineamento == 'fat3.ad.dbc')",
                                         h3("BoxPlot dos Fatores e Variável Resposta"),
                                         plotly::plotlyOutput("analise_boxplot_FAT3AD_DBC_fator1"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT3AD_DBC_fator2"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT3AD_DBC_fator3"),
                                         hr(),
                                         plotly::plotlyOutput("analise_boxplot_FAT3AD_DBC_bloco"),
                                         hr(),
                                         h3("Interações dos Fatores"),
                                         plotOutput("analise_interacao_FAT3AD_DBC_fator1_fator2"),
                                         plotOutput("analise_interacao_FAT3AD_DBC_fator1_fator3"),
                                         plotOutput("analise_interacao_FAT3AD_DBC_fator2_fator1"),
                                         plotOutput("analise_interacao_FAT3AD_DBC_fator2_fator3"),
                                         plotOutput("analise_interacao_FAT3AD_DBC_fator3_fator1"),
                                         plotOutput("analise_interacao_FAT3AD_DBC_fator3_fator2")
                                       )

                                     ),
                                     tabPanel("Análise de Resíduo",plotOutput("analise_residuo")),
                                     tabPanel("Análise de Variância",
                                              verbatimTextOutput("saida_analise"),

                                              br(),

                                              downloadButton("relatorio","Salvar análise")

                                              ),
                                     tabPanel("Análise de Regressão",

                                              conditionalPanel(
                                                condition = "input.tipo_delineamento == 'dic' && input.quali_DIC == 'FALSE'",
                                                h3("Modelo Linear"),
                                                plotOutput("analise_regressao_dic_graf1"),
                                                h3("Modelo Quadrático"),
                                                plotOutput("analise_regressao_dic_graf2"),
                                                h3("Modelo Cúbico"),
                                                plotOutput("analise_regressao_dic_graf3")
                                                ),

                                              conditionalPanel(
                                                condition = "input.tipo_delineamento == 'dbc' && input.quali_DBC == 'FALSE'",
                                                h3("Modelo Linear"),
                                                plotOutput("analise_regressao_dbc_graf1"),
                                                h3("Modelo Quadrático"),
                                                plotOutput("analise_regressao_dbc_graf2"),
                                                h3("Modelo Cúbico"),
                                                plotOutput("analise_regressao_dbc_graf3")
                                              ),

                                              conditionalPanel(
                                                condition = "input.tipo_delineamento == 'dql' && input.quali_DQL == 'FALSE'",
                                                h3("Modelo Linear"),
                                                plotOutput("analise_regressao_dql_graf1"),
                                                h3("Modelo Quadrático"),
                                                plotOutput("analise_regressao_dql_graf2"),
                                                h3("Modelo Cúbico"),
                                                plotOutput("analise_regressao_dql_graf3")
                                              )

                                            )


                                  )
                                )
                       )
              ),

              tabPanel(title="Ajuda",value = "panelAjuda",

                       sidebarLayout(

                         sidebarPanel("",

                                      fluidRow(

                                        align = "center",

                                        verticalLayout(

                                          actionButton("ir_inicio_ajuda", "Início", icon= icon("angle-left"),class="btn btn-primary", width="65%")

                                        ),

                                      ),

                         ),

                         mainPanel("",

                                   includeHTML("html/ajuda_gexpdes.html")

                                   )

                       )
              ),

              tabPanel(title="Croqui",value = "panelCroqui",

                       sidebarLayout(

                         sidebarPanel(
                                      fluidRow(

                                        align = "center",

                                        verticalLayout(
                                          fluidRow(
                                            column(12,align="left",
                                                   selectInput(inputId="tipo_delineamento_croqui",
                                                               label="Escolha o Delineamento",
                                                               choices=c("Delineamento Inteiramente Casualizado - DIC"="dic",
                                                                         "Delinamento em Blocos ao Acaso - DBC"="dbc",
                                                                         "Delineamento em Quadrado Latino - DQL"="dql",
                                                                         "Experimentos em faixas"="faixas",
                                                                         "Fatorial duplo em DIC"="fat2.dic",
                                                                         "Fatorial duplo em DBC"="fat2.dbc",
                                                                         "Fatorial triplo em DIC"="fat3.dic",
                                                                         "Fatorial triplo em DBC"="fat3.dbc",
                                                                         "Parcelas subdivididas em DIC"="psub2.dic",
                                                                         "Parcelas subdivididas em DBC"="psub2.dbc"),
                                                               selected="dic"),
                                            )
                                          ),

                                          tabsetPanel(
                                            id="painel_opcao_delineamento_croqui",
                                            type="hidden",
                                            tabPanel("tabs_DIC_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              numericInput(inputId = "croqui_n_trat", label="Número de Tratamentos", value=2,min=2, max=50, step=1),
                                                              radioButtons(inputId = "croqui_numeracao_trat",label="Númeração dos Tratamentos",choices=c("LETRAS MAIÚSCULAS (A, B, C,...)"= 'maiuscula' ,"letras minúculas"= 'minuscula', "Algarismos arábicos" = "arabico", "Algarismos romanos" = "romano"),selected='maiuscula'),
                                                              textInput(inputId="croqui_prefixo_trat", label="Prefixo do nome dos Tratamentos", value=""),
                                                              numericInput(inputId = "croqui_rep_trat", label="Número de Repetições", value=2,min=2, max=50, step=1),
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_DBC_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              numericInput(inputId = "croqui_n_trat_DBC", label="Número de Tratamentos", value=2,min=2, max=50, step=1),
                                                              radioButtons(inputId = "croqui_numeracao_trat_DBC",label="Númeração dos Tratamentos",choices=c("LETRAS MAIÚSCULAS (A, B, C,...)"= 'maiuscula' ,"letras minúculas"= 'minuscula', "Algarismos arábicos" = "arabico", "Algarismos romanos" = "romano"),selected='maiuscula'),
                                                              textInput(inputId="croqui_prefixo_trat_DBC", label="Prefixo do nome dos Tratamentos", value=""),
                                                              numericInput(inputId = "croqui_bloco_DBC", label="Número de Blocos", value=2,min=2, max=50, step=1)
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_DQL_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              numericInput(inputId = "croqui_n_trat_DQL", label="Número de Tratamentos", value=2,min=2, max=50, step=1),
                                                              radioButtons(inputId = "croqui_numeracao_trat_DQL",label="Númeração dos Tratamentos",choices=c("LETRAS MAIÚSCULAS (A, B, C,...)"= 'maiuscula' ,"letras minúculas"= 'minuscula', "Algarismos arábicos" = "arabico", "Algarismos romanos" = "romano"),selected='maiuscula'),
                                                              textInput(inputId="croqui_prefixo_trat_DQL", label="Prefixo do nome dos Tratamentos", value=""),
                                                              )
                                                     )
                                            ),
                                            tabPanel("tabs_Faixas_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              numericInput(inputId = "croqui_n_trat1_FAIXAS", label="Número de níveis do Fator 1", value=2,min=2, max=50, step=1),
                                                              radioButtons(inputId = "croqui_numeracao_trat1_FAIXAS",label="Numeração do Fator 1",choices=c("LETRAS MAIÚSCULAS (A, B, C,...)"= 'maiuscula' ,"letras minúculas"= 'minuscula', "Algarismos arábicos" = "arabico", "Algarismos romanos" = "romano"),selected='maiuscula'),
                                                              textInput(inputId="croqui_prefixo_trat1_FAIXAS", label="Prefixo dos níveis do Fator 1", value=""),
                                                              numericInput(inputId = "croqui_n_trat2_FAIXAS", label="Número de níveis do Fator 2", value=2,min=2, max=50, step=1),
                                                              radioButtons(inputId = "croqui_numeracao_trat2_FAIXAS",label="Numeração do Fator 2",choices=c("LETRAS MAIÚSCULAS (A, B, C,...)"= 'maiuscula' ,"letras minúculas"= 'minuscula', "Algarismos arábicos" = "arabico", "Algarismos romanos" = "romano"),selected='maiuscula'),
                                                              textInput(inputId="croqui_prefixo_trat2_FAIXAS", label="Prefixo dos níveis do Fator 2", value=""),
                                                              numericInput(inputId = "croqui_blocos_FAIXAS", label="Número de Blocos", value=2,min=2, max=50, step=1),
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_FAT2_DIC_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              numericInput(inputId = "croqui_n_fator1_FAT2_DIC", label="Número de níveis do Fator A", value=2,min=2, max=50, step=1),
                                                              numericInput(inputId = "croqui_n_fator2_FAT2_DIC", label="Número de níveis do Fator B", value=2,min=2, max=50, step=1),
                                                              numericInput(inputId = "croqui_rep_fator_FAT2_DIC", label="Número de Repetições", value=2,min=2, max=50, step=1),
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_FAT2_DBC_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              numericInput(inputId = "croqui_n_fator1_FAT2_DBC", label="Número de níveis do Fator A", value=2,min=2, max=50, step=1),
                                                              numericInput(inputId = "croqui_n_fator2_FAT2_DBC", label="Número de níveis do Fator B", value=2,min=2, max=50, step=1),
                                                              numericInput(inputId = "croqui_bloco_fator_FAT2_DBC", label="Número de Blocos", value=2,min=2, max=50, step=1),
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_FAT3_DIC_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              numericInput(inputId = "croqui_n_fator1_FAT3_DIC", label="Número de níveis do Fator A", value=2,min=2, max=50, step=1),
                                                              numericInput(inputId = "croqui_n_fator2_FAT3_DIC", label="Número de níveis do Fator B", value=2,min=2, max=50, step=1),
                                                              numericInput(inputId = "croqui_n_fator3_FAT3_DIC", label="Número de níveis do Fator C", value=2,min=2, max=50, step=1),
                                                              numericInput(inputId = "croqui_rep_fator_FAT3_DIC", label="Número de Repetições", value=2,min=2, max=50, step=1),
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_FAT3_DBC_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              numericInput(inputId = "croqui_n_fator1_FAT3_DBC", label="Número de níveis do Fator A", value=2,min=2, max=50, step=1),
                                                              numericInput(inputId = "croqui_n_fator2_FAT3_DBC", label="Número de níveis do Fator B", value=2,min=2, max=50, step=1),
                                                              numericInput(inputId = "croqui_n_fator3_FAT3_DBC", label="Número de níveis do Fator C", value=2,min=2, max=50, step=1),
                                                              numericInput(inputId = "croqui_bloco_fator_FAT3_DBC", label="Número de Blocos", value=2,min=2, max=50, step=1),
                                                       )
                                                     )
                                            ),

                                            tabPanel("tabs_PSUB_DIC_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              numericInput(inputId = "croqui_n_trat1_PSUB_DIC", label="Número de níveis do Fator da Parcela", value=2,min=2, max=50, step=1),
                                                              radioButtons(inputId = "croqui_numeracao_trat1_PSUB_DIC",label="Numeração do Fator da Parcela",choices=c("LETRAS MAIÚSCULAS (A, B, C,...)"= 'maiuscula' ,"letras minúculas"= 'minuscula', "Algarismos arábicos" = "arabico", "Algarismos romanos" = "romano"),selected='maiuscula'),
                                                              textInput(inputId="croqui_prefixo_trat1_PSUB_DIC", label="Prefixo dos níveis da Parcela", value=""),
                                                              numericInput(inputId = "croqui_n_trat2_PSUB_DIC", label="Número de níveis da SubParcela", value=2,min=2, max=50, step=1),
                                                              radioButtons(inputId = "croqui_numeracao_trat2_PSUB_DIC",label="Numeração da SubParcela",choices=c("LETRAS MAIÚSCULAS (A, B, C,...)"= 'maiuscula' ,"letras minúculas"= 'minuscula', "Algarismos arábicos" = "arabico", "Algarismos romanos" = "romano"),selected='maiuscula'),
                                                              textInput(inputId="croqui_prefixo_trat2_PSUB_DIC", label="Prefixo dos níveis da SubParcela", value=""),
                                                              numericInput(inputId = "croqui_rep_PSUB_DIC", label="Número de Repetições", value=2,min=2, max=50, step=1),
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_PSUB_DBC_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              numericInput(inputId = "croqui_n_trat1_PSUB_DBC", label="Número de níveis do Fator da Parcela", value=2,min=2, max=50, step=1),
                                                              radioButtons(inputId = "croqui_numeracao_trat1_PSUB_DBC",label="Numeração do Fator da Parcela",choices=c("LETRAS MAIÚSCULAS (A, B, C,...)"= 'maiuscula' ,"letras minúculas"= 'minuscula', "Algarismos arábicos" = "arabico", "Algarismos romanos" = "romano"),selected='maiuscula'),
                                                              textInput(inputId="croqui_prefixo_trat1_PSUB_DBC", label="Prefixo dos níveis da Parcela", value=""),
                                                              numericInput(inputId = "croqui_n_trat2_PSUB_DBC", label="Número de níveis da SubParcela", value=2,min=2, max=50, step=1),
                                                              radioButtons(inputId = "croqui_numeracao_trat2_PSUB_DBC",label="Numeração da SubParcela",choices=c("LETRAS MAIÚSCULAS (A, B, C,...)"= 'maiuscula' ,"letras minúculas"= 'minuscula', "Algarismos arábicos" = "arabico", "Algarismos romanos" = "romano"),selected='maiuscula'),
                                                              textInput(inputId="croqui_prefixo_trat2_PSUB_DBC", label="Prefixo dos níveis da SubParcela", value=""),
                                                              numericInput(inputId = "croqui_bloco_PSUB_DBC", label="Número de Blocos", value=2,min=2, max=50, step=1),
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_FAT3AD_DIC_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT3AD_DIC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT3AD_DIC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT3AD_DIC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT3AD_DIC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator3_FAT3AD_DIC",label="Fator 3",choices=""),
                                                              radioButtons(inputId = "quali_fator3_FAT3AD_DIC",label="O Fator 3 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "repet_FAT3AD_DIC",label="Repetição",choices=""),
                                                              selectInput(inputId = "resp_FAT3AD_DIC",label="Resposta",choices=""),
                                                              fileInput(inputId = "respAd_FAT3AD_DIC", label = "Importar arquivo com o Tratamento Adicional")
                                                       )
                                                     )
                                            ),
                                            tabPanel("tabs_FAT3AD_DBC_croqui",
                                                     fluidRow(
                                                       column(12,align="left",
                                                              selectInput(inputId = "fator1_FAT3AD_DBC",label="Fator 1",choices=""),
                                                              radioButtons(inputId = "quali_fator1_FAT3AD_DBC",label="O Fator 1 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator2_FAT3AD_DBC",label="Fator 2",choices=""),
                                                              radioButtons(inputId = "quali_fator2_FAT3AD_DBC",label="O Fator 2 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "fator3_FAT3AD_DBC",label="Fator 3",choices=""),
                                                              radioButtons(inputId = "quali_fator3_FAT3AD_DBC",label="O Fator 3 é",choices=c("Qualitativo"= TRUE ,"Quantitativo"= FALSE),selected=TRUE),
                                                              selectInput(inputId = "bloco_FAT3AD_DBC",label="Bloco",choices=""),
                                                              selectInput(inputId = "resp_FAT3AD_DBC",label="Resposta",choices=""),
                                                              fileInput(inputId = "respAd_FAT3AD_DBC", label = "Importar arquivo com o Tratamento Adicional")
                                                       )
                                                     )
                                            )

                                          ),

                                          actionButton("gerar_croqui", "Gerar Croqui", icon= icon("gears", verify_fa = FALSE),class="btn btn-primary", width="65%"),

                                          br(),

                                          actionButton("ir_inicio_croqui", "Início", icon= icon("angle-left"),class="btn btn-primary", width="65%"),

                                        ),

                                    ),

                         ),

                         mainPanel(
                                   tabsetPanel(
                                     tabPanel("Croqui",verbatimTextOutput("saida_croqui")),
                                     tabPanel("Mapa Experimental",

                                              br(),

                                              plotOutput("grafico_croqui"))
                                   )
                                  )

                       )
              ),

              tabPanel(title="Equipe",value = "panelEquipe",

                       sidebarLayout(

                         sidebarPanel(

                                      fluidRow(

                                        align = "center",

                                        verticalLayout(

                                          actionButton("ir_inicio_equipe", "Início", icon= icon("angle-left"),class="btn btn-primary", width="65%")

                                        ),

                                      ),

                         ),

                         mainPanel(

                           h2("Equipe Desenvolvedora do GExpDes 2.0", align = "center"),

                           h3("Universidade Federal da Bahia", align = "center"),

                           h4("Grupo de Pesquisa Interdisciplinar em Inovação, Tecnologia e Empreendedorismo", align = "center"),

                           h5("Crysttian Arantes Paixão - crysttianpaixao@ufba.br", align = "center"),

                           h5("Aretha da Silva dos Santos", align = "center"),

                           h3("Universidade Federal de Santa Catarina", align = "center"),

                           h5("Rogerio Kormann - rogerio.kormann@ufsc.br", align = "center"),

                           h5("Eduardo Nunes Rosa - eduardo.nunes@grad.ufsc.br", align = "center"),

                           h3("Universidade Federal de Alfenas", align = "center"),

                           h4("Grupo de Pesquisa Modelos Matemmáticos e Estatísticos Aplicados à Ciências Experimentais", align = "center"),

                           h5("Eric Batista Ferreira - eric.ferreira@unifal-mg.edu.br", align = "center"),

                           h5("Denismar Alves Nogueira - denismar.nogueira@unifal-mg.edu.br", align = "center")

                         )

                       )
              )

  )
)
