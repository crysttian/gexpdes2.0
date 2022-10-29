#' @title Func\enc{çã}{ca}o Instalar
#' @name instalar
#'
#' @description Esta fun\enc{çã}{ca}o faz a instala\enc{çã}{ca}o das depend\enc{ê}{e}ncias dos pacotes para o GExpDes funcionar.
#'
#' @author Crystian Paix\enc{ã}{a}o
#'
#' @return Fun\enc{çã}{ca}o que instala as depend\enc{ê}{e}ncias dos pacotes para o GExpDes
#'
#' @importFrom utils install.packages
#'
#' @usage instalar()
#'
#' @export
instalar = function() {

  print("############################################")

  print("Preparando para realizar a instala\u00e7\u00e3o das depend\u00eancias do Pacote GExpDes.")

  print("Os pacotes a serem instalados s\u00e3o:")

  print("shiny")

  print("shinyalert")

  print("shinyjs")

  print("readxl")

  print("readODS")

  print("labestData")

  print("ExpDes.pt")

  print("plotly")

  print("agricolae")

  print("agricolaeplotr")

  print("Podemos seguir com a instala\u00e7\u00e3o? Em 10 segundos a instala\u00e7\u00e3o ser\u00e1 iniciada, caso n\u00e3o deseje d\u00ea o comando CONTROL+C.")

  for(i in 10:1){

    print(paste("A instala\u00e7\u00e3o come\u00e7a em", i, sep=" "))

    Sys.sleep(1)

  }

  print("Iniciando a instala\u00e7\u00e3o, isso pode demorar alguns minutos. Aguarde...")

  print("############################################")

  if (!requireNamespace(package = "shiny", quietly = TRUE)) {
    install.packages('shiny',dependencies = TRUE)
  }

  if (!requireNamespace(package = "shinyalert", quietly = TRUE)) {
    install.packages('shinyalert',dependencies = TRUE)
  }

  if (!requireNamespace(package = "shinyjs", quietly = TRUE)) {
    install.packages('shinyjs',dependencies = TRUE)
  }

  if (!requireNamespace(package = "readxl", quietly = TRUE)) {
    install.packages('readxl',dependencies = TRUE)
  }

  if (!requireNamespace(package = "readODS", quietly = TRUE)) {
    install.packages('readODS',dependencies = TRUE)
  }

  if (!requireNamespace(package = "ExpDes.pt", quietly = TRUE)) {
    install.packages('ExpDes.pt',dependencies = TRUE)
  }

  if (!requireNamespace(package = "plotly", quietly = TRUE)) {
    install.packages('plotly',dependencies = TRUE)
  }

  if (!requireNamespace(package = "agricolae", quietly = TRUE)) {
    install.packages('agricolae',dependencies = TRUE)
  }

  if (!requireNamespace(package = "agricolaeplotr", quietly = TRUE)) {
    install.packages('agricolaeplotr',dependencies = TRUE)
  }

  if (!requireNamespace(package = "labestData", quietly = TRUE)) {

    print('Detectando o sistema operacional para iniciar a instala\u00e7\u00e3o do labestData.')

    print(paste('O Sistema Operacinal dectado \u00e9',Sys.info()[['sysname']],sep=" "))

    if(Sys.info()[['sysname']]=="Windows"){

      install.packages("https://github.com/crysttian/gexpdes2.0/blob/main/pacotes/labestData_0.1.17.458.zip", repos = NULL)
    }else{
      install.packages("https://github.com/crysttian/gexpdes2.0/blob/main/pacotes/labestData_0.1-17.458.tar.gz", repos = NULL)
    }
  }

  #Verificando a instalacao

  print("############################################")

  print('Checando os pacotes instalados')

  print("############################################")

  if (!requireNamespace(package = "shiny", quietly = TRUE)) {
    stop("Pacote 'shiny' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("shiny...................ok")
  }

  if (!requireNamespace(package = "shinyalert", quietly = TRUE)) {
    stop("Pacote 'shinyalert' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("shinyalert .............ok")
  }

  if (!requireNamespace(package = "shinyjs", quietly = TRUE)) {
    stop("Pacote 'shinyjs' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("shinyjs ................ok")
  }

  if (!requireNamespace(package = "readxl", quietly = TRUE)) {
    stop("Pacote 'readxl' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("readxl .................ok")
  }

  if (!requireNamespace(package = "readODS", quietly = TRUE)) {
    stop("Pacote 'readODS' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("readODS ................ok")
  }

  if (!requireNamespace(package = "ExpDes.pt", quietly = TRUE)) {
    stop("Pacote 'ExpDes.pt' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("ExpDes.pt ..............ok")
  }

  if (!requireNamespace(package = "plotly", quietly = TRUE)) {
    stop("Pacote 'plotly' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("plotly .................ok")
  }

  if (!requireNamespace(package = "agricolae", quietly = TRUE)) {
    stop("Pacote 'agricolae' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("agricolae ..............ok")
  }

  if (!requireNamespace(package = "agricolaeplotr", quietly = TRUE)) {
    stop("Pacote 'agricolaeplotr' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("agricolaeplotr..........ok")
  }


  if (!requireNamespace(package = "labestData", quietly = TRUE)) {
    stop("Pacote 'labestData' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("labestData .............ok")
  }

  if (!requireNamespace(package = "gexpdes", quietly = TRUE)) {
    stop("Pacote 'gexpdes' n\u00e3o encontrado. Instale-o, por favor.", call. = FALSE)
  }else{
    print("gexpdes... .............ok")
  }

  print("############################################")

  print("Rode os comandos dentro do seu script do R para usar o GExpDes:")

  print("library(gexpdes")

  print("GExpDesView()")

  print("A Equipe GExpDes agradece o seu interesse em utilizar a nossa interface.")

}
