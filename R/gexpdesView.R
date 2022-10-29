#' @title Func\enc{çã}{ca}o gexpdesView
#' @name gexpdesView
#'
#' @description Esta fun\enc{çã}{ca}o exibe a insterface do GExpDes.
#'
#' @author Crystian Paix\enc{ã}{a}o
#'
#' @return Fun\enc{çã}{ca}o que abre a interface do GExpDes
#'
#' @importFrom
#'
#' @usage gexpdesView()
#'
#' @export
GExpDesView = function() {
  if (!requireNamespace(package = "shiny", quietly = TRUE)) {
    stop("Pacote 'shiny' não encontrado. Instale-o, por favor.", call. = FALSE)
  }

  if (!requireNamespace(package = "shinyalert", quietly = TRUE)) {
    stop("Pacote 'shinyalert' não encontrado. Instale-o, por favor.", call. = FALSE)
  }

  if (!requireNamespace(package = "readxl", quietly = TRUE)) {
    stop("Pacote 'readxl' não encontrado. Instale-o, por favor.", call. = FALSE)
  }

  if (!requireNamespace(package = "readODS", quietly = TRUE)) {
    stop("Pacote 'readODS' não encontrado. Instale-o, por favor.", call. = FALSE)
  }

  if (!requireNamespace(package = "ExpDes.pt", quietly = TRUE)) {
    stop("Pacote 'shiny' não encontrado. Instale-o, por favor.", call. = FALSE)
  }

  if (!requireNamespace(package = "plotly", quietly = TRUE)) {
    stop("Pacote 'plotly' não encontrado. Instale-o, por favor.", call. = FALSE)
  }

  if (!requireNamespace(package = "agricolae", quietly = TRUE)) {
    stop("Pacote 'agricolae' não encontrado. Instale-o, por favor.", call. = FALSE)
  }

  if (!requireNamespace(package = "agricolaeplotr", quietly = TRUE)) {
    stop("Pacote 'agricolaeplotr' não encontrado. Instale-o, por favor.", call. = FALSE)
  }

  if (!requireNamespace(package = "labestData", quietly = TRUE)) {

    print('Detectando o sistema operacional para iniciar a instalação do labestData.')

    print(paste('O Sistema Operacinal dectado é',Sys.info()[['sysname']],sep=" "))

    if(Sys.info()[['sysname']]=="Windows"){
      install.packages("labestData_0.1.17.458.zip", repos = NULL)
    }else{
      install.packages("labestData_0.1-17.458.tar.gz", repos = NULL)
    }
  }

  if (!requireNamespace(package = "labestData", quietly = TRUE)) {

    stop("Pacote 'labestData' não encontrado. Instale-o, por favor.", call. = FALSE)

  }

  appDir <- system.file("ShinyApps","gexpdes",package = "gexpdes")

  print(appDir)

  if (appDir == "") {
    stop("Não foi possível encontrar o diretório com o GExpDes, ocorreu algum erro na instalação. Por favor, instale-o.",call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
