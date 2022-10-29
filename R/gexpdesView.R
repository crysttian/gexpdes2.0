#' @title Func\enc{çã}{ca}o gexpdesView
#' @name gexpdesView
#'
#' @description Esta fun\enc{çã}{ca}o exibe a insterface do GExpDes.
#'
#' @author Crystian Paix\enc{ã}{a}o
#'
#' @return Fun\enc{çã}{ca}o que abre a interface do GExpDes
#'
#' @usage gexpdesView()
#'
#' @export
gexpdesView = function() {
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

  appDir <- system.file("ShinyApps","gexpdes",package = "gexpdes")

  print(appDir)

  if (appDir == "") {
    stop("N\u00e3o foi poss\u00edvel encontrar o diret\u00f3rio com o GExpDes, ocorreu algum erro na instala\u00e7\u00e3o. Por favor, instale-o.",call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
