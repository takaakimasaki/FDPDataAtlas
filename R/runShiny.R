#' A wrapper function to run Shiny Apps from \code{FDPDataAtlas}.
#' 
#' Running this function will launch the FDPDataAtlas shiny
#' @return FDPDataAtlas shiny app
#' @param app FDPDataAtlas 
#' @import shiny
#' @import shinyBS
#' @import RColorBrewer
#' @import htmltools
#' @import htmlwidgets
#' @import leaflet.providers
#' @import mapview
#' @import shinyWidgets
#' @import shinydashboard
#' @import webshot
#' 
#' @export

runShiny <- function(app="FDPDataAtlas"){
  
  # find and launch the app
  appDir <- system.file("shiny-examples", app, package = "FDPDataAtlas")
  
  shiny::runApp(appDir, display.mode = "normal")
}