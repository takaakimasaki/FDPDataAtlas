#' Plot systematic map from shapefile inputs
#'
#' Created For ES Hackathon 2018
#' @param shp shapefile input in to FDPDataAtlas
#' @param popups Column with popup information
#' @import leaflet
#' @import leafem
#' @keywords SystematicReview
#'
#' @export


sys_map_shapefile <- function(shp, popups = "") {
  
  leaflet(shp) %>%
    leaflet::addProviderTiles(providers$Esri.WorldTerrain) %>%
    leafem::addFeatures(data = shp, 
                group = 'atlas_shapefile', 
                fillColor = "green",
                fillOpacity = 0.5,
                color = "black",
                weight = 2,
                popup = ~popups)
  
}
