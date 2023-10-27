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
    addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/gsdpm/civtteddj000z2jodf6dv7vw4/tiles/256/{z}/{x}}/{y}@2x?access_token=pk.eyJ1IjoiZ3NkcG0iLCJhIjoiY2toZjFvZ3gwMG1qODJ4cnpwaDdvenpzMiJ9.01pv2kccL9cXhxO6B-Naiw") %>% 
    leafem::addFeatures(data = shp, 
                group = 'atlas_shapefile', 
                fillColor = "green",
                fillOpacity = 0.5,
                color = "black",
                weight = 2,
                popup = ~popups)
  
}
