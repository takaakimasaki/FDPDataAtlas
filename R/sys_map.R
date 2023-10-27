#' Map the studies included in a systematic review.
#' 
#' Created For	  : ES Hackathon 2018
#' @param studies_data Input dataframe
#' @import leaflet
#' @keywords SystematicReview
#' @export


sys_map <- function(studies_data) {

  basemap <- leaflet::leaflet(studies_data,
                              options = leafletOptions(minZoom = 2)) %>%
    leaflet::addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/gsdpm/civtteddj000z2jodf6dv7vw4/tiles/256/{z}/{x}}/{y}@2x?access_token=pk.eyJ1IjoiZ3NkcG0iLCJhIjoiY2toZjFvZ3gwMG1qODJ4cnpwaDdvenpzMiJ9.01pv2kccL9cXhxO6B-Naiw")

  basemap
}
