library(FDPDataAtlas)
library(dplyr)
library(stringr)
library(ggplot2)
library(unhcrthemes)
library(tidyr)
library(readr)
library(DT)
library(plotly)
library(leaflet)
library(leaflet.providers)
library(htmltools)
library(htmlwidgets)
library(mapview)
library(leafem)
library(sf)
library(viridis)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(RColorBrewer)

if (webshot::is_phantomjs_installed() == FALSE) {
 webshot::install_phantomjs()
}

# functions
source("gendescplot.R")
source("genheatmap.R")

get_cols_for_plot <- function(df) {
  list_cols <- colnames(df %>%
                          dplyr::select_if(function(x)
                            dplyr::n_distinct(x) < 45))
  
  cols_to_remove <-
    c(
      "method_data_collectors_name",
      "entity_contact" ,
      "disclaimer",
      "project_specific",
      "analysis_unit",
      "total_of_country",
      "idp",
      "Longitude",
      "Latitude"
    )
  
  final_list <- setdiff(list_cols, cols_to_remove)
  
  return(final_list)
  
}

# general config 
start_text <- read_file("www/AboutEvi.html")
max_file_size_mb <- 100
options(shiny.maxRequestSize = max_file_size_mb * 1024 ^ 2)
shinyServer(function(input, output, session) {
 data_internal <- reactiveValues(raw = NULL,
                                 cols = NULL,
                                 short_cols = NULL)
# Data tab
 data_internal$raw <- FDPDataAtlas::metadata %>% as.data.frame()
 
 output$start_text <- renderPrint({
   cat(start_text)
 })
 
# Generate dataframe of surveys per country
 data_active <- reactive({
   req(data_internal$raw)
   
   d_out <- data_internal$raw
   
   # Group by 'nation_abbreviation' and count the number of records for each country
   country_count <- d_out %>%
     group_by(nation_abbreviation) %>%
     summarise(total_of_country = n())
   
   # Join back to the original data
   d_out <- d_out %>%
     left_join(country_count, by = "nation_abbreviation")
   
   d_out$year <- as.factor(d_out$year)
   return(d_out)
 })
 
# Data dictionary
 output$data_summary <- DT::renderDataTable({
   if (!is.null(data_internal$raw)) {
     datadict <- read.csv("www/data-dictionary.csv")
     DT::datatable(
       datadict,
       options = list(
         pageLength = 39,
         lengthChange = FALSE,
         paging = FALSE,
         info = FALSE
       )
     )
   }
 })

 # Data section 
 output$filtered_table <- DT::renderDataTable(
   DT::datatable(
     data_active(),
     extensions = "Buttons",
     filter = "top",
     plugins = "ellipsis",
     style = "bootstrap",
     options = list(
       scrollX = TRUE,
       scrollY = TRUE,
       pageLength = 3,
       autoWidth = FALSE,
       responsive = T,
       dom = "<'row'<'col-sm-2'f>><'row'<'col-sm-6'B>>rtlip",
       buttons = list(
         list(
           extend = "copy",
           text = "Copy",
           exportOptions = list(
             modifier = list(page = "all"),
             orthogonal = "export"
           )
         ),
         list(
           extend = "csv",
           text = "CSV",
           filename = "FDP_Data_atlas",
           exportOptions = list(
             modifier = list(page = "all"),
             orthogonal = "export"
           )
         ),
         list(
           extend = "excel",
           text = "Excel",
           filename = "FDP_Data_atlas",
           exportOptions = list(
             modifier = list(page = "all"),
             orthogonal = "export"
           )
         ),
         list(
           extend = "print",
           text = "Print",
           exportOptions = list(
             modifier = list(page = "all"),
             orthogonal = "export"
           )
         )
       ),
       columnDefs = list(list(
         targets = "_all",
         render = JS("$.fn.dataTable.render.ellipsis( 30 )")
       ))
     ),
     class = "display"
   ),
   server = F
 )
 
 # Frequency Plot
 output$location_plot_selector <- renderUI({
   req(data_internal$raw)
  
   selectInput(
     inputId = "select_loc_col",
     label = "Plot the number of datasets by:",
     choices = c("", get_cols_for_plot(data_active())),
     selected = "nation_name"
   )
 })
 
 # Descriptive plots
 gen_location_trend_plot <- reactive({
   GenDescPlots(data_active(), input$select_loc_col)
 })
 
 output$plot2 <- renderPlotly({
   req(input$select_loc_col)
   gen_location_trend_plot()
 })
 
 # Heatmap
 output$heatmap_selector <- renderUI({
   req(data_internal$raw)
   div(list(
     div(style = "display: inline-block; width = '10%'",
         br()),
     div(
       style = "display: inline-block; width = '40%'",
       title = "Select which categorical variable you wish to cross tabulate along the x axis in a heat map.)",
       selectInput(
         inputId = "heat_select_x",
         label = "Select X variable",
         choices = c("", get_cols_for_plot(data_active())),
         selected = "year",
       )
     ),
     div(
       style = "display: inline-block; width = '40%'",
       title = "Select which categorical variable you wish to cross tabulate along the y axis in a heat map.)",
       selectInput(
         inputId = "heat_select_y",
         label = "Select Y variable",
         choices = c("", get_cols_for_plot(data_active())),
         selected = "Region"
       )
     )
   ))
 })
 
 gen_heatmap <- reactive({
   GenHeatMap(data_active(),
              c(input$heat_select_x, input$heat_select_y))
 })
 
 output$heat_x_axis <- renderPrint({
   input$heat_select_x
 })
 output$heat_y_axis <- renderPrint({
   input$heat_select_y
 })
 
 output$heatmap <- renderPlotly({
   req(input$heat_select_x)
   req(input$heat_select_y)
   gen_heatmap()
 })
 

# Data Atlas Tab
 gen_map <- reactive(sys_map(data_active()))
 
# Observe click events
 clicked_ISO_A3 <- reactiveVal(NULL)
 observe({
   click <- input$map_shape_click
   if (is.null(click)) {
     return()
   }
   clicked_ISO_A3(click$id)
 })
 
 # Refugee statistics
 ref_data_filtered <- reactive({
   FDPDataAtlas::ref_data %>%
     filter(indicator == input$selected_variable)
 })
 
 
 # render map
 observe({
      lat_plotted <-
     as.numeric(unlist(data_active() %>%
                         dplyr::select(Latitude)))
   lng_plotted <-
     as.numeric(unlist(data_active() %>%
                         dplyr::select(Longitude)))
   
   
   lat_plotted[is.na(lat_plotted)] <- 0
   lng_plotted[is.na(lng_plotted)] <- -20

# Color schema
   # basemap
   breaks <- quantile(ref_data_filtered()$value,
                      probs = seq(0, 1, 0.25),
                      na.rm = TRUE)
   breaks <- rev(breaks)
   
   navy_colors <-
     c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d")
   pal <-
     colorBin(navy_colors, domain = ref_data_filtered()$value, bins = breaks)
   
   # surveys
   custom_pal <- colorRampPalette(c("#edf8e9", "#006d2c"))
   circle_pal <-
     colorNumeric(palette = custom_pal(5),
                  domain = data_active()$total_of_country)
   
   
# Display info in sidebar
   output$country_info <- renderUI({
     if (is.null(clicked_ISO_A3())) {
       return(p(""))
     } else {
       filtered_data <- FDPDataAtlas::metadata %>%
         filter(nation_abbreviation == clicked_ISO_A3())
       
       if (nrow(filtered_data) == 0) {
         return(p("No survey available for this country."))
       }
       
       nation_names <- filtered_data$nation_name
       statement_titles <- filtered_data$statement_title
       data_urls <- filtered_data$data_url
       nation_abbreviation <- filtered_data$nation_abbreviation
       abstract <- filtered_data$abstract
       
       text_to_display <-
         paste0("<h3>", head(filtered_data$nation_name, 1), "</h3>")
       
       for (i in 1:length(nation_names)) {
         unique_id <- paste0("info", i)
         text_to_display <- paste0(
           text_to_display,
           "<div>",
           "<a href='javascript:void(0);' onclick='toggleInfo(\"",
           unique_id,
           "\");'>",
           statement_titles[i],
           "</a>",
           "<div id='",
           unique_id,
           "' style='display:none;'>",
           "URL: ",
           "<a target='_blank' href='",
           data_urls[i],
           "'>",
           data_urls[i],
           "</a><br/>",
           "Abstract: ",
           abstract[i],
           "<br/>",
           "</div>",
           "</div>",
           "<br>"
         )
       }
       
       text_to_display <- paste0(
         "<script>",
         "function toggleInfo(id) {",
         "var x = document.getElementById(id);",
         "if (x.style.display === 'none') {",
         "x.style.display = 'block';",
         "} else {",
         "x.style.display = 'none';",
         "}",
         "}",
         "</script>",
         text_to_display
       )
       
       return(HTML(text_to_display))
     }
   })
   
   # map
   output$map <- renderLeaflet({
     gen_map()  %>%
       addProviderTiles(providers$Esri.WorldTerrain) 
     # #leaflet::addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/gsdpm/civtteddj000z2jodf6dv7vw4/tiles/256/{z}/{x}}/{y}@2x?access_token=pk.eyJ1IjoiZ3NkcG0iLCJhIjoiY2toZjFvZ3gwMG1qODJ4cnpwaDdvenpzMiJ9.01pv2kccL9cXhxO6B-Naiw") %>%
   })
   
   leafletProxy("map", data = data_active()) %>%
     leaflet::clearMarkers() %>%
     leaflet::addPolygons(
       data = FDPDataAtlas::bounds,
       layerId = ~ ISO_A3,
       fillColor = ~ pal(ref_data_filtered()$value),
       color = "white",
       dashArray = "3",
       weight = 1,
       fillOpacity = 0.7,
       label = FDPDataAtlas::bounds$NAME_EN
     ) %>%
     leaflet::addLegend(
       pal = pal,
       values = ~ ref_data_filtered()$value,
       opacity = 0.7,
       position = "bottomright",
       title = "Basemap",
       layerId = "ref_data",
     ) %>%
     leaflet::addCircleMarkers(
       lat = ~ lat_plotted,
       lng = ~ lng_plotted,
       layerId = ~ nation_abbreviation,
       radius = 3 + sqrt(4 * data_active()$total_of_country),
       color = circle_pal(data_active()$total_of_country),
       stroke = FALSE,
       fillOpacity = 0.7,
       label = sprintf(
         "%s: %d surveys",
         data_active()$nation_name,
         data_active()$total_of_country
       )
     ) %>%
     leaflet::addLabelOnlyMarkers(
       lat = ~ lat_plotted,
       lng = ~ lng_plotted,
       label = sprintf("%d", data_active()$total_of_country),
       labelOptions = labelOptions(
         noHide = T,
         direction = 'center',
         textOnly = T
       )
     )
 })
 
 # end shiny server
})
