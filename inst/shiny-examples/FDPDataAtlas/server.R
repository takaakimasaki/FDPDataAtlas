## server.R ##

# load functions
library(FDPDataAtlas)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(readr)
library(DT)
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

if(webshot::is_phantomjs_installed()==FALSE){
  webshot::install_phantomjs()
}

# load data + text
#load("www/pilotdata.rdata")
start_text <- read_file("www/AboutEvi.html")
# about_sysmap_text <- read_file("www/AboutSysMap.html")
# how_cite_text <- read_file("www/HowCiteEvi.html")
# how_works_text <- read_file("www/HowEviWorks.html")

# maximum upload size 100 MB-- could be increased if proves problematic for users and we have server space
max_file_size_mb <- 100
options(shiny.maxRequestSize = max_file_size_mb*1024^2)

shinyServer(

  function(input, output, session){

    data_internal <- reactiveValues(
      raw = NULL,
      cols = NULL,
      short_cols = NULL#,
      #filtered = NULL
    )


    # DATA TAB
    # if no data are available but input$sample_or_real == 'sample', show intro text
    
    data_internal$raw <- FDPDataAtlas::metadata %>% as.data.frame()
    
    output$start_text <- renderPrint({
      cat(start_text)
    })
    output$about_sysmap_text <- renderPrint({
      cat(about_sysmap_text)
    })
    output$how_works_text <- renderPrint({
      cat(how_works_text)
    })
    output$how_cite_text <- renderPrint({
      cat(how_cite_text)
    })


    # if CSV data are supplied, add them to data_internal
    observeEvent(input$sysmapdata_upload, {
      data_internal$raw <- read.csv(
        file = input$sysmapdata_upload$datapath,
        header = input$header,
        sep = input$sep,
        dec = input$dec,
        quote = input$quote,
        fileEncoding = input$upload_encoding,
        stringsAsFactors = F)
      #data_internal$filtered <- data_internal$raw #instantiate filtered table with raw values
    })

    # if shapefile data are supplied, add them to data_internal
    observeEvent(input$shape, {
      req(input$shape)

      shpdf <- input$shape
      tempdirname <- dirname(shpdf$datapath[1])
      for(i in 1:nrow(shpdf)){
        file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
      }
      data_internal$raw <- sf::st_read(
        paste(tempdirname,
              shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
              sep="/")
      )

      #data_internal$filtered <- data_internal$raw #instantiate filtered table with raw values
    })

    data_active <- reactive({
      req(data_internal$raw)

      #dataset_gateway <- (!is.null(input$map_filtered_select))
      #d_out <- if (dataset_gateway && input$map_filtered_select == TRUE) {
      # data_internal$filtered
      #} else {
      d_out <-data_internal$raw
      # }

      # d_out
    })


    # give an outline of what that dataset contains
    output$data_summary <- renderTable({
      if(!is.null(data_internal$raw)){
        datadict <- read.csv("../../../data/datadictionary.csv")
        return(datadict)
      }
    })

    ##### end dynamic filter ####

    output$filtered_table <- DT::renderDataTable(
      DT::datatable(data_active(),
                    extensions = 'Buttons',
                    filter = 'top',
                    #caption = "Use the boxes below column headers to filter data",
                    #class = c('display', 'compact'),
                    style='bootstrap',
                    options = list(scrollX = TRUE,
                                   scrollY = TRUE,
                                   pageLength = 10,
                                   autoWidth = FALSE,
                                   responsive=T,
                                   dom='Blfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   columnDefs = list(list(
                                     targets = "_all",
                                     render = JS(
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data != null && data.length > 30 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                       "}")
                                   ))
                                   ), #,
                    class="display"
      ),
      server = F)

  

    output$atlas_link_popup <- renderUI({
      # req(input$sample_or_real != "shapefile") #does not work for shapefiles currently

      div(
        title = "If your dataset has a link to each dataset, you can include it in the popup when a point is clicked with the mouse. If you have any hyperlinks you wish to display in the pop-up (e.g. email addresses or URLs), select them here.",
        selectInput(
          inputId = "map_link_select",
          label = "Select Link Column (in pop-up)",
          choices = c("", "data_url"),
          selected = "data_url"
        )
      )

    })


    output$atlas_selectmap <- renderUI({
      req(data_internal$raw)

      div(
        title = "You can change the default basemap to highlight different geographical features or change the language of map labels",
        selectInput(
          inputId = "map_basemap_select",
          label = "Select Basemap",
          choices = c("OpenStreetMap", "OpenTopoMap", "Stamen.TonerLite", "Esri.WorldStreetMap"),
          selected = "OpenStreetMap"
        )
      )
    })


    output$atlas_popups <- renderUI({
      # req(data_internal$raw)

      div(
       selectizeInput(
          inputId = "map_popup_select",
          label = "Select Popup Info",
          selected = c("statement_title","data_url"),
          choices = colnames(data_active()),
          multiple = T
        )
      )
    })

    output$cluster_columns <- renderUI({
      req(data_internal$raw)
      # req(input$sample_or_real != "shapefile") #does not work for shapefiles currently

      div(
        title = "Toggle displaying points in relative geographic clusters",
        shinyWidgets::materialSwitch(
          inputId = "map_cluster_select",
          label = "Cluster Map Points?",
          value = TRUE,
          status = "primary"
        )
      )
    })

    output$cluster_size <- renderUI({
      div(
        title = "Adjust cluster sensitivity. Higher numbers correspond to smaller distances",
        shinyWidgets::noUiSliderInput(
          inputId = "cluster_size_select",
          label = "Cluster Sensitivity",
          value = 4,
          step = 1,
          min = 6,
          max = 16)
      )
    })

    output$atlas_color_by <- renderUI({
      req(data_internal$raw)
      colnames <- FDPDataAtlas::metadata %>% dplyr::select(!where(is.numeric)) %>% colnames()
      div(
        title="Select variable to color points by",
        selectInput(
          inputId = "atlas_color_by_select",
          label = "Color points by:",
          choices = c("", colnames),
          selected = ""
        )
      )
    })



    # Location Frequency Plot
    output$location_plot_selector <- renderUI({
      req(data_internal$raw)

      selectInput(
        inputId = "select_loc_col",
        label = "Plot the number of datasets by:",
        choices = c("", get_histogram_viable_columns(data_active())),
        selected = ""
      )
    })

    ## HEATMAP
    output$heatmap_selector <- renderUI({
      req(data_internal$raw)
      div(
        list(
          div(
            style = "display: inline-block; width = '10%'",
            br()
          ),
          div(
            style = "display: inline-block; width = '40%'",
            title = "Select which categorical variable you wish to cross tabulate along the x axis in a heat map. Values must be discrete categories (i.e. not free text and not decimal)",
            selectInput(
              inputId = "heat_select_x",
              label = "Select X variable",
              choices = c("", get_histogram_viable_columns(data_active())),
              selected = ""
            )
          ),
          div(
            style = "display: inline-block; width = '40%'",
            title = "Select which categorical variable you wish to cross tabulate along the y axis in a heat map. Values must be discrete categories (i.e. not free text and not decimal)",
            selectInput(
              inputId = "heat_select_y",
              label = "Select Y variable",
              choices = c("", get_histogram_viable_columns(data_active())),
              selected = ""
            )
          )
        )
      )
    })

    #geom_bar rather than geom_histogram so that non-continous variables can be plotted

    gen_location_trend_plot <- reactive({
      GenLocationTrend(data_active(), input$select_loc_col)
    })

    output$plot2 <- renderPlot({
      req(input$select_loc_col)
      gen_location_trend_plot()
    })

    output$save_plot_1 <- downloadHandler(
      filename = 'FDPDataAtlas1.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        ggsave(file, plot = gen_time_trend_plot(), device = device)
      }
    )

    output$save_plot_2 <- downloadHandler(
      filename = 'FDPDataAtlas2.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        ggsave(file, plot = gen_location_trend_plot(), device = device)
      }
    )

    gen_heatmap <- reactive({
      GenHeatMap(data_active(), c(input$heat_select_x, input$heat_select_y))
    })

    output$heatmap <- renderPlot({
      req(input$heat_select_x)
      req(input$heat_select_y)
      gen_heatmap()
    })

    output$heat_x_axis <- renderPrint({ input$heat_select_x })
    output$heat_y_axis <- renderPrint({ input$heat_select_y })

    output$save_heatmap <- downloadHandler(
      filename = 'FDPDataAtlasHeatmap.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = width,
                         res = 300, units = "in")
        }
        ggsave(file, plot = gen_heatmap(), device = device)
      }
    )

    generate_systematic_map <- reactive(
        sys_map(data_active() )
    )

    output$map <- renderLeaflet({
      generate_systematic_map() %>%
        onRender(
          "function(el, x) {
            L.easyPrint({
              sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
              filename: 'FDPDataAtlasMap',
              exportOnly: true,
              hideControlContainer: true
            }).addTo(this);
            }"
        )
    })

    cluster_level <- reactive({input$cluster_size_select})

    popup_string <- reactive({
      popup_string <- ''
      #TODO: Vectorize this
      for (popup in input$map_popup_select) {
        popup_string = paste0(
          popup_string, "<strong>", popup, '</strong>: ',
          str_replace_all(str_wrap(data_active()[[popup]]), coll("\n"), "<br/>"), "<br/>"
        )
      }
      popup_string
    })

    atlas_point_links <- reactive({
      if (input$map_link_select != "") {
        links_input <- sapply(data_active()[input$map_link_select], as.character)
        links = paste0("<strong><a target='_blank' rel='noopener noreferrer' href='",
                       links_input, "'>Link to data</a></strong>")
      } else {links <- ""}
      links
    })

    cluster_options <- reactive({
      ifelse(input$map_cluster_select,
              parse(text=paste0('markerClusterOptions(freezeAtZoom = ',
                                input$cluster_size_select, ",spiderfyDistanceMultiplier=2" ,')')),
              NULL)

    })

    observe({
      req(!is.null(input$atlas_color_by_select)) #could be anything in the evidence atlas pane
      # req(input$sample_or_real != 'shapefile') #shapefiles are handled differently, so they have their own section

      radiusby <- input$atlas_radius_select

      lat_plotted <-
        as.numeric(unlist(data_active() %>%
                            dplyr::select(Latitude)))
      lng_plotted <-
        as.numeric(unlist(data_active() %>%
                            dplyr::select(Longitude)))

      # replace missing lat/long with standard locations chosen by 'nonplotted' input
      #if(input$nonplotted == 'Not plotted'){
      lat_plotted[is.na(lat_plotted)] <- 0
      lng_plotted[is.na(lng_plotted)] <- -20

      if (input$atlas_color_by_select != "") {
        color_user <- input$atlas_color_by_select
        factpal <- colorFactor(RColorBrewer::brewer.pal(9, 'Set1'),
                               data_active()$color_user, reverse = TRUE)
        colorby <- ~factpal(data_active()[[color_user]])

        if (length(unique(data_active()[, color_user])) < 9) {
          leafletProxy("map", data = data_active()) %>%
            leaflet::removeControl("ref_data") %>%
            leaflet::addLegend(
              title = stringr::str_to_title(stringr::str_replace_all(color_user, "\\.", " ")),
              position = 'topright',
              pal = factpal,
              values = data_active()[, color_user],
              layerId = "color_by_legend",
              group = "legend",
              na.label = "None",
              opacity = .8
            )
        }
        else {
          leafletProxy("map") %>%
            leaflet::removeControl("color_by_legend")
        }

      } else {
        colorby <- "blue"
      }

      #Refugee statistics
      ref_data_filtered <- reactive({
        FDPDataAtlas::ref_data %>%
          filter(indicator==input$selected_variable)
      })

      breaks <- quantile(ref_data_filtered()$value, probs = seq(0, 1, 0.25), na.rm=TRUE)
      breaks <- rev(breaks)
      pal <- colorBin("Reds", domain = ref_data_filtered()$value, bins = breaks)

      labels <- sprintf("%s: %g", ref_data_filtered()$NAME_EN, ref_data_filtered()$value) %>%
        lapply(htmltools::HTML)

      leafletProxy("map", data = data_active()) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                  popup = ~paste(popup_string(), atlas_point_links()),
                                  popupOptions = popupOptions(maxWidth = 500,
                                                              maxHeight = 200),
                                  radius = ~as.numeric(radiusby * 3),
                                  color = colorby,
                                  stroke = FALSE, fillOpacity = 0.7,
                                  label = ~lapply(popup_string(), shiny::HTML),
                                  clusterOptions = eval(cluster_options())) %>%
                                    addPolygons(
                                      data = FDPDataAtlas::bounds,
                                      fillColor = ~ pal(ref_data_filtered()$value),
                                      color = "white", # set the border color (e.g., black, blue, etc)
                                      dashArray = "3", # set the dash of the border (e.g., 1,2,3, etc)
                                      weight = 1, # set the thickness of the border (e.g., 1,2,3, etc)
                                      fillOpacity = 0.7, # set the transparency of the border (range: 0-1)
                                      label = labels) %>%
                                    leaflet::addLegend(
                                      pal = pal,
                                      values = ~ref_data_filtered()$value,
                                      opacity = 0.7, # set the transparency of the legend (range: 0-1)
                                      title = input$selected_variable,
                                      layerId = "ref_data")


    })

    observeEvent(input$map_title_select, {

      leafletProxy("map") %>%
        leaflet::removeControl("atlas_title") %>%
        leaflet::addControl(input$map_title_select,
                            position = "topleft",
                            className="map-title",
                            layerId = "atlas_title")

    })

    observeEvent(input$map_basemap_select, {
      leafletProxy("map") %>%
        leaflet::removeTiles("atlas_basemap") %>%
        leaflet::addProviderTiles(input$map_basemap_select,  group=input$map_basemap_select,
                                  layerId = "atlas_basemap", options = providerTileOptions(noWrap=TRUE))

    })

    atlas_for_saving <- reactive({
      # This is redundant to everything in the app, but the is best solution I could find
      # for saving a map that's been heavily edited with leafletProxy
      if(input$sample_or_real == 'shapefile') {
        if(input$map_basemap_select=="OpenStreetMap"){
          return(
            sys_map_shapefile(data_active(),
                              popups = popup_string()) %>%
              setView(
                lng = input$map_center$lng,
                lat = input$map_center$lat,
                zoom = input$map_zoom
              ) %>%
              leaflet::addControl(
                input$map_title_select,
                position = "topleft",
                className = "map-title",
                layerId = "atlas_title"
              ) %>%
              leaflet::addTiles()
          )}
        else{
          return(
            sys_map_shapefile(data_active(),
                              popups = popup_string()) %>%
              setView(
                lng = input$map_center$lng,
                lat = input$map_center$lat,
                zoom = input$map_zoom
              ) %>%
              leaflet::addControl(
                input$map_title_select,
                position = "topleft",
                className = "map-title",
                layerId = "atlas_title"
              ) %>%
              leaflet::addProviderTiles(input$map_basemap_select, group=input$map_basemap_select,
                                        layerId = "atlas_basemap", options = providerTileOptions(noWrap=TRUE))
          )

        }

      } # end shapefile

      radiusby <- input$atlas_radius_select

      lat_plotted <-
        as.numeric(unlist(data_active() %>%
                            dplyr::select(input$map_lat_select)))
      lng_plotted <-
        as.numeric(unlist(data_active() %>%
                            dplyr::select(input$map_lng_select)))

      # replace missing lat/long with standard locations chosen by 'nonplotted' input
       lat_plotted[is.na(lat_plotted)] <- 0
        lng_plotted[is.na(lng_plotted)] <- -20
      if (input$atlas_color_by_select != "") {
        color_user <- input$atlas_color_by_select
        factpal <- colorFactor(RColorBrewer::brewer.pal(9, 'Set1'),
                               data_active()$color_user, reverse = TRUE)
        colorby <- ~factpal(data_active()[[color_user]])

      } else {
        colorby <- "blue"
      }

      #if points are coloured then legend is needed
      if(input$map_basemap_select=="OpenStreetMap"){
        if (input$atlas_color_by_select != "") {
          # call the foundational Leaflet map
          generate_systematic_map() %>%
            # store the view based on UI
            setView(
              lng = input$map_center$lng,
              lat = input$map_center$lat,
              zoom = input$map_zoom
            ) %>%
            leaflet::addControl(input$map_title_select,
                                position = "topleft",
                                className="map-title",
                                layerId = "atlas_title") %>%
            leaflet::addTiles() %>%
            leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                      popup = ~paste(popup_string(), atlas_point_links()),
                                      radius = ~as.numeric(radiusby * 3),
                                      color = colorby,
                                      stroke = FALSE, fillOpacity = 0.7,
                                      label = ~popup_string() %>% lapply(shiny::HTML),
                                      clusterOptions = eval(cluster_options())
            ) %>%
            leaflet::removeControl("ref_data") %>%
            leaflet::addLegend(
              title = stringr::str_to_title(stringr::str_replace_all(color_user, "\\.", " ")),
              position = 'topright',
              pal = factpal,
              values = data_active()[, color_user],
              layerId = "color_by_legend",
              group = "legend",
              na.label = "None",
              opacity = .8)
        } else { #if no legend needed...
          # call the foundational Leaflet map
          generate_systematic_map() %>%
            # store the view based on UI
            setView(
              lng = input$map_center$lng,
              lat = input$map_center$lat,
              zoom = input$map_zoom
            ) %>%
            leaflet::addControl(input$map_title_select,
                                position = "topleft",
                                className="map-title",
                                layerId = "atlas_title") %>%
            leaflet::addTiles() %>%
            leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                      popup = ~paste(popup_string(), atlas_point_links()),
                                      radius = ~as.numeric(radiusby * 3),
                                      color = colorby,
                                      stroke = FALSE, fillOpacity = 0.7,
                                      label = ~popup_string() %>% lapply(shiny::HTML),
                                      clusterOptions = eval(cluster_options())
            )
        }
      }else{
        if (input$atlas_color_by_select != "") {
          # call the foundational Leaflet map
          generate_systematic_map() %>%
            # store the view based on UI
            setView(
              lng = input$map_center$lng,
              lat = input$map_center$lat,
              zoom = input$map_zoom
            ) %>%
            leaflet::addControl(input$map_title_select,
                                position = "topleft",
                                className="map-title",
                                layerId = "atlas_title") %>%
            leaflet::addProviderTiles(input$map_basemap_select, group=input$map_basemap_select,
                                      layerId = "atlas_basemap", options = providerTileOptions(noWrap=TRUE)) %>%
            leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                      popup = ~paste(popup_string(), atlas_point_links()),
                                      radius = ~as.numeric(radiusby * 3),
                                      color = colorby,
                                      stroke = FALSE, fillOpacity = 0.7,
                                      label = ~popup_string() %>% lapply(shiny::HTML),
                                      clusterOptions = eval(cluster_options())
            ) %>%
            leaflet::removeControl("ref_data") %>%
            leaflet::addLegend(
              title = stringr::str_to_title(stringr::str_replace_all(color_user, "\\.", " ")),
              position = 'topright',
              pal = factpal,
              values = data_active()[, color_user],
              layerId = "color_by_legend",
              group = "legend",
              na.label = "None",
              opacity = .8)
        } else { #if no legend needed...
          # call the foundational Leaflet map
          generate_systematic_map() %>%
            # store the view based on UI
            setView(
              lng = input$map_center$lng,
              lat = input$map_center$lat,
              zoom = input$map_zoom
            ) %>%
            leaflet::addControl(input$map_title_select,
                                position = "topleft",
                                className="map-title",
                                layerId = "atlas_title") %>%
            leaflet::addProviderTiles(input$map_basemap_select, group=input$map_basemap_select,
                                      layerId = "atlas_basemap", options = providerTileOptions(noWrap=TRUE)) %>%
            leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                      popup = ~paste(popup_string(), atlas_point_links()),
                                      radius = ~as.numeric(radiusby * 3),
                                      color = colorby,
                                      stroke = FALSE, fillOpacity = 0.7,
                                      label = ~popup_string() %>% lapply(shiny::HTML),
                                      clusterOptions = eval(cluster_options())
            )

        }}

    })

    output$savemap_interactive <- downloadHandler(
      filename = paste0('FDPDataAtlasMap', Sys.Date(), '.html'),
      content = function(file){
        saveWidget(
          widget = atlas_for_saving(),
          file = file)
      }
    )

    output$savemap_pdf <- downloadHandler(
      filename = paste0('FDPDataAtlasMap', Sys.Date(), '.pdf'),
      content = function(file) {
        mapview::mapshot(x = atlas_for_saving(),
                         file = file,
                         cliprect = 'viewport',
                         selfcontained = FALSE)
      }
    )

    output$savemap_png <- downloadHandler(
      filename = paste0('FDPDataAtlasMap', Sys.Date(), '.png'),
      content = function(file) {
        mapview::mapshot(x = atlas_for_saving(),
                         file = file,
                         cliprect = 'viewport',
                         selfcontained = FALSE)
      }
    )


    outputOptions(output, "cluster_columns", suspendWhenHidden = FALSE)

  })
