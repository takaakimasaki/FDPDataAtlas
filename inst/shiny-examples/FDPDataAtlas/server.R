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

if (webshot::is_phantomjs_installed() == FALSE) {
  webshot::install_phantomjs()
}

start_text <- read_file("www/AboutEvi.html")

max_file_size_mb <- 100
options(shiny.maxRequestSize = max_file_size_mb * 1024^2)

shinyServer(
  function(input, output, session) {
    data_internal <- reactiveValues(
      raw = NULL,
      cols = NULL,
      short_cols = NULL
    )


    # DATA TAB

    data_internal$raw <- FDPDataAtlas::metadata %>% as.data.frame()

    output$start_text <- renderPrint({
      cat(start_text)
    })

    data_active <- reactive({
      req(data_internal$raw)

      d_out <- data_internal$raw

      # Group by 'nation_abbreviation' and count the number of records for each group
      country_count <- d_out %>%
        group_by(nation_abbreviation) %>%
        summarise(total_of_country = n())

      # Join this summary back to the original data
      d_out <- d_out %>%
        left_join(country_count, by = "nation_abbreviation")

      return(d_out)
    })


    # outline of what the dataset contains
    output$data_summary <- renderTable({
      if (!is.null(data_internal$raw)) {
        datadict <- read.csv("../../../data/datadictionary.csv")
        return(datadict)
      }
    })

    ##### end dynamic filter ####

    output$filtered_table <- DT::renderDataTable(
      DT::datatable(data_active(),
        extensions = "Buttons",
        filter = "top",
        # caption = "Use the boxes below column headers to filter data",
        # class = c('display', 'compact'),
        style = "bootstrap",
        options = list(
          scrollX = TRUE,
          scrollY = TRUE,
          pageLength = 10,
          autoWidth = FALSE,
          responsive = T,
          dom = "Blfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print"),
          columnDefs = list(list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data != null && data.length > 30 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
              "}"
            )
          ))
        ), # ,
        class = "display"
      ),
      server = F
    )


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
      div(
        selectizeInput(
          inputId = "map_popup_select",
          label = "Select Popup Info",
          selected = c("statement_title", "data_url"),
          choices = colnames(data_active()),
          multiple = T
        )
      )
    })

    output$atlas_color_by <- renderUI({
      req(data_internal$raw)
      colnames <- FDPDataAtlas::metadata %>%
        dplyr::select(!where(is.numeric)) %>%
        colnames()
      div(
        title = "Select variable to color points by",
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

    # geom_bar rather than geom_histogram so that non-continous variables can be plotted

    gen_location_trend_plot <- reactive({
      GenLocationTrend(data_active(), input$select_loc_col)
    })

    output$plot2 <- renderPlot({
      req(input$select_loc_col)
      gen_location_trend_plot()
    })

    output$save_plot_1 <- downloadHandler(
      filename = "FDPDataAtlas1.png",
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(...,
            width = width, height = height,
            res = 300, units = "in"
          )
        }
        ggsave(file, plot = gen_time_trend_plot(), device = device)
      }
    )

    output$save_plot_2 <- downloadHandler(
      filename = "FDPDataAtlas2.png",
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(...,
            width = width, height = height,
            res = 300, units = "in"
          )
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

    output$heat_x_axis <- renderPrint({
      input$heat_select_x
    })
    output$heat_y_axis <- renderPrint({
      input$heat_select_y
    })

    output$save_heatmap <- downloadHandler(
      filename = "FDPDataAtlasHeatmap.png",
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(...,
            width = width, height = width,
            res = 300, units = "in"
          )
        }
        ggsave(file, plot = gen_heatmap(), device = device)
      }
    )

    # Data Atlas Tab
    generate_systematic_map <- reactive(
      sys_map(data_active())
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

    cluster_level <- reactive({
      input$cluster_size_select
    })


    observe({
      req(!is.null(input$atlas_color_by_select)) # could be anything in the evidence atlas panel

      lat_plotted <-
        as.numeric(unlist(data_active() %>%
          dplyr::select(Latitude)))
      lng_plotted <-
        as.numeric(unlist(data_active() %>%
          dplyr::select(Longitude)))

      # replace missing lat/long with standard locations chosen by 'nonplotted' input
      
      lat_plotted[is.na(lat_plotted)] <- 0
      lng_plotted[is.na(lng_plotted)] <- -20

      if (input$atlas_color_by_select != "") {
        color_user <- input$atlas_color_by_select
        factpal <- colorFactor(RColorBrewer::brewer.pal(9, "Set1"),
          data_active()$color_user,
          reverse = TRUE
        )
        colorby <- ~ factpal(data_active()[[color_user]])

        if (length(unique(data_active()[, color_user])) < 9) {
          leafletProxy("map", data = data_active()) %>%
            leaflet::removeControl("ref_data") %>%
            leaflet::addLegend(
              title = stringr::str_to_title(stringr::str_replace_all(color_user, "\\.", " ")),
              position = "topright",
              pal = factpal,
              values = data_active()[, color_user],
              layerId = "color_by_legend",
              group = "legend",
              na.label = "None",
              opacity = .8
            )
        } else {
          leafletProxy("map") %>%
            leaflet::removeControl("color_by_legend")
        }
      } else {
        colorby <- "blue"
      }

      # Refugee statistics
      ref_data_filtered <- reactive({
        FDPDataAtlas::ref_data %>%
          filter(indicator == input$selected_variable)
      })
  
      breaks <- quantile(ref_data_filtered()$value, probs = seq(0, 1, 0.25), na.rm = TRUE)
      breaks <- rev(breaks)
      pal <- colorBin("Reds", domain = ref_data_filtered()$value, bins = breaks)
      # Create a custom color palette function
      custom_pal <- colorRampPalette(c("lightblue", "#4747ff"))
      circle_pal <- colorNumeric(palette = custom_pal(5), domain = data_active()$total_of_country)



    polygon_labels <- sprintf("<h5>%s</h5> <br> <b>%s</b>: %g", ref_data_filtered()$NAME_EN, ref_data_filtered()$indicator,ref_data_filtered()$value) %>%
        lapply(htmltools::HTML)


# Function to generate popup text based on selected columns and ISO_A3
generate_popup_text <- function(selected_columns, data_active) {

  filtered_data <- dplyr::filter(data_active(), nation_abbreviation == "BRA")
  
  popup_text <- ""
  
  for (i in 1:nrow(filtered_data)) {
    
    row <- filtered_data[i,]
    
    for (col in selected_columns) {
    
      value <- row[[col]]
      
      popup_text <- paste0(popup_text, 
        "<strong>", col, ":</strong> ", value, "<br>")
        
    }
    
    popup_text <- paste0(popup_text, "<br>")
    
  }

  return(popup_text)

}

# Function to generate the complete popup content
generate_popup_content <- function(ISO_A3, data_active, input_map_popup_select) {
  # Generate popup text based on selected columns and ISO_A3
  popup_text <- generate_popup_text(input_map_popup_select, data_active())

  # Create the final popup string
  popup_info <- paste("Country code:", ISO_A3, "<br>", popup_text)

  return(popup_info)
}
      leafletProxy("map", data = data_active()) %>%
        leaflet::clearMarkers() %>%
        leaflet::addPolygons(
          data = FDPDataAtlas::bounds,
          fillColor = ~ pal(ref_data_filtered()$value),
          color = "white", # set the border color (e.g., black, blue, etc)
          dashArray = "3", # set the dash of the border (e.g., 1,2,3, etc)
          weight = 1, # set the thickness of the border (e.g., 1,2,3, etc)
          fillOpacity = 0.7, # set the transparency of the border (range: 0-1)
          label = polygon_labels,
          # popup = ~ generate_popup_content(ISO_A3,data_active()),
          popup = ~ generate_popup_content(ISO_A3, data_active, input$map_popup_select)
         # popup = ~ paste("code", ISO_A3)
        ) %>%
        leaflet::addCircleMarkers(
          lat = ~lat_plotted, lng = ~lng_plotted,
          # popup = ~ paste(popup_string()),
          # popupOptions = popupOptions(
          #   maxWidth = 500,
          #   maxHeight = 200
          # ),
          radius = 10,
          color = circle_pal(data_active()$total_of_country),
          stroke = FALSE,
          fillOpacity = 0.7,
          label = ~total_of_country,
          labelOptions = labelOptions(noHide = TRUE, offset = c(0, 0), textOnly = TRUE)
        ) %>%
        leaflet::addLegend(
          pal = pal,
          values = ~ ref_data_filtered()$value,
          opacity = 0.7, # set the transparency of the legend (range: 0-1)
          title = input$selected_variable,
          layerId = "ref_data"
        )
    })

    observeEvent(input$map_title_select, {
      leafletProxy("map") %>%
        leaflet::removeControl("atlas_title") %>%
        leaflet::addControl(input$map_title_select,
          position = "topleft",
          className = "map-title",
          layerId = "atlas_title"
        )
    })

    observeEvent(input$map_basemap_select, {
      leafletProxy("map") %>%
        leaflet::removeTiles("atlas_basemap") %>%
        leaflet::addProviderTiles(input$map_basemap_select,
          group = input$map_basemap_select,
          layerId = "atlas_basemap", options = providerTileOptions(noWrap = TRUE)
        )
    })

    atlas_for_saving <- reactive({
      # This is redundant to everything in the app, but the is best solution I could find
      # for saving a map that's been heavily edited with leafletProxy
      if (input$sample_or_real == "shapefile") {
        if (input$map_basemap_select == "OpenStreetMap") {
          return(
            sys_map_shapefile(data_active(),
              popups = popup_string()
            ) %>%
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
          )
        } else {
          return(
            sys_map_shapefile(data_active(),
              popups = popup_string()
            ) %>%
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
              leaflet::addProviderTiles(input$map_basemap_select,
                group = input$map_basemap_select,
                layerId = "atlas_basemap", options = providerTileOptions(noWrap = TRUE)
              )
          )
        }
      } # end shapefile

      

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
        factpal <- colorFactor(RColorBrewer::brewer.pal(9, "Set1"),
          data_active()$color_user,
          reverse = TRUE
        )
        colorby <- ~ factpal(data_active()[[color_user]])
      } else {
        colorby <- "blue"
      }

      # if points are coloured then legend is needed
      if (input$map_basemap_select == "OpenStreetMap") {
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
              className = "map-title",
              layerId = "atlas_title"
            ) %>%
            leaflet::addTiles() %>%
            leaflet::addCircleMarkers(
              lat = ~lat_plotted, lng = ~lng_plotted,
              popup = ~paste(popup_string()),
              radius = 10,
              color = colorby,
              stroke = FALSE, fillOpacity = 0.7,
              label = ~ popup_string() %>% lapply(shiny::HTML)
            ) %>%
            leaflet::removeControl("ref_data") %>%
            leaflet::addLegend(
              title = stringr::str_to_title(stringr::str_replace_all(color_user, "\\.", " ")),
              position = "topright",
              pal = factpal,
              values = data_active()[, color_user],
              layerId = "color_by_legend",
              group = "legend",
              na.label = "None",
              opacity = .8
            )
        } else { # if no legend needed...
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
              className = "map-title",
              layerId = "atlas_title"
            ) %>%
            leaflet::addTiles() %>%
            leaflet::addCircleMarkers(
              lat = ~lat_plotted, lng = ~lng_plotted,
              popup = ~ paste(popup_string()),
              radius = 10,
              color = colorby,
              stroke = FALSE, fillOpacity = 0.7,
              label = ~ popup_string() %>% lapply(shiny::HTML)
            )
        }
      } else {
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
              className = "map-title",
              layerId = "atlas_title"
            ) %>%
            leaflet::addProviderTiles(input$map_basemap_select,
              group = input$map_basemap_select,
              layerId = "atlas_basemap", options = providerTileOptions(noWrap = TRUE)
            ) %>%
            leaflet::addCircles(
              lat = ~lat_plotted, lng = ~lng_plotted,
              popup = ~ paste(popup_string()),
              radius = 10,
              color = colorby,
              stroke = FALSE, fillOpacity = 0.7,
              label = ~ popup_string() %>% lapply(shiny::HTML)
            ) %>%
            leaflet::removeControl("ref_data") %>%
            leaflet::addLegend(
              title = stringr::str_to_title(stringr::str_replace_all(color_user, "\\.", " ")),
              position = "topright",
              pal = factpal,
              values = data_active()[, color_user],
              layerId = "color_by_legend",
              group = "legend",
              na.label = "None",
              opacity = .8
            )
        } else { # if no legend needed...
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
              className = "map-title",
              layerId = "atlas_title"
            ) %>%
            leaflet::addProviderTiles(input$map_basemap_select,
              group = input$map_basemap_select,
              layerId = "atlas_basemap", options = providerTileOptions(noWrap = TRUE)
            ) %>%
            leaflet::addCircleMarkers(
              lat = ~lat_plotted, lng = ~lng_plotted,
              popup = ~ paste(popup_string()),
              radius = 10,
              color = colorby,
              stroke = FALSE, fillOpacity = 0.7,
              label = ~ popup_string() %>% lapply(shiny::HTML)
            )
        }
      }
    })

    output$savemap_interactive <- downloadHandler(
      filename = paste0("FDPDataAtlasMap", Sys.Date(), ".html"),
      content = function(file) {
        saveWidget(
          widget = atlas_for_saving(),
          file = file
        )
      }
    )

    output$savemap_pdf <- downloadHandler(
      filename = paste0("FDPDataAtlasMap", Sys.Date(), ".pdf"),
      content = function(file) {
        mapview::mapshot(
          x = atlas_for_saving(),
          file = file,
          cliprect = "viewport",
          selfcontained = FALSE
        )
      }
    )

    output$savemap_png <- downloadHandler(
      filename = paste0("FDPDataAtlasMap", Sys.Date(), ".png"),
      content = function(file) {
        mapview::mapshot(
          x = atlas_for_saving(),
          file = file,
          cliprect = "viewport",
          selfcontained = FALSE
        )
      }
    )


    # outputOptions(output, "cluster_columns", suspendWhenHidden = FALSE)
  }
)
