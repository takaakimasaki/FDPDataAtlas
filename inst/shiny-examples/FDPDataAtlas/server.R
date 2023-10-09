## server.R ##

# load functions
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
library(leaflet.providers)

if (webshot::is_phantomjs_installed() == FALSE) {
  webshot::install_phantomjs()
}


GenHeatMap = function(idata, selcols, axis_txt_lim = 60){
  
  listone<-listtwo<-n<-NULL
  
  # if  df is a shapefile, remove geometry column
  if (any(class(idata) == 'sf')) {idata <- sf::st_drop_geometry(idata)}
  
  
  # Convert columns to factors to allow for categorical classification for both numeric and character data -------
  tmp <- as.data.frame(sapply(idata[selcols], function(x) as.factor(x)))
  
  
  # Plot Heatmap ------
  heatmp <- tmp %>%
    dplyr::rename(listone=colnames(tmp[1]), listtwo=colnames(tmp[2]))%>%
    dplyr::count(listone, listtwo) %>%
    tidyr::complete(listone, listtwo, fill = list(n = 0)) %>%
    dplyr::mutate(listtwo = forcats::fct_rev(forcats::fct_inorder(listtwo))) %>%  # Sort listtwo in reverse alphabetical order
    ggplot2::ggplot(aes(x = listone, y = listtwo, fill= n, label= n)) +
    ggplot2::geom_tile(aes(alpha = 0.3), color="grey60") +
    ggplot2::geom_text() +
    ggplot2::scale_fill_gradientn(colors = c("white", "#0072BC")) +
    theme_unhcr(grid="N")    + 
    # 
    # + ggplot2::theme(
    #   axis.title.x = ggplot2::element_text(size = 14, margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)), 
    #   axis.title.y = ggplot2::element_text(size = 14, margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0))  
    # )
    ggplot2::xlab(paste0(selcols[1])) +
    ggplot2::ylab(paste0(selcols[2])) +
    ggplot2::labs(fill = "Count") +
    # Limit axis text to a certain number of characters, so that long text doesn't ruin the chart display
    ggplot2::scale_x_discrete(labels = function(x) substr(x, 1, axis_txt_lim)) +
    ggplot2::scale_y_discrete(labels = function(x) substr(x, 1, axis_txt_lim)) + 
    ggplot2::ggtitle("Study Heatmap", subtitle = paste(selcols[2], "by", selcols[1])) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12), 
                   axis.title.y = ggplot2::element_text(size = 12)) 
  
  
  
  heatmp
}

get_histogram_viable_columns <- function(df) {
  list_cols <- colnames(df %>%
             dplyr::select_if(function(x) dplyr::n_distinct(x) < 45))
  
  
  list_toremove <-  c("method_data_collectors_name", "entity contact" , "disclaimer", 
                      "project_specific", "total_of_country","idp", "Longitude", "Latitude") 
  
  final_list <- setdiff(list_cols, list_toremove)
  
  return(final_list)
  
}


start_text <- read_file("www/AboutEvi.html")

max_file_size_mb <- 100
options(shiny.maxRequestSize = max_file_size_mb * 1024 ^ 2)

shinyServer(function(input, output, session) {
  data_internal <- reactiveValues(raw = NULL,
                                  cols = NULL,
                                  short_cols = NULL)
  
  
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
      datadict <- FDPDataAtlas::datadictionary
      return(datadict)
    }
  })
  
  ##### end dynamic filter ####
  
  output$filtered_table <- DT::renderDataTable(
    DT::datatable(
      data_active(),
      extensions = "Buttons",
      filter = "top",
      # caption = "Use the boxes below column headers to filter data",
      style = "bootstrap",
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 3,
        autoWidth = FALSE,
        responsive = T,
        dom = "<'row'<'col-sm-1'f>><'row'<'col-sm-6'B>>rtlip",
        # dom = "<'row'<'col-sm-6'f><'col-sm-6'B>><'row'<'col-sm-6'l><'col-sm-6'>>prti",
        buttons = c("copy", "csv", "excel", "print"),
        columnDefs = list(list(
          targets = "_all",
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data != null && data.length > 30 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
            "}"
          )
        ))
      ),
      # ,
      class = "display"
    ),
    server = F
  )
 
  
  # Location Frequency Plot
  output$location_plot_selector <- renderUI({
    req(data_internal$raw)
    
    selectInput(
      inputId = "select_loc_col",
      label = "Plot the number of datasets by:",
      choices = c("", get_histogram_viable_columns(data_active())),
      selected = "nation_name"
    )
  })
  
  ## HEATMAP
  output$heatmap_selector <- renderUI({
    req(data_internal$raw)
    div(list(
      div(style = "display: inline-block; width = '10%'",
          br()),
      div(
        style = "display: inline-block; width = '40%'",
        title = "Select which categorical variable you wish to cross tabulate along the x axis in a heat map. Values must be discrete categories (i.e. not free text and not decimal)",
        selectInput(
          inputId = "heat_select_x",
          label = "Select X variable",
          choices = c("", get_histogram_viable_columns(data_active())),
          selected = "year",
        )
      ),
      div(
        style = "display: inline-block; width = '40%'",
        title = "Select which categorical variable you wish to cross tabulate along the y axis in a heat map. Values must be discrete categories (i.e. not free text and not decimal)",
        selectInput(
          inputId = "heat_select_y",
          label = "Select Y variable",
          choices = c("", get_histogram_viable_columns(data_active())),
          selected = "Region"
        )
      )
    ))
  })
  
  # geom_bar rather than geom_histogram so that non-continous variables can be plotted
  source("gendescplot.R")
  gen_location_trend_plot <- reactive({
    GenDescPlots(data_active(), input$select_loc_col)
  })
  
  output$plot2 <- renderPlotly({
    req(input$select_loc_col)
    gen_location_trend_plot()
  })

  
  gen_heatmap <- reactive({
    GenHeatMap(data_active(),
               c(input$heat_select_x, input$heat_select_y))
  })
  
  output$heatmap <- renderPlotly({
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
        grDevices::png(
          ...,
          width = width,
          height = width,
          res = 300,
          units = "in"
        )
      }
      ggsave(file, plot = gen_heatmap(), device = device)
    }
  )
  
  ##############################################################################################################
  
  ##### Data Atlas Tab
  generate_systematic_map <- reactive(sys_map(data_active()))
  
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
        custom_pal <- colorRampPalette(c("#e5f5e0", "#31a354"))
        circle_pal <-
          colorNumeric(palette = custom_pal(5),
                       domain = data_active()$total_of_country)

        lat_plotted <-
        as.numeric(unlist(data_active() %>%
                            dplyr::select(Latitude)))
      lng_plotted <-
        as.numeric(unlist(data_active() %>%
                            dplyr::select(Longitude)))

    
      lat_plotted[is.na(lat_plotted)] <- 0
      lng_plotted[is.na(lng_plotted)] <- -20

      tooltip_label <- reactive({
        print("OI")
        
      })
      
      # print(colnames(data_active()))
      
      output$map <- renderLeaflet({
        generate_systematic_map()  %>%
          addProviderTiles(providers$Esri.WorldTerrain) %>%
        leaflet::addPolygons(
            data = FDPDataAtlas::bounds,
            layerId = ~ ISO_A3,
            fillColor = ~ pal(ref_data_filtered()$value),
            color = "white",
            dashArray = "3",
            weight = 1,
            fillOpacity = 0.7,
            #label = sprintf("%s: %d",data_active()$nation_abbreviation, data_active()$total_of_country)
          ) %>%
          leaflet::addCircleMarkers(
            lat = ~ lat_plotted,
            lng = ~ lng_plotted,
            layerId = ~ nation_abbreviation,
            radius = 1.25 * data_active()$total_of_country,
            color = circle_pal(data_active()$total_of_country),
            stroke = FALSE,
            fillOpacity = 0.7,
            label = sprintf("%s: %d surveys",data_active()$nation_abbreviation, data_active()$total_of_country)
          ) %>%
          leaflet::addLabelOnlyMarkers(lat = ~ lat_plotted,
                              lng = ~ lng_plotted,
                              label = sprintf("%d",data_active()$total_of_country), 
                              labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T)
                              )
})
    
    # COLOR
    breaks <- quantile(ref_data_filtered()$value,
               probs = seq(0, 1, 0.25),
               na.rm = TRUE)
    breaks <- rev(breaks)
    navy_colors <- c("#E0E9FE", "#B8C9EE", "#8395B9", "#506489", "#18375F")
    pal <- colorBin(navy_colors, domain = ref_data_filtered()$value, bins = breaks)
    #pal <- colorBin("Reds", domain = ref_data_filtered()$value, bins = breaks)
  
    # Display info in sidebar
    output$country_info <- renderUI({
      if (is.null(clicked_ISO_A3())) {
        return(p("Click on a country to check the surveys available."))
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
        # label = sprintf("%s: %d",data_active()$nation_abbreviation, data_active()$total_of_country)
      ) %>%
      leaflet::addLegend(
        pal = pal,
        values = ~ ref_data_filtered()$value,
        opacity = 0.7,
        title = input$selected_variable,
        layerId = "ref_data"
      ) %>%
      leaflet::addCircleMarkers(
        lat = ~ lat_plotted,
        lng = ~ lng_plotted,
        layerId = ~ nation_abbreviation,
        radius = 1 * data_active()$total_of_country,
        color = circle_pal(data_active()$total_of_country),
        #  color = "blue",
        stroke = FALSE,
        fillOpacity = 0.7,
        label = sprintf("%s: %d surveys",data_active()$nation_abbreviation, data_active()$total_of_country)
      )%>%
      leaflet::addLabelOnlyMarkers(lat = ~ lat_plotted,
                                   lng = ~ lng_plotted,
                                   label = sprintf("%d",data_active()$total_of_country), 
                                   labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T)
      )
  })
  
  # end shiny server
})
