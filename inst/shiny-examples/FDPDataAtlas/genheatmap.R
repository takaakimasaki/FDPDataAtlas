GenHeatMap = function(idata, selcols, axis_txt_lim = 60){
  listone<-listtwo<-n<-NULL
  # Convert columns to factors to allow for categorical classification for both numeric and character data
  tmp <- as.data.frame(sapply(idata[selcols], function(x) as.factor(x)))
  # Plot Heatmap
  heatmp <- tmp %>%
    dplyr::rename(listone=colnames(tmp[1]), listtwo=colnames(tmp[2])) %>%
    dplyr::count(listone, listtwo) %>%
    tidyr::complete(listone, listtwo, fill = list(n = 0)) %>%
    dplyr::mutate(listtwo = forcats::fct_rev(forcats::fct_inorder(listtwo)),  # Sort listtwo in reverse alphabetical order
                  tooltip_text = paste(selcols[1], ':', listone, '<br>', selcols[2], ':', listtwo, '<br>Count:', n)) %>%
    ggplot(aes(x = listone, y = listtwo, fill = n, text = tooltip_text)) +  # Add the tooltip_text aesthetic here
    geom_tile(aes(alpha = 0.3), color="grey60") +
    geom_text(aes(label = n), vjust = 1) +  # Display only the count on the heatmap
    scale_fill_gradientn(colors = c("#ecf0f5", "#0072BC")) +
    xlab(paste0(selcols[1])) +
    ylab(paste0(selcols[2])) +  
    labs(fill = "Count") +
    scale_x_discrete(labels = function(x) substr(x, 1, axis_txt_lim)) +
    scale_y_discrete(labels = function(x) substr(x, 1, axis_txt_lim)) +
    ggtitle("Heatmap") +
    theme_unhcr(grid="N") +
    theme(axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12))
  
  # Convert ggplot object to plotly object and specify the tooltip
  interactive_plot <- ggplotly(heatmp, tooltip = "text")  # Use "text" because that's the aesthetic we specified for tooltip_text
  
  interactive_plot <- interactive_plot %>% plotly::layout(paper_bgcolor='#ecf0f5', plot_bgcolor='#ecf0f5')
  
  
}
