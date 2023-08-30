#' Create bar plot with distribution of studies over the region from lat/long info
#'
#' Created For ES Hackathon 2018
#' @param df Input dataframe
#' @param location_column Column with location information (preferably country-level or higher)
#' @param axis_txt_lim Numeric limit of number of characters in labels
#' @return Returns a bar plot object showing counts of literature in systematic review for each location
#'
#' @author Sanita Dhaubanjar
#'
#' @keywords SystematicReview
#'
#' @export

GenDescPlots = function(df, location_column, axis_txt_lim = 15){
  # Count per locations --------
  location_counts <- as.data.frame(table(df[location_column])) # table() tabulates frequency
  colnames(location_counts)<-c(location_column, "n")
  
  # Sort the data by 'n'
  location_counts <- location_counts[order(-location_counts$n), ]
  
  # Reorder factor levels for sorting bars
  location_counts[[location_column]] <- factor(location_counts[[location_column]], levels = rev(location_counts[[location_column]]))
  
  # Get max value
  max_val <- max(location_counts[[colnames(location_counts[2])]])
  
  # Plot bar chart
  locmp <- ggplot2::ggplot(location_counts, 
                           aes_string(x=colnames(location_counts[2]),
                                      y=colnames(location_counts[1]),
                                      label = colnames(location_counts[2]))) +
    ggplot2::geom_bar(stat="identity", fill="light green", width=0.8) + 
    ggplot2::scale_x_continuous(limits = c(0, max_val * 1.05), expand = c(0, 0)) +
    ggplot2::scale_y_discrete(labels = function(y) {
      ifelse(nchar(y) > axis_txt_lim, 
             paste0(substr(y, 1, axis_txt_lim), "..."), 
             y) # Trim the label if it's too long
    }) +
    ggplot2::geom_text(aes(), size = 4, nudge_x = 0.5) +
    ggplot2::labs(x="# Studies", 
                  y="", 
                  title = paste(location_column, "frequency")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.line = element_line(colour = "black"),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      # plot.margin = margin(20, 20, 20, 20) # Add margins
    )
  
  # Rotate x-axis label if too many
  if (nrow(location_counts) > 5){
    locmp <- locmp + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 0.95, size = 11))
  }
  
  return(locmp)
}
