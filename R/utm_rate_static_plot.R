#'  utm_rate_static_plot() : Generate static rate plot
#'
#'  This function produces a static plot for a specified rate metric based on the provided dataset and list name.
#'  It filters the dataset to select data associated with the provided list name and extracts the title for the plot.
#'  The plot is generated using "plot_rate_skeleton()" as its base, with legend customization based on the presence of
#'  a colon in the title.
#'
#' @param .data a data frame cleaned up from df_rate_metric_values.rda
#' @param list_name a name found in df_rate_metric_values.rda
#'
#' @return A plot that uses plot_rate_skeleton as its base
#' @importFrom stringr str_detect
#' @importFrom ggplot2 theme
utm_rate_static_plot <- function(.data, list_name) {

  data_frame <- .data
  selected_data <- data_frame[data_frame$list_name == list_name, ]
  title <- selected_data$title_name[1]

  p <- plot_rate_skeleton(.data, list_name)

  if(stringr::str_detect(title, ":") == TRUE){
    p <- p + ggplot2::theme(legend.position = "none")
    print(p)
  } else{

    p <- p + ggplot2::theme(legend.position = "bottom")
    print(p)
  }
}
