#' utm_sum_static_plot() : One static plot at-a-time using ggplot2, for a sum metrics.
#'
#' This function adjusts legend position based on the plot title format and prints the plot
#'
#' @param .data a data frame cleaned up from df_sum_metric_values.rda
#' @param list_name a name found in df_sum_metric_values.rda
#' @export
#'
#' @return A plot that uses plot_sum_skeleton as its base
#' @importFrom stringr str_detect
#' @importFrom ggplot2 theme
#'
utm_sum_static_plot <- function(.data, list_name) {

  data_frame <- .data
  selected_data <- data_frame[data_frame$list_name == list_name, ]
  title <- selected_data$title_name[1]

  p <- plot_sum_skeleton(.data, list_name)

  if(stringr::str_detect(title, ":") == TRUE){
    p <- p + theme(legend.position = "none")
    print(p)
  } else {

    p <- p + theme(legend.position = "bottom") # legend.direction = "vertical")
    print(p)
  }
}
