#'  utm_count_static_regression_plot() : Modify headcount regression plot for "utMetrics Dashboard" shiny app.
#'
#'  This function utilizes the plot_count_regression_skeleton function to create the plot and then removes the legend
#'  from the plot.
#'
#' @param .data a data frame cleaned up from df_count_metric_values.rda
#' @param list_name a name found in df_count_metric_values.rda
#'
#' @return A plot that uses plot_count_regression_skeleton as its base
#' @export
#' @importFrom ggplot2 theme
utm_count_static_regression_plot <- function(.data, list_name) {

  p <- plot_count_regression_skeleton(.data, list_name)

  p <- p + theme(legend.position = "none")

  print(p)
}
