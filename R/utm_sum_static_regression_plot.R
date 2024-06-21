#'  utm_sum_static_regression_plot() : Modify sum regression plot for "utMetrics Dashboard" shiny app.
#'
#'  This function utilizes the "plot_sum_regression_skeleton()" function to create the plot and then removes the legend
#'  from the plot.
#'
#' @param .data a data frame cleaned up from df_sum_metric_values.rda
#' @param list_name a name found in df_sum_metric_values.rda
#'
#' @return A plot that uses plot_sum_regression_skeleton as its base
#' @export
#' @importFrom ggplot2 theme
utm_sum_static_regression_plot <- function(.data, list_name) {

  p <- plot_sum_regression_skeleton(.data, list_name)

  p <- p + theme(legend.position = "none")

  print(p)
}
