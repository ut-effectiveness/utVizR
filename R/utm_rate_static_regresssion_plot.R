#'  utm_rate_regression_plot () : Creates a static Regression Plot
#'
#'  This function generates a regression plot by using  "plot_rate_regression_skeleton()" function filtered by a
#'  specified list name, and removes the legend for a cleaner presentation.
#'
#'
#' @param .data a data frame cleaned up from df_rate_metric_values.rda
#' @param list_name a name found in df_rate_metric_values.rda
#'
#' @return A plot that uses plot_rate_regression_skeleton as its base
#' @export
#' @importFrom ggplot2 theme
utm_rate_regression_plot <- function(.data, list_name) {

  p <- plot_rate_regression_skeleton(.data, list_name)

  p <- p + theme(legend.position = "none")

  print(p)
}
