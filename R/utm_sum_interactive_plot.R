#' utm_sum_interactive_plot () : For one interactive plot at-a-time using plotly, for sum metric
#'
#' The function takes a data frame from df_sum_metric_values.rda, creates a plot using plot_sum_skeleton() function,
#' and increases interactivity with tooltips showing text and sum values.
#'
#' @param .data A data frame from df_sum_metric_values.rda
#' @param list_name a var in df_sum_metric_values.rda
#' @export
#'
#' @return An interactive plot for sum
#'
#' @importFrom ggplot2 theme
#' @importFrom plotly ggplotly layout
#' @importFrom magrittr %>%
utm_sum_interactive_plot <- function(.data, list_name) {

  p <- plot_sum_skeleton(.data, list_name)

  p  <- p + ggplot2::theme(legend.position = "none")

  print(
    plotly::ggplotly(p,
                     tooltip = c("text", "sum"), dynamicTicks = FALSE) %>%
      plotly::layout(hovermode = "x unified")
  )

}
