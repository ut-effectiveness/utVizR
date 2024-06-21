#' utm_rate_interactive_plot () : For one interactive plot at-a-time using plotly, for rate metric
#'
#' The function takes a data frame from df_rate_metric_values.rda, creates a plot using plot_rate_skeleton() function,
#' and increases interactivity with tooltips showing text and rate values.
#'
#' @param .data A data frame from df_rate_metric_values.rda
#' @param list_name a var in df_rate_metric_values.rda
#' @return An interactive plot for rate
#' @export
#' @importFrom ggplot2 theme
#' @importFrom plotly ggplotly layout
#' @importFrom magrittr %>%
utm_rate_interactive_plot <- function(.data, list_name) {

  p <- plot_rate_skeleton(.data, list_name)

  p  <- p + ggplot2::theme(legend.position = "none")

  print(
    plotly::ggplotly(p,
      tooltip = c("text", "rate"), dynamicTicks = FALSE) %>%
      plotly::layout(hovermode = "x unified")
  )

}
