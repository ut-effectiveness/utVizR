#'  utm_count_interactive_plot () : For one interactive plot at-a-time using plotly, for count metric
#'
#' The function takes a data frame from df_count_metric_values.rda, creates a plot using plot_count_skeleton() function,
#' and increases interactivity with tooltips showing text and count values.
#'
#' @param .data A data frame from df_count_metric_values.rda
#' @param list_name a var in df_count_metric_values.rda
#' @export
#'
#' @return An interactive plot for count
#' @importFrom ggplot2 theme
#' @importFrom plotly ggplotly layout
#' @importFrom magrittr %>%
utm_count_interactive_plot <- function(.data, list_name) {

  p <- plot_count_skeleton(.data, list_name)

  p  <- p + ggplot2::theme(legend.position = "none")

  print(
    plotly::ggplotly(p,
      tooltip = c("text", "count"), dynamicTicks = FALSE) %>%
      plotly::layout(hovermode = "x unified")
  )

}
