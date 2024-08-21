#'  For one interactive plot at-a-time using ggplot2 and plotly, for a rate regression metrics.
#'
#' @param .data a data frame cleaned up from df_rate_metric_values.rda
#' @param list_name a name found in df_rate_metric_values.rda
#' @export
#'
#' @return A plot that uses plot_rate_regression_skeleton as its base
#' @importFrom ggplot2 theme
#' @importFrom plotly ggplotly
utm_rate_interactive_regression_plot <- function(.data, list_name) {

  p <- plot_rate_regression_skeleton(.data, list_name)

  #p <- p + theme(legend.position = "none")

  print(
    plotly::ggplotly(p,
                     tooltip = c("outcome_rate_num", "year"),
                     dynamicTicks = FALSE) %>%
      plotly::layout(hovermode = "x unified")
  )
}
