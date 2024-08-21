#' For many static plots using the plotly and purr libraries for a rate regression metrics.
#'
#' @param .data a data frame from df_rate_metric_values.rda
#'
#' @return many plots of rate regression metrics
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
utm_rate_interactive_regression_plots <- function(.data){

  df <- .data %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  df_list <- make_graph_list(.data = df)

  output_graph <- purrr::map(df_list, ~utm_rate_interactive_regression_plot(.data = df, .))
  names(output_graph) <- df_list

  return(output_graph)
}
