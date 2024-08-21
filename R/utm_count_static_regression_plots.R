#' For many static plots using the ggplot2 and purr libraries for a count regression metrics.
#'
#' @param .data a data frame from df_count_metric_values.rda
#'
#' @return many plots of count regression metrics
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
utm_count_static_regression_plots <- function(.data){

  df <- .data %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  df_list <- make_graph_list(.data = df)

  output_graph <- purrr::map(df_list, ~utm_count_static_regression_plot(.data = df, .))
  names(output_graph) <- df_list

  return(output_graph)
}
