#' utm_sum_interacitve_plots() : Generate interactive sum plots using the plotly and purr libraries
#'
#' This function creates multiple interactive plots for sum metrics using utm_sum_interactive_plot().
#' It processes data frames from df_sum_metric_values.rda.
#'
#' @param .data a data frame from df_sum_metric_values.rda
#'
#' @return many plotly graphs of sum metrics
#' @export
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
#' @importFrom magrittr %>%
utm_sum_interacitve_plots <- function(.data){

  df <- .data %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  df_list <- make_graph_list(.data = df)

  output_graph <- purrr::map(df_list, ~utm_sum_interactive_plot(.data = df, .))
  names(output_graph) <- df_list

  return(output_graph)
}
