#' utm_count_static_plots() : For many static plots using the plotly and purr libraries for a count metrics.
#'
#' This function converts title names to snake case, creates a list of plots, and returns the plots using
#' utm_count_static_plot() function.
#'
#' @param .data a data frame from df_count_metric_values.rda
#'
#' @return plots of count metrics
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
utm_count_static_plots <- function(.data){

  df <- .data %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  df_list <- make_graph_list(.data = df)

  output_graph <- purrr::map(df_list, ~utm_count_static_plot(.data = df, .))
  names(output_graph) <- df_list

  return(output_graph)
}
