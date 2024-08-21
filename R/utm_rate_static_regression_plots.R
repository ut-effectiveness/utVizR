#' utm_rate_static_regression_plots () : Creates static Regression Plots
#'
#' This function generates multiple static regression plots by transforming the data and applying
#' the `utm_rate_static_regression_plot()` function to each subset of data, identified by unique list names.
#'
#' @param .data a data frame from df_rate_metric_values.rda
#'
#' @return many plots of rate regression metrics
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
utm_rate_static_regression_plots <- function(.data){

  df <- .data %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  df_list <- make_graph_list(.data = df)

  output_graph <- purrr::map(df_list, ~utm_rate_static_regression_plot(.data = df, .))
  names(output_graph) <- df_list

  return(output_graph)
}
