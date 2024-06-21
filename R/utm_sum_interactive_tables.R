#' utm_sum_interactive_tables () : For many interactive table using the DT library and purrr for a sum metrics.
#'
#' This function converts title names to snake case, creates a list of interactive tables, and returns the tables
#' named by their list names.
#'
#' @param .data a data frame from df_sum_metric_values.rda
#'
#' @return many DT tables of sum metrics
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
#' @importFrom magrittr %>%
utm_sum_interactive_tables <- function(.data){

  df <- .data %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  df_list <- make_graph_list(.data = df)

  output_graph <- purrr::map(df_list, ~utm_sum_interactive_table(.data = df, .))
  names(output_graph) <- df_list

  return(output_graph)
}
