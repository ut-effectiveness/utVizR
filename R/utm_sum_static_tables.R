#' utm_sum_static_tables() : Generate static rate tables
#'
#' This function generates static sum(fte) tables for the utMetrics package. It prepares the data frame by converting title
#' names to snake case and then creates a list of graphs using the make_graph_list function. Finally, it generates static
#' sum tables for each graph in the list using the utm_sum_static_table function.
#'
#'
#' @param .data a data frame from df_sum_metric_values.rda
#'
#' @return many gt tables of sum metrics
#' @export
#'
#'
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
#' @importFrom magrittr %>%
utm_sum_static_tables <- function(.data){

  df <- .data %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  df_list <- make_graph_list(.data = df)

  output_graph <- purrr::map(df_list, ~utm_sum_static_table(.data = df, .))
  names(output_graph) <- df_list

  print(output_graph)
}
