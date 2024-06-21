#' For many static tables using the gt and purrr libraries for a count metrics.
#'
#' @param .data a data frame from df_count_metric_values.rda
#'
#' @return many gt tables of count metrics
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
#' @importFrom magrittr %>%
utm_count_static_tables <- function(.data){

  df <- .data %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  df_list <- make_graph_list(.data = df)

  output_graph <- purrr::map(df_list, ~utm_count_static_table(.data = df, .))
  names(output_graph) <- df_list

  print(output_graph)
}
