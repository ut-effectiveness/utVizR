#' utm_sum_static_table() : Create a Formatted GT Table
#'
#' This function generates a formatted GT table from a given data frame, filtering by a specified list name and
#' renaming columns for better readability.
#'
#' @param .data a data frame made from df_sum_metric_values.rda
#' @param list_name a distinct var in df_sum_metric_values.rda
#' @return A gt table
#' @importFrom gt tab_header
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_title
#' @importFrom dplyr rename select
utm_sum_static_table <- function(.data, list_name){

  data_frame <- .data %>%
    dplyr::rename(category = graph_name)

  selected_data <- data_frame[data_frame$list_name == list_name, c("title_name", "year", "sum", "category")]
  title <- selected_data$title_name[1]

  selected_data2 <- selected_data %>%
    dplyr::select(!title_name)

  colnames(selected_data2) <- stringr::str_replace_all(colnames(selected_data2), "_", " ")
  colnames(selected_data2) <- stringr::str_to_title(colnames(selected_data2))


  tbl <- selected_data2 %>%
    gt::gt() %>%
    gt::tab_header(
      title = title
    )

  return(tbl)
}
