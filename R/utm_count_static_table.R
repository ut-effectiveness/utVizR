#' For one static table at-a-time using the DT library.
#'
#' @param .data a data frame made from df_count_metric_values.rda
#' @param list_name a distinct var in df_count_metric_values.rda
#' @return A gt table
#' @importFrom gt tab_header
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_title
#' @importFrom dplyr rename select
utm_count_static_table <- function(.data, list_name){

  data_frame <- .data %>%
    dplyr::rename(category = graph_name)

  selected_data <- data_frame[data_frame$list_name == list_name, c("title_name", "year", "count", "category")]
  title <- selected_data$title_name[1]

  selected_data2 <- selected_data %>%
    dplyr::select(!title_name)

  colnames(selected_data2) <- stringr::str_to_title(colnames(selected_data2))

  tbl <- selected_data2 %>%
    gt::gt() %>%
    gt::tab_header(
      title = title
    )

  return(tbl)
}
