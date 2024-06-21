#' utm_sum_interactive_table() : Modify data frame in order to make it compatible for 'utm_sum_shiny_table()
#' function.'
#'
#' This function achives the following:
#'  It renames the column "graph_name" to category,
#'  Filters data_frame so that it matches the provided "list_name" and then selects "title_name", "year", "sum", "category"
#'  It extracts the Title from the first row of "title_name" column, removes the the column and modifies the Title
#'
#'
#' @param .data a data frame made from df_sum_metric_values.rda
#' @param list_name a distinct var in df_sum_metric_values.rda
#' @export
#'
#' @return Modified data frame
#'
#' @importFrom dplyr rename select
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_title
#' @importFrom DT datatable
utm_sum_interactive_table <- function(.data, list_name) {

  data_frame <- .data %>%
    rename(category = graph_name)

  selected_data <- data_frame[data_frame$list_name == list_name, c("title_name", "year", "sum", "category")]

  title <- selected_data$title_name[1]

  selected_data2 <- selected_data %>%
    dplyr::select(!title_name)

  colnames(selected_data2) <- stringr::str_to_title(colnames(selected_data2))

  # table <- DT::datatable(
  #   selected_data2,
  #   filter = "top",
  #   rownames = FALSE,
  #   caption = title,
  #   options = list(
  #     pageLength = 10,
  #     scrollX = TRUE
  #   )
  #
  # )

  return(selected_data2)
}
