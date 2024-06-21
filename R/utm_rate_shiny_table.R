#'  utm_rate_shiny_table() : Create retention table for "utMetrics Dashboard" shiny app.
#'
#'  This function effectively shows the retention table in the utMetrics dashboard. It takes filtered data frame
#'  as input, creates title and used the function utm_rate_interactive_table()' to produce the table. For one
#'  table at-a-time using DT library.
#'
#' @param .data A data frame from df_rate_metric_values.rda
#' @export
#'
#' @return A table for rate. It consists of three columns: Year, Outcome Rate and Category.
#'
#' @importFrom ggplot2 theme
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom stringr str_to_title str_squish
utm_rate_shiny_table <- function(.data) {

  step_1 <- .data %>%
    select(choice) %>%
    unique()

  step_2 <- as.list(step_1)

  step_3 <- .data %>%
    dplyr::mutate(choices = purrr::map(step_2, function(x) paste0(x, collapse = ':'))) %>%
    dplyr::mutate(new_title_name = paste(metric_df_name, name_list, sep = " ")) %>%
    dplyr::mutate(new_title_name = stringr::str_to_title(new_title_name)) %>%
    dplyr::mutate(new_title_name = stringr::str_squish(new_title_name)) %>%
    dplyr::mutate(new_title_name = paste0(new_title_name, ":", choices)) %>%
    dplyr::mutate(title_name = new_title_name) %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  new_table <- utVizR::utm_rate_interactive_table(.data =step_3, list_name =step_3$list_name)

  return(new_table)

}
