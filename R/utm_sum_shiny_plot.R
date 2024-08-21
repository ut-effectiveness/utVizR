#' utm_sum_shiny_plot() : For one interactive plot at-a-time using plotly, for shiny dashboard
#'
#' This function creates of one interactive plot at a time using Plotly for sum metric. It takes a data frame from
#' df_sum_metric_values.rda, processes it to generate an interactive plot, and returns the plot for shiny dashboard.
#'
#' @param .data A data frame from df_sum_metric_values.rda
#' @export
#'
#' @return An interactive plot for sum
#' @importFrom ggplot2 theme
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom stringr str_to_title str_squish
utm_sum_shiny_plot <- function(.data) {

  step_1 <- .data %>%
    select(choice) %>%
    unique()

  step_2 <- as.list(step_1)

  step_3 <- .data %>%
    dplyr::mutate(choices = purrr::map(step_2, function(x) paste0(x, collapse = ':'))) %>%
    dplyr::mutate(new_title_name = paste(season, metric_df_name, name_list, sep = " ")) %>%
    dplyr::mutate(new_title_name = stringr::str_to_title(new_title_name)) %>%
    dplyr::mutate(new_title_name = stringr::str_squish(new_title_name)) %>%
    dplyr::mutate(new_title_name = paste0(new_title_name, ":", choices)) %>%
    dplyr::mutate(title_name = new_title_name) %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  new_plot <- utVizR::utm_sum_interactive_plot(.data =step_3, list_name =step_3$list_name)

  return(new_plot)

}
