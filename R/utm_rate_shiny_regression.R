#'  utm_rate_shiny_regression() : Modify retention regression plot for "utMetrics Dashboard" shiny app.
#'
#'  This function effectively shows the retention regression plot in the utMetrics dashboard. It takes filtered data frame
#'  as input, creates title and used the function 'utm_rate_regression_plot()' to produce the plot. For one
#'  plot at-a-time.
#'
#' @param .data A data frame from df_rate_metric_values.rda
#' @export
#'
#' @return An regression plot for rate
#' @importFrom ggplot2 theme
#' @importFrom dplyr mutate
#' @importFrom snakecase to_snake_case
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_title str_squish
utm_rate_shiny_regression <- function(.data) {


  step_3 <- .data %>%
    dplyr::mutate(new_title_name = paste(metric_df_name, name_list, sep = " ")) %>%
    dplyr::mutate(new_title_name = stringr::str_to_title(new_title_name)) %>%
    dplyr::mutate(new_title_name = stringr::str_squish(new_title_name)) %>%
    dplyr::mutate(new_title_name = paste0(new_title_name, ":", choice)) %>%
    dplyr::mutate(title_name = new_title_name) %>%
    dplyr::mutate(list_name = snakecase::to_snake_case(title_name))

  new_plot <- utVizR::utm_rate_regression_plot(.data =step_3, list_name =step_3$list_name)

  return(new_plot)

}
