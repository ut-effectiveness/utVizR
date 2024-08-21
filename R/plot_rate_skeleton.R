#' make the skeleton for all plots for rate metrics
#'
#' @param .data a data frame cleaned up from df_rate_metric_values.rda
#' @param list_name a name found in df_rate_metric_values.rda
#'
#' @return a plot
#' @export
#' @importFrom stringr str_to_title str_remove
#' @importFrom scales unit_format
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_y_continuous labs theme_minimal
#' @importFrom ggrepel geom_text_repel
plot_rate_skeleton <- function(.data, list_name) {

  data_frame <- .data
  selected_data <- data_frame[data_frame$list_name == list_name, ]
  title <- selected_data$title_name[1]
  y_axis_label <- stringr::str_to_title(selected_data$metric_group[1])
  y_var <- as.numeric(selected_data$outcome_rate_num)
  x_axis_label <- "Year"
  x_var <- selected_data$year
  metric <- selected_data$graph_name
  Rate <- selected_data$outcome_rate
  legend_name <- stringr::str_to_title(stringr::str_remove(selected_data$name_list[1], "by "))
  min <- max(c(0, min(selected_data$outcome_rate_num) - (.20 * min(selected_data$outcome_rate_num))))
  max <- min(c(100, max(selected_data$outcome_rate_num) + (.20 * max(selected_data$outcome_rate_num))))


  p <-  ggplot2::ggplot(
    selected_data,
    ggplot2::aes(
      x = x_var,
      y = y_var,
      group = metric,
      text = graph_name,
      rate = Rate)
  ) +
    ggplot2::geom_line(aes(color = metric), size = 1) +
    ggplot2::geom_point(aes(color = metric), size = 2, alpha = .3) +
    ggplot2::scale_y_continuous(limits = c(min, max), labels = scales::unit_format(unit = "%", scale = 1, sep = "")) +
    ggplot2::labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label,
      color = legend_name)+
    ggrepel::geom_text_repel(
      size=4,
      mapping = aes(label = Rate),
    )+
    ggplot2::theme_minimal()
}
