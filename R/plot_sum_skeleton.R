#' plot_sum_skeleton() : make the skeleton for all plots for sum metrics
#'
#' This function generate a plot with year on the x-axis and sum values on the y-axis, including text labels and a legend.
#'
#' @param .data a data frame cleaned up from df_sum_metric_values.rda
#' @param list_name a name found in df_sum_metric_values.rda
#'
#' @export
#' @return a plot
#' @importFrom stringr str_to_title str_remove
#' @importFrom scales unit_format
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_y_continuous labs theme_minimal
#' @importFrom ggrepel geom_text_repel
plot_sum_skeleton <- function(.data, list_name) {

  data_frame <- .data
  selected_data <- data_frame[data_frame$list_name == list_name, ]
  title <- selected_data$title_name[1]
  y_axis_label <- stringr::str_to_title(selected_data$metric_group[1])
  y_var <- as.numeric(selected_data$sum)
  x_axis_label <- "Year"
  x_var <- selected_data$year
  metric <- selected_data$graph_name
  Sum <- scales::comma(selected_data$sum)
  legend_name <- stringr::str_to_title(stringr::str_remove(selected_data$name_list[1], "by "))
  min <- min(selected_data$sum)
  max <- max(selected_data$sum)


  p <-  ggplot2::ggplot(
    selected_data,
    ggplot2::aes(
      x = x_var,
      y = y_var,
      group = metric,
      text = graph_name,
      sum = Sum)
  ) +
    ggplot2::geom_line(aes(color = graph_name), size = 1) +
    ggplot2::geom_point(aes(color = graph_name), size = 2, alpha = .3) +
    ggplot2::scale_y_continuous(limits = c(min, max), labels = scales::label_comma(1))+
    ggplot2::labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label,
      color = legend_name)+
    ggrepel::geom_text_repel(
      size=2,
      mapping = aes(label = Sum),
    )+
    ggplot2::theme_minimal()
}
