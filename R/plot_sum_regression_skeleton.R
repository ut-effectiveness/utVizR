#' plot_sum_regression_skeleton() : skeleton function to create sum (fte) regression plot for
#' "utMetrics Dashboard" shiny app
#'
#' This function is a skeleton for creating a plot to visualize sum regression over time. It fits
#' a linear regression model to the provided data and plots the regression line along with the data
#' points. The plot also includes the predicted count for the next year, and labels for the residuals
#' and prediction.
#'
#' @param .data a data frame cleaned up from df_sum_metric_values.rda
#' @param list_name a name list found in df_sum_metric_values.rda
#'
#' @return a plot
#' @export
#'
#' @importFrom stringr str_to_title str_remove
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom scales unit_format
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_y_continuous labs theme_minimal
#' @importFrom stats lm residuals predict
#' @importFrom ggrepel geom_text_repel
plot_sum_regression_skeleton <- function(.data, list_name) {
  data_frame <- .data
  selected_data <- data_frame[data_frame$list_name == list_name,] %>%
    mutate(year = as.numeric(year))

  title <- selected_data$title_name[1]
  y_axis_name <- selected_data$metric_group_graph[1]
  x_axis_name <- "Year"
  legend_name <- stringr::str_to_title(stringr::str_remove(selected_data$name_list[1], "by "))
  FTE <- scales::comma(selected_data$sum)


  # Fitting linear regression model
  lm_model <- lm(sum ~ year, data = selected_data)

  # Calculate residuals
  residuals <- residuals(lm_model)

  # Calculate the predicted value for the next year
  prediction_year <- max(selected_data$year) + 1
  prediction_value <- predict(lm_model, newdata = data.frame(year = prediction_year))

  # Create a dataframe for the prediction point label
  prediction_label <- data.frame(year = prediction_year, sum = prediction_value)


  min <- max(c(0, min(selected_data$sum) - (.20 * min(selected_data$sum))))
  max <- (max(selected_data$sum) + (.20 * max(selected_data$sum)))

  p <- ggplot(
    selected_data,
    aes(x = year,
        y = sum,
        color = graph_name)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(
      method = "lm",
      se = FALSE,
      color = "#003058"
    ) +
    ggrepel::geom_text_repel(
      aes(x = year, y = sum, label = FTE),
      color = "#BA1C21",
      box.padding = 1,
      point.padding = 0.5,
      size = 3,
      segment.color = "#BA1C21",
      segment.size = 0.5
    ) +
    # Predict for next year
    ggplot2::geom_point(
      data = prediction_label,
      aes(y = sum),
      color = "#003058",
      size = 3
    ) +
    ggplot2::geom_segment(
      aes(
        x = max(year),
        y = predict(lm_model, newdata = data.frame(year = max(year))),
        xend = max(year) + 1,
        yend = predict(lm_model, newdata = data.frame(year = max(year) + 1))
      ),
      color = "#003058",
      linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = year,
        xend = year,
        y = sum,
        yend = sum - residuals
      ),
      color = "#003058",
      alpha = 0.5
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(
        x = year,
        y = sum - residuals,
        label = round(residuals, 2)
      ),
      color = "#003058",
      size = 3
    ) +
    ggplot2::geom_text(
      data = prediction_label,
      ggplot2::aes(
        x = year,
        y = sum,
        label = round(sum, 2)
      ),
      color = "#003058",
      vjust = -0.5,
      hjust = 0.8,
      size = 3
    )  +
    ggplot2::scale_y_continuous(limits = c(min, max), labels = scales::label_comma(1))+
    ggplot2::labs(
      title = title,
      x = x_axis_name,
      y = y_axis_name,
      color = legend_name
    ) +
    ggplot2::theme_minimal()
  p <- p + theme(legend.position = "none")

  print(p)
}
