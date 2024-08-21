#' plot_rate_regression_skeleton() : skeleton function to create retention regression plot for
#' "utMetrics Dashboard" shiny app.
#'
#' This function is a skeleton for creating a plot to visualize retention regression over time. It fits
#' a linear regression model to the provided data and plots the regression line along with the data
#' points. The plot also includes the predicted rate for the next year, and labels for the residuals
#' and prediction.
#'
#'
#' @param .data a data frame cleaned up from df_rate_metric_values.rda
#' @param list_name a name list found in df_rate_metric_values.rda
#' @return a plot
#' @export
#' @importFrom stringr str_to_title str_remove
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom scales unit_format
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_y_continuous labs theme_minimal
#' @importFrom stats lm residuals predict
#' @importFrom ggrepel geom_text_repel
plot_rate_regression_skeleton <- function(.data, list_name) {
  data_frame <- .data
  selected_data <- data_frame[data_frame$list_name == list_name,] %>%
    dplyr::mutate(year = as.numeric(year))

  title <- selected_data$title_name[1]
  y_axis_name <- stringr::str_to_title(selected_data$metric_group[1])
  x_axis_name <- "year"
  legend_name <- stringr::str_to_title(stringr::str_remove(selected_data$name_list[1], "by "))
  Rate <- selected_data$outcome_rate

  # Fitting linear regression model
  lm_model <- stats::lm(outcome_rate_num ~ year, data = selected_data)

  # Calculate residuals
  residuals <- stats::residuals(lm_model)

  # Calculate the predicted value for the next year
  prediction_year <- max(selected_data$year) + 1
  prediction_value <- stats::predict(lm_model, newdata = data.frame(year = prediction_year))

  # Create a dataframe for the prediction point label
  prediction_label <- data.frame(year = prediction_year, outcome_rate_num = prediction_value)

  min <- max(c(0, min(selected_data$outcome_rate_num) - (.30 * min(selected_data$outcome_rate_num))))
  max <- min(c(100, max(selected_data$outcome_rate_num) + (.30 * max(selected_data$outcome_rate_num))))

  p <- ggplot2::ggplot(
    selected_data,
    ggplot2::aes(x = year,
      y = outcome_rate_num,
      color = graph_name
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(
      method = "lm",
      se = FALSE,
      color = "#003058"
    ) +
    ggrepel::geom_text_repel(
      aes(x = year, y = outcome_rate_num, label = Rate),
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
      ggplot2::aes(y = outcome_rate_num),
      color = "#003058",
      size = 3
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = max(year),
        y = stats::predict(lm_model, newdata = data.frame(year = max(year))),
        xend = max(year) + 1,
        yend = stats::predict(lm_model, newdata = data.frame(year = max(year) + 1))
      ),
      color = "#003058",
      linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = year,
        xend = year,
        y = outcome_rate_num,
        yend = outcome_rate_num - residuals
      ),
      color = "#003058",
      alpha = 0.5
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(
        x = year,
        y = outcome_rate_num - residuals,
        label = round(residuals, 2)
      ),
      color = "#003058",
      # vjust = -0.5,
      # hjust = 0.5,
      size = 3
    ) +
    #Label for the prediction point
    ggplot2::geom_text(
      data = prediction_label,
      ggplot2::aes(
        x = year,
        y = outcome_rate_num,
        label = round(outcome_rate_num, 2)
      ),
      color = "#003058",
      vjust = -0.5,
      hjust = 0.8,
      size = 3
    )  +
    ggplot2::scale_y_continuous(limits = c(min, max), labels = scales::unit_format(unit = "%", scale = 1, sep = "")) +
    ggplot2::labs(
      title = title,
      x = x_axis_name,
      y = y_axis_name,
      color = legend_name
    )+
    ggplot2::theme_minimal()

  return(p)
}
