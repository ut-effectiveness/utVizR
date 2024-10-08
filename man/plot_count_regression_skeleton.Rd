% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_count_regression_skeleton.R
\name{plot_count_regression_skeleton}
\alias{plot_count_regression_skeleton}
\title{plot_count_regression_skeleton() : skeleton function to create headcount regression plot for
"utMetrics Dashboard" shiny app.}
\usage{
plot_count_regression_skeleton(.data, list_name)
}
\arguments{
\item{.data}{a data frame cleaned up from df_count_metric_values.rda}

\item{list_name}{a name found in df_count_metric_values.rda}
}
\value{
a plot
}
\description{
This function is a skeleton for creating a plot to visualize headcount regression over time. It fits
a linear regression model to the provided data and plots the regression line along with the data
points. The plot also includes the predicted count for the next year, and labels for the residuals
and prediction.
}
