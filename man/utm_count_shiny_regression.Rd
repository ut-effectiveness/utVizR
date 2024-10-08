% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utm_count_shiny_regression.R
\name{utm_count_shiny_regression}
\alias{utm_count_shiny_regression}
\title{utm_count_shiny_regression() : Modify headcount regression plot for "utMetrics Dashboard" shiny app.}
\usage{
utm_count_shiny_regression(.data)
}
\arguments{
\item{.data}{A data frame from df_count_rate_values.rda}
}
\value{
An regression plot for count
}
\description{
This function shows the headcount regression plot in the utMetrics dashboard. It takes filtered data frame
as input, creates title and use the function 'utm_count_static_regression_plot()' to produce the plot. For one
plot at-a-time.
}
