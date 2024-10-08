% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utm_rate_interactive_plot.R
\name{utm_rate_interactive_plot}
\alias{utm_rate_interactive_plot}
\title{utm_rate_interactive_plot () : For one interactive plot at-a-time using plotly, for rate metric}
\usage{
utm_rate_interactive_plot(.data, list_name)
}
\arguments{
\item{.data}{A data frame from df_rate_metric_values.rda}

\item{list_name}{a var in df_rate_metric_values.rda}
}
\value{
An interactive plot for rate
}
\description{
The function takes a data frame from df_rate_metric_values.rda, creates a plot using plot_rate_skeleton() function,
and increases interactivity with tooltips showing text and rate values.
}
