% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utm_count_interactive_table.R
\name{utm_count_interactive_table}
\alias{utm_count_interactive_table}
\title{utm_count_interactive_table() : Modify data frame in order to make it compatible for 'utm_count_shiny_table()
function.'}
\usage{
utm_count_interactive_table(.data, list_name)
}
\arguments{
\item{.data}{a data frame made from df_count_metric_values.rda}

\item{list_name}{a distinct var in df_count_metric_values.rda}
}
\value{
Modified data frame
}
\description{
This function achives the following:
It renames the column "graph_name" to category,
Filters data_frame so that it matches the provided "list_name" and then selects "title_name", "year", "count", "category"
It extracts the Title from the first row of "title_name" column, removes the the column and modifies the Title
}
