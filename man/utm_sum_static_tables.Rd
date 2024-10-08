% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utm_sum_static_tables.R
\name{utm_sum_static_tables}
\alias{utm_sum_static_tables}
\title{utm_sum_static_tables() : Generate static rate tables}
\usage{
utm_sum_static_tables(.data)
}
\arguments{
\item{.data}{a data frame from df_sum_metric_values.rda}
}
\value{
many gt tables of sum metrics
}
\description{
This function generates static sum(fte) tables for the utMetrics package. It prepares the data frame by converting title
names to snake case and then creates a list of graphs using the make_graph_list function. Finally, it generates static
sum tables for each graph in the list using the utm_sum_static_table function.
}
