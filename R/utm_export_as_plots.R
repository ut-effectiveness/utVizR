#' Produce a folder with .png files made from the plot functions
#'
#' @param list_name the name made from either the plot_graph_for_rate or plot_graph_for_count functions
#'
#' @return A folder with .png files
#' @export
#'
#' @importFrom stringr str_glue
#' @importFrom purrr walk2
#' @importFrom ggplot2 ggsave
utm_export_as_plots <- function(list_name){

  graph_paths <- stringr::str_glue("graphs/{names(list_name)}.png")

  graphs_list <- list_name

  purrr::walk2(
    graph_paths,
    graphs_list,
    \(path, plot) ggplot2::ggsave(path, plot, width = 10, height = 10)
  )

}
