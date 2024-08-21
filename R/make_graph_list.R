#' A way to make a list for further use in other helper functions
#'
#' @param .data a data frame made in another helper function
#'
#' @return a list of the variable list_names
#' @export
#' @importFrom dplyr select
#' @importFrom magrittr %>%
make_graph_list <- function(.data){

  datapoints_for_list <- .data %>%
    dplyr::select(list_name) %>%
    unique()

  metrics_list <- as.list(datapoints_for_list$list_name)

  return(metrics_list)

}
