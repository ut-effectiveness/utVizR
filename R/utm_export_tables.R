#' Produce a folder with ,png files made from the gt table functions
#'
#' @param list_name the name made from either the ut_outcome_count_gt_tables or ut_outcome_rate_gt_tables functions
#'
#' @return a folder called tables filled with .png
#' @export
#'
#' @importFrom stringr str_glue
#' @importFrom purrr walk2
#' @importFrom gt gtsave
utm_export_tables <- function(list_name){
  table_paths <- stringr::str_glue("tables/{names(list_name)}.png")

  table_list <- list_name


  walk2(
    table_list,
    table_paths,
    \(data, path) gt::gtsave(data, path)
  )
}
