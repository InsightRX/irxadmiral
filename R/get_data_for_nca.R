#' Get the data needed for model fitting and parse into format for easy parsing
#' with NCA tool.
#' 
#' @param db list with db connection information. If NULL, will try to read
#' same data from xpt files in `path`
#' @param path optional. If no `db` specified, will read tables from xpt
#' files in folder `path`.
#' 
#' @returns data.frame
#'
#' @export
#' 
get_data_for_nca <- function(
    db = NULL,
    path = NULL
) {
  
  data <- read_data(
    tables = c("pc", "dm", "ex"), 
    db = db, 
    path = path
  )
  
  # Parse tables for NCA:
  parse_data_for_nca(data)
  
}

