#' Get the data needed for model fitting and parse into NONMEM style
#' format
#' 
#' @param db list with db connection information. If NULL, will try to read
#' same data from xpt files in `path`
#' @param path optional. If no `db` specified, will read tables from xpt
#' files in folder `path`.
#' 
#' @returns NONMEM style dataset as data.frame
#'
#' @export
#' 
get_data_for_modelfit <- function(
  db = NULL,
  path = NULL
) {
  
  data <- read_data(
    tables = c("pc", "adsl", "ex", "lb", "vs"), 
    db = db, 
    path = path
  )

  # parse data into NONMEM-style and return
  parse_data_for_modelfit(data)
  
}

