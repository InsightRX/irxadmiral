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
  
  reqd_tables <- c("pc", "adsl", "ex", "lb", "vs")
  if(is.null(db)) {
    data <- read_data_files(
      reqd_tables, 
      path = path
    )
  } else {
    # read from database:
    conn <- create_db_connection(db)
    on.exit(DBI::dbDisconnect(conn))
    data <- read_data_db(
      tables = reqd_tables, 
      conn = conn
    )
  }

  # parse data into NONMEM-style and return
  parse_data_for_modelfit(data)
  
}

