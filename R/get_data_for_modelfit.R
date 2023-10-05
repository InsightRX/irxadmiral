#' Get the data needed for model fitting and parse into NONMEM style
#' format
#' 
#' @inheritParams create_db_connection
#' 
#' @returns NONMEM style dataset as data.frame
#'
get_data_for_modelfit <- function(
  db
) {
  
  # read from database:
  conn <- create_db_connection(db)
  data <- read_data_db(
    tables = c("pc", "adsl", "ex", "lb", "vs"), 
    conn = conn
  )
  DBI::dbDisconnect(conn)

  # parse data into NONMEM-style and return
  parse_data_for_modelfit(data)
  
}

