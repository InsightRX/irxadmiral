#' Create connection to a postgres database
#'
#' @importFrom RPostgres Postgres
#' @param db list containing database connection details. Requires `host`, 
#' `dbname`, `user`, `password`, and `port`
#' 
#' @returns db connection object
#' 
#' @export
#' 
create_db_connection <- function(
  db
) {
  ## check all arguments specified
  reqd <- c("host", "dbname", "user", "password", "port")
  chk <- setdiff(reqd, names(db))
  if(length(chk) > 0)  {
    stop("Missing DB parameters: ", paste0(chk, collapse = ", "))
  }
  
  ## make connection and return
  DBI::dbConnect(
    Postgres(),
    host = db$host, 
    dbname = db$dbname, 
    user = db$user, 
    password = db$password, 
    port = db$port
  )
}
