#' Run SQL query against a database
#' 
#' Based on provided database connection info and query statement.
#' 
#' @inheritParams create_db_connection
#' @param query SQL query to run
#'
#' @examples
#' \dontrun{
#' db <- list(
#'   host = "test",
#'   dbname = "postgres",
#'   user = "postgres",
#'   password = "secret",
#'   port = 5432
#' )
#' ex <- run_sql_query(db, "SELECT * FROM ex")
#' }
#' 
#' @returns results from query as data.frame
#' 
#' @export
#' 
run_sql_query <- function(
  db, 
  query
) {
  conn <- create_db_connection(db) # for now, we will not try to persist the connection
  on.exit(DBI::dbDisconnect(conn))
  result <- DBI::dbGetQuery(
    conn = conn, 
    statement = query
  )
  return(result)
}

