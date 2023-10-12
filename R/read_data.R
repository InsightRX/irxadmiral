#' Get specific tables from database or xpt files
#' 
#' @param tables Character vector of tables to read
#' @param db list with db connection information. If NULL, will try to read
#' same data from xpt files in `path`
#' @param path optional. If no `db` specified, will read tables from xpt
#' files in folder `path`.
#' 
#' @return List containing each table as a data frame
#'
#' @export
#' 
read_data <- function(
    tables,
    db = NULL,
    path = NULL
) {
  if(is.null(db)) {
    data <- read_data_files(
      tables, 
      path = path
    )
  } else {
    # read from database:
    conn <- create_db_connection(db)
    on.exit(DBI::dbDisconnect(conn))
    data <- read_data_db(
      tables = tables, 
      conn = conn
    )
  }
  
  data
}

#' Read in .xpt files
#'
#' @param tables Character vector of tables to load
#' @param path Directory containing .xpt files
#' @return List containing each table as a data frame
#' @export
#' @examples
#' \dontrun{
#' haven::write_xpt(mtcars, file.path(tempdir(), "mtcars.xpt"))
#' haven::write_xpt(trees, file.path(tempdir(), "trees.xpt"))
#' dat <- read_data_files(tables = c("mtcars", "trees"), path = tempdir())
#' }
read_data_files <- function(tables, path = ".") {
  files <- file.path(path, paste0(tables, ".xpt"))
  names(files) <- tables
  if (!all(file.exists(files))) {
    missing <- files[!file.exists(files)]
    stop(paste0("File(s) ", paste0(basename(missing), collapse = ", "), " not found"))
  }
  lapply(files, haven::read_xpt)
}

#' Read in data from a database
#'
#' @inheritParams read_data_files
#' @param conn database connection
#' @return List containing each table as a data frame
#' @export
#' @examples
#' \dontrun{
#' # setup
#' db <- tempfile()
#' conn <- DBI::dbConnect(RSQLite::SQLite(), db)
#' DBI::dbWriteTable(conn, "mtcars", mtcars)
#'
#' dat <- read_data_db(tables = c("mtcars"), conn)
#'
#' # cleanup
#' DBI::dbDisconnect(conn)
#' unlink(db)
#' }
read_data_db <- function(tables, conn) {
  dat <- vector(mode = "list", length = length(tables))
  names(dat) <- tables

  db_tables <- DBI::dbListTables(conn)
  missing_tables <- setdiff(tables, db_tables)
  if (length(missing_tables) > 0) {
    stop(paste0("Table(s) ", paste0(missing_tables, collapse = ", "), " not found"))
  }

  for (i in tables) {
    query <- glue::glue_sql("SELECT * FROM {`i`}", .con = conn)
    dat[[i]] <- DBI::dbGetQuery(conn, query)
  }
  dat
}
