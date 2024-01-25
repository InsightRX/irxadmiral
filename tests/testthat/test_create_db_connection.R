test_that("create_db_connection checks input data", {
  expect_error(create_db_connection(db = list(a = 1)), regexp = "Missing")
})

test_that("create_db_connection returns a connection", {
  local_mocked_bindings(Postgres = function(...) RSQLite::SQLite())
  conn <- create_db_connection(
    db = list(
      host = "127.0.0.1",
      dbname = ":memory:",
      user = "postgres",
      password = "password",
      port = 5432
    )
  )
  withr::defer(DBI::dbDisconnect(conn))
  expect_true(inherits(conn, "SQLiteConnection"))
})
