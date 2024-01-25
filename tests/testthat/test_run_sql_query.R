test_that("run_sql_query works", {
  db_file <- fs::file_temp()
  withr::defer(unlink(db_file))
  local_create_sqlite_db(db = db_file)
  local_mocked_bindings(
    create_db_connection = function(...) DBI::dbConnect(RSQLite::SQLite(), db_file)
  )

  res <- run_sql_query(db = NULL, "SELECT * FROM mtcars LIMIT 1")
  expect_equal(nrow(res), 1)
  expect_named(
    res,
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )
})
