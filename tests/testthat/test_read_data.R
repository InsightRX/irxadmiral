test_that("read_data_files reads in files", {
  tmppath <- fs::file_temp()
  local_create_data_dir(tmppath)

  dat <- read_data_files(c("mtcars", "trees"), tmppath)

  expect_length(dat, 2)
  expect_true(inherits(dat[[1]], "data.frame"))
  expect_true(inherits(dat[[2]], "data.frame"))
  expect_named(dat, c("mtcars", "trees"))
})

test_that("read_data_files errors if can't find data", {
  tmppath <- fs::file_temp()
  local_create_data_dir(tmppath)

  expect_error(read_data_files(c("unknowntable"), tmppath))
})

test_that("read_data_db returns data from tables", {
  db_file <- fs::file_temp()
  withr::defer(unlink(db_file))
  local_create_sqlite_db(db = db_file)
  conn <- withr::local_db_connection(DBI::dbConnect(RSQLite::SQLite(), db_file))

  dat <- read_data_db(tables = c("mtcars", "trees"), conn)
  expect_length(dat, 2)
  expect_true(inherits(dat[[1]], "data.frame"))
  expect_true(inherits(dat[[2]], "data.frame"))
  expect_named(dat, c("mtcars", "trees"))
})

test_that("read_data_db errors if can't find tables", {
  db_file <- fs::file_temp()
  withr::defer(unlink(db_file))
  local_create_sqlite_db(db = db_file)
  conn <- withr::local_db_connection(DBI::dbConnect(RSQLite::SQLite(), db_file))

  expect_error(
    read_data_db(tables = c("unknowntable"), conn = conn)
  )
})

test_that("read_data looks for files if db is null", {
  tmppath <- fs::file_temp()
  local_create_data_dir(tmppath)
  dat <- read_data(tables = "mtcars", db = NULL, path = tmppath)
  expect_named(dat, "mtcars")
  expect_true(is.data.frame(dat[["mtcars"]]))
})

test_that("read_data looks in DB if db arg is provided", {
  db_file <- fs::file_temp()
  withr::defer(unlink(db_file))
  local_create_sqlite_db(db = db_file)
  local_mocked_bindings(
    create_db_connection = function(...) DBI::dbConnect(RSQLite::SQLite(), db_file)
  )
  dat <- read_data("mtcars", db = list())
  expect_named(dat, "mtcars")
  expect_true(is.data.frame(dat[["mtcars"]]))
})
