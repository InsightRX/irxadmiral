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
  m1 <- mockery::mock(mtcars, trees)
  m2 <- mockery::mock(c("mtcars", "trees"))
  mockery::stub(read_data_db, "DBI::dbGetQuery", m1)
  mockery::stub(read_data_db, "DBI::dbListTables", m2)

  dat <- read_data_db(tables = c("mtcars", "trees"), conn = DBI::ANSI())
  expect_length(dat, 2)
  expect_true(inherits(dat[[1]], "data.frame"))
  expect_true(inherits(dat[[2]], "data.frame"))
  expect_named(dat, c("mtcars", "trees"))
})

test_that("read_data_db errors if can't find tables", {
  m1 <- mockery::mock(mtcars, trees)
  m2 <- mockery::mock(c("mtcars", "trees"))
  mockery::stub(read_data_db, "DBI::dbGetQuery", m1)
  mockery::stub(read_data_db, "DBI::dbListTables", m2)

  expect_error(
    read_data_db(tables = c("unknowntable"), conn = DBI::ANSI())
  )
})
