test_that("create_nca_table gives expected result", {
  dat <- data.frame(
    USUBJID = c("A", "B", "C", "D", "E", "F"),
    ACTARM = c(
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X Low Dose",
      "Drug X Low Dose"
    ),
    CMAX = c(4.3, 4.9, 6.8, 2.2, 4.2, 3.9),
    TMAX = c(7.8, 1.5, 3.8, 1.4, 4, 4),
    TLAG = c(0, 0, 0, 0, 0, 0),
    AUCMETHD = c(
      "linear log",
      "linear log",
      "linear log",
      "linear log",
      "linear log",
      "linear log"
    ),
    AUCALL = c(75.8, 41.4, 106.9, 26.6, 47.9, 38.6),
    LAMZHL = c(12.4, 8.3, 12.2, 8, 26.1, 21.1),
    AUCLST = c(75.1, 41.5, 109.3, 27.7, 47.3, 42.4),
    CLST = c(0.4, 0.1, 0.6, 0.1, 0.2, 0.1),
    LAMZ = c(0.1, 0.1, 0.1, 0.1, 0, 0),
    LAMZNPT = c(5.2, 8.6, 6.7, 9.1, 3.1, 3.1)
  )
  res <- create_nca_table(dat)

  expect_true(is.data.frame(res))
  expect_named(res, c("Parameter", "Description", "Statistic", "value"))
  expect_equal(nrow(res), 48)
  expect_true(all(c("AUC to infinity", "AUC to last obs") %in% res$Description))
  expect_equal(
    res[res$Parameter == "AUCALL" & res$Statistic == "mean", "value"],
    56.20
  )

  res2 <- create_nca_table(dat, format = "wide")
  expect_named(
    res2,
    c("Parameter", "Description", "mean", "sd", "cv_pct", "median", "min", "max")
  )
  expect_equal(
    head(res2, 1),
    data.frame(
      Parameter = "AUCALL",
      Description = "AUC to infinity",
      mean = 56.2,
      sd = 29.76,
      cv_pct = 52.96,
      median = 44.65,
      min = 26.6,
      max = 106.9
    )
  )
})

test_that("Description is excluded if not requested", {
  dat <- data.frame(
    USUBJID = c("A", "B", "C", "D", "E", "F"),
    ACTARM = c(
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X Low Dose",
      "Drug X Low Dose"
    ),
    CMAX = c(4.3, 4.9, 6.8, 2.2, 4.2, 3.9),
    TMAX = c(7.8, 1.5, 3.8, 1.4, 4, 4),
    TLAG = c(0, 0, 0, 0, 0, 0),
    AUCMETHD = c(
      "linear log",
      "linear log",
      "linear log",
      "linear log",
      "linear log",
      "linear log"
    ),
    AUCALL = c(75.8, 41.4, 106.9, 26.6, 47.9, 38.6),
    LAMZHL = c(12.4, 8.3, 12.2, 8, 26.1, 21.1),
    AUCLST = c(75.1, 41.5, 109.3, 27.7, 47.3, 42.4),
    CLST = c(0.4, 0.1, 0.6, 0.1, 0.2, 0.1),
    LAMZ = c(0.1, 0.1, 0.1, 0.1, 0, 0),
    LAMZNPT = c(5.2, 8.6, 6.7, 9.1, 3.1, 3.1)
  )
  res <- create_nca_table(dat, description = FALSE)

  expect_named(res, c("Parameter", "Statistic", "value"))
})

test_that("Grouping works", {
  dat <- data.frame(
    USUBJID = c("A", "B", "C", "D", "E", "F"),
    ACTARM = c(
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X Low Dose",
      "Drug X Low Dose"
    ),
    CMAX = c(4.3, 4.9, 6.8, 2.2, 4.2, 3.9),
    TMAX = c(7.8, 1.5, 3.8, 1.4, 4, 4),
    TLAG = c(0, 0, 0, 0, 0, 0),
    AUCMETHD = c(
      "linear log",
      "linear log",
      "linear log",
      "linear log",
      "linear log",
      "linear log"
    ),
    AUCALL = c(75.8, 41.4, 106.9, 26.6, 47.9, 38.6),
    LAMZHL = c(12.4, 8.3, 12.2, 8, 26.1, 21.1),
    AUCLST = c(75.1, 41.5, 109.3, 27.7, 47.3, 42.4),
    CLST = c(0.4, 0.1, 0.6, 0.1, 0.2, 0.1),
    LAMZ = c(0.1, 0.1, 0.1, 0.1, 0, 0),
    LAMZNPT = c(5.2, 8.6, 6.7, 9.1, 3.1, 3.1)
  )
  res <- create_nca_table(dat, description = FALSE, group = "ACTARM")

  expect_named(
    res,
    c("Parameter", "Statistic", "Drug X High Dose", "Drug X Low Dose")
  )
  expect_equal(
    res[res$Parameter == "AUCALL" & res$Statistic == "mean", "Drug X High Dose", drop = TRUE],
    91.35
  )
  expect_equal(
    res[res$Parameter == "AUCALL" & res$Statistic == "mean", "Drug X Low Dose", drop = TRUE],
    38.62
  )
})

test_that("csv export works", {
  tmpfile <- withr::local_file(file.path(tempdir(), "nca_table.csv"))
  dat <- data.frame(
    USUBJID = c("A", "B", "C", "D", "E", "F"),
    ACTARM = c(
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X Low Dose",
      "Drug X Low Dose"
    ),
    CMAX = c(4.3, 4.9, 6.8, 2.2, 4.2, 3.9),
    TMAX = c(7.8, 1.5, 3.8, 1.4, 4, 4),
    TLAG = c(0, 0, 0, 0, 0, 0),
    AUCMETHD = c(
      "linear log",
      "linear log",
      "linear log",
      "linear log",
      "linear log",
      "linear log"
    ),
    AUCALL = c(75.8, 41.4, 106.9, 26.6, 47.9, 38.6),
    LAMZHL = c(12.4, 8.3, 12.2, 8, 26.1, 21.1),
    AUCLST = c(75.1, 41.5, 109.3, 27.7, 47.3, 42.4),
    CLST = c(0.4, 0.1, 0.6, 0.1, 0.2, 0.1),
    LAMZ = c(0.1, 0.1, 0.1, 0.1, 0, 0),
    LAMZNPT = c(5.2, 8.6, 6.7, 9.1, 3.1, 3.1)
  )
  tbl <- create_nca_table(dat, path = tmpfile)
  res <- read.csv(tmpfile)
  expect_equal(nrow(res), 48)
  expect_named(res, c("Parameter", "Description", "Statistic", "value"))
})
