test_that("is_continuous gives expected results", {
  expect_true(is_continuous(1:10))
  expect_false(is_continuous(LETTERS))
  expect_true(is_continuous(c("a", 1:9), cutoff = 0.8))
  expect_false(is_continuous(c("a", 1:9), cutoff = 1))
})

test_that("cutoff is no more than 1", {
  expect_warning(expect_true(is_continuous(1:10, cutoff = 2)))
})

test_that("NAs in original data don't count toward cutoff", {
  x1 <- 1:10
  x2 <- c(1:10, rep(NA, 5))
  x3 <- c(1:7, LETTERS[1:3])
  x4 <- c(1:3, rep(NA, 4), LETTERS[1:3])
  expect_true(is_continuous(x1, cutoff = 0.7))
  expect_true(is_continuous(x2, cutoff = 0.7))
  expect_true(is_continuous(x3, cutoff = 0.7))
  expect_true(is_continuous(x4, cutoff = 0.7))
  expect_false(is_continuous(x4, 0.8))
})
