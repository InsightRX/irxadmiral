test_that("get_route_from_data returns expected result", {
  expect_equal(get_route_from_data("oral"), "oral")
  expect_equal(get_route_from_data(c("oral", "oral")), "oral")
  expect_equal(get_route_from_data(c("subcutaneous", "subcutaneous")), "sc")
  expect_equal(get_route_from_data(c("sc", "sc")), "sc")
  expect_equal(get_route_from_data(c("intramuscular", "intramuscular")), "im")
  expect_equal(get_route_from_data(c("im", "im")), "im")
  expect_equal(get_route_from_data(c("intravenous", "intravenous")), "iv")
  expect_equal(get_route_from_data(c("iv", "iv")), "iv")
  expect_equal(get_route_from_data("unknown"), "oral")
})

test_that("get_route_from_data warns of multiple routes and returns first", {
  expect_warning(res1 <- get_route_from_data(c("oral", "iv")))
  expect_warning(res2 <- get_route_from_data(c("iv", "oral")))
  expect_equal(res1, "oral")
  expect_equal(res2, "iv")
})

test_that("get_route_from_data handles NAs", {
  expect_equal(get_route_from_data(c(NA, "iv")), "iv")
  expect_equal(get_route_from_data(c(NA)), "oral")
})
