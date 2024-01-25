test_that("set_global_plot_theme works for bar charts", {
  local_set_global_plot_theme()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(cyl)) + ggplot2::geom_bar()
  dat <- ggplot2::layer_data(p)
  expect_true(all(is.na(dat$colour)))
  expect_true(all(dat$fill == "#35bcb1"))
})

test_that("set_global_plot_theme works for histograms", {
  local_set_global_plot_theme()

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg)) + 
    ggplot2::geom_histogram(bins = 30)
  dat <- ggplot2::layer_data(p)
  expect_true(all(is.na(dat$colour)))
  expect_true(all(dat$fill == "#35bcb1"))
})
