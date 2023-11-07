# Create a temporary directory and put a couple .xpt files in it
local_create_data_dir <- function(dir = fs::file_temp(), env = parent.frame()) {
  fs::dir_create(dir)
  withr::defer(fs::dir_delete(dir), envir = env)

  fs::file_create(fs::path(dir, "mtcars.xpt"))
  fs::file_create(fs::path(dir, "trees.xpt"))
  haven::write_xpt(mtcars, path = fs::path(dir, "mtcars.xpt"))
  haven::write_xpt(trees, path = fs::path(dir, "trees.xpt"))
}

local_set_global_plot_theme <- function(theme = "irx", env = parent.frame()) {
  set_global_plot_theme(theme)
  withr::defer(reset_global_plot_theme(), envir = env)
}
