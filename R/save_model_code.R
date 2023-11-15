#' Save model code to a markdown file
#' 
#' @param model nlmixr2 model object
#' @param path path to .rds file for fit. Will change extension to .md
#' 
#' @export
save_model_code <- function(
  model,
  path
) {
  
  ## Works for nlmixr2 models. Will need to adapt for NONMEM models
  md <- paste(
    "```",
    paste0(attr(model, "srcref"), collapse = "\n"),
    "```",
    sep = "\n"
  )

  md_path <- stringr::str_replace(path, "\\.rds", ".md")
  writeLines(md, md_path)
}