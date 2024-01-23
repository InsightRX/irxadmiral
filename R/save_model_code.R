#' Save model code to a markdown file
#' 
#' @param model nlmixr2 model object
#' @param path path to .md file to save model code to
#' 
#' @export
save_model_code <- function(
  model,
  path
) {
  
  ## Works for nlmixr2 models. Will need to adapt for NONMEM models
  md <- paste(
    "## Model code",
    "",
    "The following nlmixr2 model code was used in the compartmental analysis: ",
    "",
    "```",
    paste0(utils::capture.output(dput(model)), collapse = "\n"),
    "```",
    sep = "\n"
  )

  writeLines(md, path)
}
