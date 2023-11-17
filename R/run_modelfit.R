#' Run a modelfit
#' 
#' This function fits a population PK model to a population PK dataset
#' that is in a NONMEM-style format. The fit will be performed using 
#' nlmixr2 using the SAEM algorithm. The fit object will be saved to an
#' rds file, and not be returned.
#' 
#' @param data dataset in NONMEM-style format, preferrably created using
#' `create_modelfit_data()`.
#' @param path path to file to store output object from fit.
#' @param ... arguments passed to `create_model()` function. See 
#' available arguments to that function.
#' 
#' @return NULL
#' @export
#' 
#' @examples
#' 
#' run_modelfit(
#'   data = data,
#'   n_cmt = 1, 
#'   route = "oral",
#'   absorption = "linear",
#'   path = "./outputs/fit.rds"
#' )

run_modelfit <- function(
  data,
  path,
  ...
) {
  
  ## load / create model  
  model <- create_model(
    software = "nlmixr",
    data = data,
    ...
  )

  ## fit model to data
  fit <- nlmixr2::nlmixr2(
    model,
    data,
    est = "saem",
    nlmixr2::saemControl(print=50, nBurn=100, nEm=150), # limit to 250 iterations for now
    nlmixr2::tableControl(cwres=TRUE, npde=TRUE)
  )

  ## save fit object to file
  saveRDS(fit, path)
  
  ## save model to markdown file
  md_path <- stringr::str_replace(path, "\\.rds", ".md")
  save_model_code(model, md_path)
  
  return(fit)
  
}
