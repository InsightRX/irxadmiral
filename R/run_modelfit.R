#' Run a modelfit
#' 
#' This function fits a population PK model to a population PK dataset
#' that is in a NONMEM-style format. The fit will be performed using 
#' nlmixr2 using the SAEM algorithm. The fit object will be saved to an
#' rds file, and not be returned.
#' 
#' @param data dataset in NONMEM-style format, preferrably created using
#' `create_modelfit_data()`.
#' @param n_cmt number of compartments for the population PK model
#' @param route administration route, either `"oral"` or `"iv"`
#' @param path path to file to store output object from fit.
#' 
#' @return NULL
#' 
#' @examples
#' 
#' run_modelfit(
#'   data = data,
#'   cmt = 1, 
#'   admin_type = "oral",
#'   path = "./outputs/fit.rds"
#' )

run_modelfit <- function(
  data,
  n_cmt = 1,
  route = "oral",
  path
) {
  
  ## read model file and source
  modelfile <- system.file(
    package = "irxadmiral", 
    paste0("models/nlmixr2/pk_", n_cmt, "_", route, "linear.R")
  )
  if(!file.exists(modelfile)) {
    stop("Sorry, requested model is not available.")
  }
  model <- source(modelfile)$value

  ## fit model to data
  fit <- nlmixr2(
    model, 
    poppk_data,
    est = "saem"
  )
  
  ## save fit object to file
  saveRDS(fit, path)
  
}