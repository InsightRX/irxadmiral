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
#' @param route administration route, either `"oral"` or `"iv"` or NULL. If NULL 
#' (default) will read from data (`ROUTE` column) and used route specified for 
#' first dose in dataset.
#' @param path path to file to store output object from fit.
#' 
#' @return NULL
#' @export
#' 
#' @examples
#' 
#' run_modelfit(
#'   data = data,
#'   cmt = 1, 
#'   route = "oral",
#'   path = "./outputs/fit.rds"
#' )

run_modelfit <- function(
  data,
  n_cmt = 1,
  route = NULL,
  path
) {
  
  if(is.null(route)) {
    route <- get_route_from_data(data$ROUTE)
  }
  
  ## read model file and source
  model <- get(paste0("nlmixr2_pk_", n_cmt, "cmt_", route, "_linear"))

  ## fit model to data
  fit <- nlmixr2::nlmixr2(
    model, 
    data,
    est = "saem",
    nlmixr2::saemControl(print=50, nBurn=100, nEm=150), # limit to 250 iterations for now
    nlmixr2::tableControl(cwres=FALSE, npde=TRUE)
  )
  
  ## save fit object to file
  saveRDS(fit, path)
  
  return(fit)
  
}
