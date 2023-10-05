#' Run nlmixr model based on a given dataset
#' 
#' User needs to specify:
#' - the number of model compartments
#' - the type of administration: "iv" or "oral
#' - if "oral", if transition compartments should be used or not
#'
#' Some aspects of fitting the model are fixed for now, such as
#' - the fitting method (SAEM, which is most reliable)
#' - the selection of models
#' - the BSV and residual error models
#'
#' @param data data.frame with data to fit
#' @param n_cmt number of compartments, e.g. 1, 2, or 3. All models assume 
#' linearity, non-linear models will be added later.
#' @param type either "oral" or "iv". Other administration types will be
#' added later.
#' @param transit_compartments 
#' 
#' @export 
#' 
run_modelfit <- function(
    data,
    n_cmt,
    type = "oral",
    transit_compartments = FALSE
) {

  model <- import_model(
    n_cmt = n_cmt, 
    oral = (type == "oral")
  )
  
  # Fit the model to the data
  fit <- nlmixr2(
    model, 
    data,
    est = "saem"
  )
  saveRDS(fit, "./outputs/fit.rds")
}