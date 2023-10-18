#' Run simulation from a fitted object
#' 
#' Using fit object from fit with nlmixr2.
#' 
#' @param obj fit object from nlmixr2
#' @param dose dose amount
#' @param interval dosing interval
#' @param n_doses number of doses to simulate
#' @param n_subjects number of subjects to simulate
#' @param aggregate summarize the data using mean, median, sd, etc.
#' @param group grouping to be added to aggregated data? 
#' @param bsv Simulate using between subject variability
#' @param res_error Add residual unexplained error to the simulated data?
#' @param ... arguments passed on to rxode2::rxSolve() function
#' 
#' @export
#' 
run_simulation <- function(
    obj,
    dose, 
    interval,
    n_doses = 5,
    n_subjects = 500,
    aggregate = TRUE,
    bsv = TRUE,
    res_error = TRUE,
    group = NULL,
    ...
) {
  t_obs <- seq(0, (n_doses+1)*interval, by = 2)
  ev <- rxode2::eventTable() %>%
    rxode2::add.dosing(
      dose = dose,
      nbr.doses = 5, 
      dosing.interval = interval
    ) %>%
    rxode2::add.sampling(t_obs)
  if(!bsv) {
    omega <- fit$omega * 1e-6 # cannot be NULL or matrix of 0s
  } else {
    omega <- fit$omega
  }
  dat <- rxode2::rxSolve(
    object = fit,
    omega = omega,
    events = ev,
    nsim =  n_subjects,
    seed = 12345,
    returnType = "data.frame",
    simVariability = FALSE, # This seems to be "uncertainty in Theta" rather than "variability". We usually want this switched off.
    ...
  ) %>%
    dplyr::mutate(y = ipredSim)
  
  ## add residual error
  prop_sd <- 0
  add_sd <- 0
  pars <- as.list(fit$fixef)
  if("prop_sd" %in% names(pars)) {
    prop_sd <- pars$prop_sd
  }
  if("add_sd" %in% names(pars)) {
    add_sd <- pars$add_sd
  }
  if(res_error) {
    dat$y <- PKPDsim::add_ruv(dat$y, list(add = add_sd, prop = prop_sd))
  }
  if(!is.null(group)) {
    dat <- dat %>%
      dplyr::group_by(all_of(group))
  }
  if(aggregate) {
    dat <- dat %>%
      dplyr::group_by(time, .add = TRUE) %>%
      dplyr::summarise(
        mean = mean(y),
        median = stats::median(y),
        q_5 = stats::quantile(y, .05),
        q_95 = stats::quantile(y, .95),
        sd = stats::sd(y)
      )
  } else {
    dat 
  }
  return(dat)
}