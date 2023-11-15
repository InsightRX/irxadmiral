#' Create a model 
#' 
#' @inheritParams run_modelfit
#' @param n_cmt number of compartments for the population PK model
#' @param route administration route, either `"oral"` or `"iv"` or NULL. If NULL 
#' (default) will read from data (`ROUTE` column) and used route specified for 
#' first dose in dataset.
#' @param absorption one of `"linear"`, `"linear_lag"` (linear with lagtime), or
#'  `"transit"` (transit compartments using the Stirling approximation as 
#'  described in Savic et al. JPKPD 2007). Argument is only required when 
#'  `route` is `"oral"`.
#' @param software Currently only `nlmixr2` supported. 
#' 
#' @returns an R function object for nlmixr2, or a NONMEM model as a 
#' character vector (TODO)
#'  
#' @export
create_model <- function(
  n_cmt = 1,
  route = NULL,
  absorption = c("linear", "linear_lag", "transit"),
  software = c("nlmixr2", "nlmixr"),
  data
) {

  ## parse arguments
  absorption <- match.arg(absorption)
  software <- match.arg(software)
  if(is.null(route)) {
    route <- get_route_from_data(data$ROUTE)
  }
  if(route %in% c("iv", "oral")) {
    stop("Only `iv` and `oral` supported currently as `route`.")
  }

  if(tolower(software) %in% c("nlmixr", "nlmixr2")) {
    extra <- NULL
    if(route == "oral") {
      extra <- dplyr::case_when(
        absorption == "linear_lag" ~ "lag",
        absorption == "transit" ~ "transit",
        .default = NA
      )
      if(is.na(extra)) { extra <- NULL }
    }
    model <- get(
      paste0(c(
        "nlmixr2_pk", 
        paste0(n_cmt, "cmt"), 
        route, 
        "linear", 
        extra
        ), collapse = "_"
      )
    )
  }

  model
}
