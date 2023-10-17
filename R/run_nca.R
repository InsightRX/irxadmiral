#' Run a non-compartmental analysis
#' 
#' This function runs an NCA on a pre-parsed dataset. The NCA will be performed
#' using the IQnca package. The NCA output object will be saved to an
#' rds file, and not be returned.
#' 
#' @param data dataset in pre-parse format, preferrably created using
#' `create_nca_data()`.
#' @param path path to file to store output object from fit.
#' 
#' @return NULL
#' @export
#' 
#' @examples
#' 
#' run_nca(
#'   data = data,
#'   path = "./outputs/nca.rds"
#' )
#' 
run_nca <- function(
  data,
  path
) {
  
  nca <- IQnca::IQdataNCA(data) %>% # creates NCA object and fits linear model to terminal data
    IQnca::nca_IQdataNCA() %>%  # calculates the actual NCA parameters
    dplyr::select(
      USUBJID, ACTARM, 
      CMAX, TMAX, TLAG, AUCMETHD, AUCALL, LAMZHL, 
      AUCLST, CLST, LAMZ, LAMZNPT
    ) %>%
    data.frame()
  
  ## save nca object to file
  saveRDS(nca, path)
  
  return(nca)
}
