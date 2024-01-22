#' Test if a vector of values is likely to be continuous or categorical
#' 
#' The function will try to convert all values to numeric. If a certain fraction
#' of values is able to convert succesfully, it will assume the vector is 
#' continuous. Default treshold is 0.8.
#' 
#' @param x vector of values
#' @param cutoff cutoff value for deciding between continuous/categorical
#' 
is_continuous <- function(x, cutoff = 0.8) {
  suppressWarnings(
    tmp <- as.numeric(as.character(x))
  )
  sum(!is.na(tmp)) / length(tmp) >= cutoff
}
