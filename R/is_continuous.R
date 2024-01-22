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
  if (cutoff > 1) {
    warning("Can't have a cutoff greater than 1; setting value to 1 instead")
    cutoff <- 1
  }
  suppressWarnings(
    tmp <- as.numeric(as.character(x))
  )

  # Are number of *new* NAs/total number of values <= 1 - cutoff
  ((sum(is.na(tmp)) - sum(is.na(x))) / length(tmp)) <= (1 - cutoff)
}
