#' Greater-than-or-equal-to with a little room for floating point precision
#' issues
#'
#' @keywords internal
#' @param x Numeric vector
#' @param y Numeric vector
`%>=%` <- function(x, y) {
  if (length(x) == 0 | length(y) == 0) {
    return(logical(0))
  }
  x > y | mapply(function(x, y) isTRUE(all.equal(x, y)), x, y)
}

#' Less-than-or-equal-to with a little room for floating point precision
#' issues
#'
#' @keywords internal
#' @param x Numeric vector
#' @param y Numeric vector
`%<=%` <- function(x, y) {
  if (length(x) == 0 | length(y) == 0) {
    return(logical(0))
  }
  x < y | mapply(function(x, y) isTRUE(all.equal(x, y)), x, y)
}
