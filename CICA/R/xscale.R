#' Scale data blocks to have an equal sum of squares
#'@description Internal function for CICA package
#' @param Xi a single datablock
#' @param value desired sum of squares value of the datablock
#'
#' @keywords internal
#' @return a matrix that is scale to have a sum of squares of size \code{value}

xscale <- function(Xi, value = 1000){
  f <- sqrt(value/sum(Xi^2))
  return(f*Xi)
}
