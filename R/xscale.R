#' Scale data blocks to have an equal sum of squares
#'
#' @param Xi a single datablock
#' @param value desired sum of squares value of the datablock
#'
#' @return
#' @keywords internal

xscale <- function(Xi, value = 1000){
  f <- sqrt(value/sum(Xi^2))
  return(f*Xi)
}
