#' Compute loss per data matrix using multivariate regression
#'@description Internal function for CICA package
#' @param Xi datablock
#' @param SkL list with Group ICA parameters
#'
#' @keywords internal
#' @return a numeric vector with loss function values
Lir <- function(Xi, SkL)
{
  # compute Ahats
  Ah <- lapply(seq_along( SkL), function(i) {
    crossprod(Xi, SkL[[i]]) %*% NMFN::mpinv( t(SkL[[i]]) %*% SkL[[i]])
  })

  # compute Xhats --> Xhat^r
  Xhat <- lapply(seq_along(Ah), function(i) {
    tcrossprod( SkL[[i]], Ah[[i]])
  })

  # loss per data matrix
  SS <- sapply(seq_along(Xhat), function(i) {
    sum((Xi - Xhat[[i]]) ^ 2)
  })

  return(SS)
}#function
