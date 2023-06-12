#' Moore Penrose inverse
#'
#' @param X input matrix
#'
#' @return mp Moore Penrose inverse of matrix X
#'

mpinv <- function (X)
{
  #### function from NMFN package but was not maintained any longer
  #### original author: Suhai (Timothy) Liu <tim.liu@alumni.duke.edu>
  Eps <- 100 * .Machine$double.eps
  s <- svd(X)
  d <- s$d
  m <- length(d)
  if (!(is.vector(d)))
    return(t(s$v %*% (1/d) %*% t(s$u)))
  d <- d[d > Eps]
  notnull <- length(d)
  if (notnull == 1) {
    inv <- 1/d
  }
  else {
    inv <- solve(diag(d))
  }
  if (notnull != m) {
    inv <- cbind(inv, matrix(0, nrow = notnull, ncol = (m -
                                                          notnull)))
    inv <- rbind(inv, matrix(0, nrow = (m - notnull), ncol = m))
  }
  mp <- s$v %*% inv %*% t(s$u)
  mp[abs(mp) < Eps] <- 0
  return(mp)
}
