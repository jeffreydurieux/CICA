#' The modified-RV coefficient
#'
#' @param X first matrix, number of rows in X should corresponds to number of rows in Y
#' @param Y second matrix, number of rows in Y should corresponds to number of rows in X
#'
#' @return A single value, indicating the similarity between the two input matrices
#' @export
#'
#'@author Jeffrey Durieux
#'@references Smilde AK, Kiers HAL, Bijlsma S, Rubingh CM, van Erk MJ (2009) Matrix correlations for high-dimensional data: the modified RV-coefficient. Bioinformatics 25(3):401–405
#'
#' @seealso \code{\link{computeRVmat}}
#'
#' @examples
#' X1 <- matrix(rnorm(1000), 10)
#' SVD <- svd(X1)
#' X2 <- SVD$u[,-5] %*% diag(SVD$d[-5]) %*% t(SVD$v[,-5])
#' modRV(X1,X2)




modRV <- function(X, Y){

  if(nrow(X) != nrow(Y)){
    stop('Number of rows of input matrices are not equal')
  }

  XXtilde <- ( X %*% t(X) ) - diag (diag( X %*% t(X) ) )
  YYtilde <- ( Y %*% t(Y) ) - diag (diag( Y %*% t(Y) ) )

  res <-  ( t(c(XXtilde)) %*% c(YYtilde) ) /
    ( sqrt( ( t(c(XXtilde)) %*% c(XXtilde)) * ( t(c(YYtilde)) %*% c(YYtilde)) ) )


  return(res)
}
