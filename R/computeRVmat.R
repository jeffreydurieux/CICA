# Thu Jul  1 13:39:33 2021
# Author: Jeffrey Durieux, MSc

#' Compute modified RV matrix
#' @description This function computes a NxN modified RV matrix
#' @param DataList a list with matrices
#' @param dist boolean if TRUE distance object is returned
#' @param verbose boolean if TRUE progressbar is printed to the console
#'
#' @return \item{RVsS} a matrix or distance object containing the pairwise modified RV values
#' @export
#'
computeRVmat <- function(DataList = DataList, dist = TRUE, verbose = TRUE){

  N <- length(DataList)

  comb <- t(utils::combn(1:N, 2))

  if(verbose == TRUE){
    cat("Computing pairwise modified-RV statistics: \n")
    pb <- txtProgressBar(min = 0, max = nrow(comb), initial = 0)

    RVsS <- matrix(data = NA, nrow = N , ncol = N)
    RVS <- numeric()

    for(i in 1:nrow(comb)){
      RVS[i] <- modRV( DataList[[ comb[i,1] ]] , DataList[[ comb[i,2] ]])

      res <- c(comb[i , ] , RVS[i] )

      RVsS[res[1]  , res[2] ] <- res[3]
      setTxtProgressBar(pb, i)

    }
    cat('\n')
  }else{
    RVsS <- matrix(data = NA, nrow = N , ncol = N)
    RVS <- numeric()

    for(i in 1:nrow(comb)){
      RVS[i] <- modRV( DataList[[ comb[i,1] ]] , DataList[[ comb[i,2] ]])

      res <- c(comb[i , ] , RVS[i] )

      RVsS[res[1]  , res[2] ] <- res[3]
    }
  }

  RVsS[lower.tri(RVsS)] = t(RVsS)[lower.tri(RVsS)]
  diag(RVsS) <- 1

  if(dist == TRUE){
    RVsS <- as.dist(1-RVsS)
  }
  return(RVsS)
}
