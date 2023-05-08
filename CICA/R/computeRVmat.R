# Thu Jul  1 13:39:33 2021
# Author: Jeffrey Durieux, MSc

#' Compute modified RV matrix
#' @description This function computes a NxN modified RV matrix
#' @param DataList a list with matrices
#' @param dist boolean if TRUE distance object is returned
#' @param verbose boolean if TRUE progressbar is printed to the console
#'
#' @return \item{RVsS}{a square similarity matrix of \code{class} \code{\link{matrix}} or distance object of \code{class} \code{\link{dist}} containing the pairwise modified RV values}
#'
#' @examples
#'
#' \dontrun{
#' CICA_data <- Sim_CICA(Nr = 15, Q = 5, R = 4, voxels = 100, timepoints = 10,
#' E = 0.4, overlap = .25, externalscore = TRUE)
#' #Compute single subject ICAs (nClus equals length(ExampleData))
#' output <- CICA(DataList = CICA_data$X, nStarts = 1,
#'                nComp = 5, nClus = 9, verbose = FALSE)
#'
#' RV <- computeRVmat(DataList = output$Sr, dist = TRUE,
#'                     verbose = FALSE)
#'
#' # apply hierarchical clustering on RV output
#' hcl <- hclust(RV)
#' plot(hcl)
#'
#' # low dimensional visualisation using Classical Multidimensional Scaling
#' mds <- cmdscale(RV)
#' plot(mds)
#' }
#'
#' @importFrom utils combn
#'
#' @export
#'
computeRVmat <- function(DataList = DataList, dist = TRUE, verbose = TRUE){

  N <- length(DataList)

  comb <- t(combn(1:N, 2))

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
    RVsS <- as.dist(sqrt(1-RVsS))
  }
  return(RVsS)
}

