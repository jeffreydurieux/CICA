# Thu Jul  1 13:39:33 2021
# Author: Jeffrey Durieux, MSc

#' Compute modified RV matrix
#' @description This function computes a NxN modified RV matrix
#' @param DataList a list with matrices
#' @param dist boolean if TRUE distance object is returned
#' @param verbose boolean if TRUE progressbar is printed to the console
#' @param parallel boolean if TRUE computations are done in parallel
#' @param cl cl object present
#'
#' @return \item{RVsS}{a square similarity matrix of \code{class} \code{\link{matrix}} or distance object of \code{class} \code{\link{dist}} containing the pairwise modified RV values}
#' @export
#' @examples data('CICA_data', package = 'CICA')
#'
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
#'
#'
computeRVmat <- function(DataList = DataList, dist = TRUE, verbose = TRUE, parallel = FALSE, cl=NULL){

  #if(verbose == TRUE & parallel == TRUE){
  #  stop('Parallel computations cannot be performed when verbose == TRUE\n P)
  #}
  N <- length(DataList)
  comb <- t(utils::combn(1:N, 2))
  RVsS <- matrix(data = NA, nrow = N , ncol = N)
  RVS <- numeric()

  if(parallel == TRUE){

    if(is.null(cl)){
      cores <- makeCluster( mc <- getOption('cl.cores', detectCores() - 1))
      doParallel::registerDoParallel(cores)
    }else{
      doParallel::registerDoParallel(cl)
    }
    parobj <- foreach(i = 1:nrow(comb), .combine = 'rbind') %dopar% {
      RVS <- modRV( DataList[[ comb[i,1] ]] , DataList[[ comb[i,2] ]])
      res <- c(comb[i , ] , RVS )
    }
    if(is.null(cl)){
      stopCluster(cores)
    }
    for(i in 1:nrow(parobj)){
      RVsS[parobj[i,1]  , parobj[i,2] ] <- parobj[i,3]
    }
  }else{
    if(verbose == TRUE){
      cat("Computing pairwise modified-RV statistics: \n")
      pb <- txtProgressBar(min = 0, max = nrow(comb), initial = 0)
      for(i in 1:nrow(comb)){
        RVS[i] <- modRV( DataList[[ comb[i,1] ]] , DataList[[ comb[i,2] ]])

        res <- c(comb[i , ] , RVS[i] )

        RVsS[res[1]  , res[2] ] <- res[3]
        setTxtProgressBar(pb, i)
      }
      cat('\n')
    }else{
      for(i in 1:nrow(comb)){
        RVS[i] <- modRV( DataList[[ comb[i,1] ]] , DataList[[ comb[i,2] ]])

        res <- c(comb[i , ] , RVS[i] )

        RVsS[res[1]  , res[2] ] <- res[3]
      }
    }
  }




  RVsS[lower.tri(RVsS)] = t(RVsS)[lower.tri(RVsS)]
  diag(RVsS) <- 1

  if(dist == TRUE){
    RVsS <- as.dist(sqrt(1-RVsS))
  }
  return(RVsS)
}

