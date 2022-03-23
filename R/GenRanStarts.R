#' Title
#'
#' @param RanStarts number of randomstarts to generate
#' @param nClus     number of clusters
#' @param nBlocks   number of objects
#' @param ARIlim maximal value of adjusted Rand Index
#' @param itmax     maximum number of iterations used to find suitable random starts
#' @param verbose
#'
#' @return a list where the first element is a matrix with random starts, second element all pairwise ARIs
#' #'

GenRanStarts <- function(RanStarts, nClus, nBlocks, ARIlim = 0.2,
                         itmax = 1000, verbose = FALSE){
  # random starts
  rs <- matrix(clusf(nBlocks = nBlocks, nClus = nClus) )

  it <- 0
  while(ncol(rs) != RanStarts & it < itmax){
    it <- it + 1
    candidate <- clusf(nBlocks = nBlocks, nClus = nClus)
    if(all(abs(apply(rs, MARGIN = 2, adjustedRandIndex, y = candidate)) < ARIlim)){
      rs <- cbind(rs, candidate)
    }
    if(verbose){
      cat(ncol(rs), fill = TRUE)
    }

  }

  if(it == itmax){
    warning(paste('Not enough random starts found with ARIlimRan of:',ARIlim))
  }


  # comb <- t(utils::combn(1:ncol(rs), 2))
  #
  # rsARI <- matrix(data = NA, nrow = ncol(rs) , ncol = ncol(rs))
  # rsi <- numeric()
  #
  # for(i in 1:nrow(comb)){
  #   idx <- comb[i,]
  #   rsi<- adjustedRandIndex( rs[,idx[1]], rs[,idx[2]] )
  #   res <- c(comb[i , ] , rsi)
  #   rsARI[res[1]  , res[2] ] <- res[3]
  # }
  #
  # rsARI[lower.tri(rsARI)] = t(rsARI)[lower.tri(rsARI)]

  out <- list()
  out$rs <- rs
  # out$ARIs <- rsARI
  return(out)
}
