#' Title
#'
#' @param DataList DataList
#' @param RatStarts Type of rational start
#' @param nComp    number of components
#' @param nClus    number of clusters
#' @param scalevalue value for blockscaling procedure
#' @param center  center
#' @param verbose verbose
#' @param pseudo  percentage used for perturbation rational starts (between 0)
#' @param pseudoFac multiplication factor for pseudo rational starts
#'
#' @return out
#'
GenRatStarts <- function(DataList, RatStarts, nComp, nClus, scalevalue, center, verbose, pseudo, pseudoFac){


  rat <- FindRationalStarts(DataList = DataList, RatStarts = RatStarts, nComp = nComp, nClus = nClus,
                            scalevalue = scalevalue, center = center, verbose = verbose,
                            pseudo = pseudo, pseudoFac = pseudoFac)

  comb <- t(combn(1:ncol(rat$rationalstarts), 2))

  rsARI <- matrix(data = NA, nrow = ncol(rat$rationalstarts) , ncol = ncol(rat$rationalstarts))
  rsi <- numeric()

  for(i in 1:nrow(comb)){
    idx <- comb[i,]
    rsi<- adjustedRandIndex( rat$rationalstarts[,idx[1]], rat$rationalstarts[,idx[2]] )
    res <- c(comb[i , ] , rsi)
    rsARI[res[1]  , res[2] ] <- res[3]
  }

  rsARI[lower.tri(rsARI)] = t(rsARI)[lower.tri(rsARI)]

  out <- list()
  out$rat <- rat
  out$ARIs <- rsARI
  return(out)
}
