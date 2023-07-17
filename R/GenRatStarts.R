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

  if(NCOL(rat$rationalstarts)>1){
    comb <- t(combn(1:NCOL(rat$rationalstarts), 2))
  }else{
    out <- list()
    out$rat <- rat
    out$ARIs <- matrix(NA,1,1)
    return(out)
  }

  rsARI <- matrix(data = NA, nrow = NCOL(rat$rationalstarts) , ncol = NCOL(rat$rationalstarts))
  rsi <- numeric()

  for(i in 1:nrow(comb)){
    idx <- comb[i,]
    rsi<- adjustedRandIndex( rat$rationalstarts[,idx[1]], rat$rationalstarts[,idx[2]] )
    res <- c(comb[i , ] , rsi)
    rsARI[res[1]  , res[2] ] <- res[3]
  }
rsARI
  idxdublicate <- which(rsARI == 1, arr.ind = TRUE)
  if(nrow(idxdublicate != 0)){
    for(i in 1:nrow(idxdublicate)){
      rat$rationalstarts[,idxdublicate[i,2]] <- clusf(nBlocks = length(DataList), nClus = nClus)
      colnames(rat$rationalstarts)[idxdublicate[i,2]] <- 'ReplacedRandom'
    }
  }


  #rsARI[lower.tri(rsARI)] = t(rsARI)[lower.tri(rsARI)]

  out <- list()
  out$rat <- rat
  out$ARIs <- rsARI
  return(out)
}
