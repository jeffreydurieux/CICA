#' Random clustering generation
#'@description Internal function for CICA package
#' @param nBlocks integer: number of objects
#' @param nClus integer: number of clusters
#'
#'
#'
#' @keywords internal
#' @return a numeric vector indicating the clustering

clusf <- function(nBlocks, nClus) {
  #simplyfied cluster generation function using an equal probability
  clus <- GenerateRandomClustering(nBlocks, nClus, rep(c(1 / nClus), nClus))
  return(clus)
}
