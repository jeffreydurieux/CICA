#' Random clustering generation
#'
#' @param nBlocks number of objects
#' @param nClus number of clusters
#'
#' @return
#'
#'
#' @keywords internal
#'

clusf <- function(nBlocks, nClus) {
  #simplyfied cluster generation function using an equal probability
  clus <- CICA:::GenerateRandomClustering(nBlocks, nClus, rep(c(1 / nClus), nClus))
  #clus <- sample(x = 1:nClus, size = nBlocks, replace = TRUE)
  return(clus)
}