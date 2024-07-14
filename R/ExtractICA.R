#' Extract Group ICA parameters
#'@description Internal function for CICA package
#' @param DataList a list object to perform Group ICAs on
#' @param nComp number of ICA components to extract
#' @param method either fastICA else EVD is used
#' @param covL large precomputed covariance matrix, used for EVD method
#' @param indexList list with indices for extract covariances based on cluster indices
#'
#'
#' @keywords internal
#' @return a list with cluster specific independent components
ExtractICA <- function(DataList,  nComp, method = 'fastICA', covL=NULL, indexList=NULL){
  

  if(method == 'fastICA'){
    icaListCluster <- lapply(DataList, icafast, nc = nComp)
    ICA_S <- lapply(icaListCluster, function(anom) anom$S)
    ListRes <- list("Sr" = ICA_S)
  }else{
    
    subcovs <- lapply(1:length(indexList), function(i) covL[ indexList[[i]] , indexList[[i]] ] )
    eigL <- lapply(subcovs, eigen.sym, k = nComp,vectors = TRUE)
    ICA_S <- lapply(seq_along(eigL), function(i) DataList[[i]] %*% eigL[[i]]$vectors)
    
    ListRes <- list("Sr" = ICA_S)
  }

  return(ListRes)
}


