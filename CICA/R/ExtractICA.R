#' Extract Group ICA parameters
#'@description Internal function for CICA package
#' @param DataList a list object to perform Group ICAs on
#' @param nComp number of ICA components to extract
#' @param svd
#'
#'
#' @keywords internal
#' @return a list with cluster specific independent components
ExtractICA <- function(DataList,  nComp, svd = FALSE){
  #DataList <- lapply(DataList, FUN = scale, center = T, scale = T)

  if(svd == FALSE){
    icaListCluster <- lapply(DataList, ica::icafast, nc = nComp)
    ICA_S <- lapply(icaListCluster, function(anom) anom$S)
    ListRes <- list("Sr" = ICA_S)
  }else{
    icaListCluster <- lapply(DataList, svd)
    ICA_S <- lapply(icaListCluster, function(anom) anom$u[,1:nComp] %*% diag(anom$d[1:nComp]))
    ListRes <- list("Sr" = ICA_S)
  }

  return(ListRes)
}


