#' Extract Group ICA parameters
#'@description Internal function for CICA package
#' @param DataList a list object to perform Group ICAs on
#' @param nComp number of ICA components to extract
#' @param parallel compute ICA components in parallel
#' @param cl an object created by parallel::makeCluster
#'
#'
#' @keywords internal
#' @return a list with cluster specific independent components
ExtractICA <- function(DataList,  nComp, parallel, cl){
  #DataList <- lapply(DataList, FUN = scale, center = T, scale = T)
  if(parallel == parallel){
    icaListCluster <- parLapply(cl = cl, DataList, ica::icafast, nc = nComp)
    ICA_S <- parLapply(cl = cl, icaListCluster, function(anom) anom$S)
  }else{
    icaListCluster <- lapply(DataList, ica::icafast, nc = nComp)
    ICA_S <- lapply(icaListCluster, function(anom) anom$S)
  }

  ListRes <- list("Sr" = ICA_S)
  return(ListRes)
}


