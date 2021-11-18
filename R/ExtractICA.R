#' Extract Group ICA parameters
#'@description Internal function for CICA package
#' @param DataList a list object to perform Group ICAs on
#' @param nComp number of ICA components to extract
#'
#'
#' @keywords internal
#' @return a list with cluster specific independent components
ExtractICA <- function(DataList,  nComp){
  #DataList <- lapply(DataList, FUN = scale, center = T, scale = T)
  icaListCluster <- lapply(DataList, ica::icafast, nc = nComp)
  ICA_S <- lapply(icaListCluster, function(anom) anom$S)
  ListRes <- list("Sr" = ICA_S)
  return(ListRes)
}


