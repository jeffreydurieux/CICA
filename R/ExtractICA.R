#' Extract Group ICA parameters
#'@description Internal function for CICA package
#' @param DataList a list object to perform Group ICAs on
#' @param nComp number of ICA components to extract
#' @param EVD Do computations using ICA or EVD
#'
#'
#' @keywords internal
#' @return a list with cluster specific independent components
ExtractICA <- function(DataList,  nComp, EVD = FALSE){
  DataList <- lapply(DataList, FUN = scale, center = T, scale = T)
  if(EVD == FALSE){
    icaListCluster <- lapply(DataList, ica::icafast, nc = nComp)
    ICA_S <- lapply(icaListCluster, function(anom) anom$S)
  }else{
    ICA_S <- EVDs(DataList, nComp = nComp)
  }

  ListRes <- list("Sr" = ICA_S)
  return(ListRes)
}


