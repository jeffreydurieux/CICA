#' Extract Group ICA parameters
#'
#' @param DataList a list object to perform Group ICAs on
#' @param nComp number of ICA components to extract
#'
#'
#' @keywords internal
#'
ExtractICA <- function(DataList,  nComp){

  icaListCluster <- lapply(DataList, ica::icafast, nc = nComp)

  #extract RSN
  ICA_S <- lapply(icaListCluster, function(anom) anom$S)

  ListRes <- list("Sr" = ICA_S)
  return(ListRes)
}
