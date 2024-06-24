#' Extract Group ICA parameters
#'@description Internal function for CICA package
#' @param DataList a list object to perform Group ICAs on
#' @param nComp number of ICA components to extract
#' @param method
#'
#'
#' @keywords internal
#' @return a list with cluster specific independent components
ExtractICA <- function(DataList,  nComp, method = 'fastICA', covL=NULL, indexList=NULL){
  #DataList <- lapply(DataList, FUN = scale, center = T, scale = T)

  if(method == 'fastICA'){
    icaListCluster <- lapply(DataList, icafast, nc = nComp)
    ICA_S <- lapply(icaListCluster, function(anom) anom$S)
    ListRes <- list("Sr" = ICA_S)
  }else{
    
    subcovs <- lapply(1:length(indexList), function(i) covL[ indexList[[i]] , indexList[[i]] ] )
    #for(i in 1:length(indexList)){
    #  subcov <- covL[ indexList[[i]] , indexList[[i]]]
    #}
    
    eigL <- lapply(subcovs, Rfast::eigen.sym, k = nComp,vectors = TRUE)
    ICA_S <- lapply(seq_along(eigL), function(i) DataList[[i]] %*% eigL[[i]]$vectors)
    
    ListRes <- list("Sr" = ICA_S)
  }

  return(ListRes)
}


