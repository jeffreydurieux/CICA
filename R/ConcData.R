#' Concatenate datablocks into list determined by cluster labels
#'@description Internal function for CICA package
#' @param DataList list with datablocks
#' @param ClusVec  vector with cluster labels
#'
#' @return list with concatenated datablocks
#'
#' @keywords internal
#'
#'
#' @author Jeffrey Durieux
#'

ConcData <- function(DataList, ClusVec){
  ClusL <- length( unique(ClusVec) )

  NewList <- list()

  for(i in 1:ClusL){
    NewList[[i]] <- do.call(cbind, DataList[ClusVec == i])
  }
  return(NewList)
} # end f_ConcData
