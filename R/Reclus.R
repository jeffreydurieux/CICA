#' Recluster based on cluster specific components
#'@description Internal function for CICA package
#' @param DataList list with data matrices
#' @param SrList list with Group ICA RSN parameters
#'
#'
#' @keywords internal
#' @return \item{newclus}{a list object with an updated partitioning}
#' \item{SSminVec}{a numeric vector with the lowest loss function values per data block}
#' \item{Loss}{a numeric indicating the new loss function value}
Reclus <- function(DataList, SrList) {

  SSList <- lapply(DataList, FUN = Lir, SkL = SrList)
  SSminIndexVec <- sapply(SSList, FUN = which.min)
  SSminVec <- sapply(SSList, FUN = min)
  Loss <- sum(SSminVec)

  ResList <- list("newclus" = SSminIndexVec,'SSminVec' = SSminVec,
                  "Loss" = Loss)

  return(ResList)
}
