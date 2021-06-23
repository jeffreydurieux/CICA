#' Title
#'
#' @param DataList list with data matrices
#' @param SrList list with Group ICA RSN parameters
#'
#' @return
#'
#' @keywords internal
#'
Reclus <- function(DataList, SrList) {

  SSList <- lapply(DataList, FUN = Lir, SkL = SrList)
  SSminIndexVec <- sapply(SSList, FUN = which.min)
  SSminVec <- sapply(SSList, FUN = min)
  Loss <- sum(SSminVec)

  ResList <- list("newclus" = SSminIndexVec,'SSminVec' = SSminVec,
                  "Loss" = Loss)

  return(ResList)
}
