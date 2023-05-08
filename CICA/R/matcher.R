#' Match components between cluster specific spatial maps
#'
#' @param x object of class CICA
#' @param reference integer cluster index that serves as the reference. If nifti path is supplied, clusters will be matched to this template
#' @param RV compute modified-RV between cluster components
#' @param ... other arguments
#'
#' @return out
#' @export
#'
#'
matcher <- function(x, reference, RV = FALSE, ...){
  UseMethod('matcher')
}
