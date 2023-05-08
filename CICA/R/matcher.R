#' Match components between cluster specific spatial maps
#'
#' @param x object of class CICA
#' @param reference integer cluster index that serves as the reference. If nifti path is supplied, clusters will be matched to this template
#' @param RV compute modified-RV between cluster components
#' @param ... other arguments
#'
#' @return out
#'
#' @examples
#' \dontrun{
#' CICA_data <- Sim_CICA(Nr = 15, Q = 5, R = 4, voxels = 100, timepoints = 10,
#' E = 0.4, overlap = .25, externalscore = TRUE)
#'
#' multiple_output = CICA(DataList = CICA_data$X, nComp = 2:6, nClus = 1:5,
#' userGrid = NULL, RanStarts = 30, RatStarts = NULL, pseudo = c(0.1, 0.2),
#' pseudoFac = 2, userDef = NULL, scalevalue = 1000, center = TRUE,
#' maxiter = 100, verbose = TRUE, ctol = .000001)
#'
#' matcher(multiple_output$Q_5_R_4, reference = 1, RV = TRUE)
#' }
#'
#' @export
#'
#'
matcher <- function(x, reference, RV = FALSE, ...){
  UseMethod('matcher')
}
