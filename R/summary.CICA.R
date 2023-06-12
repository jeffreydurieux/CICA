#' Summary method for class CICA
#' @description Summarize a CICA analysis
#' @param object Object of the type produced by \code{\link{CICA}}
#' @param ... Additional arguments
#'
#' @return \code{summary.CICA} returns an overview of the estimated clustering of a \code{\link{CICA}} analysis
#' \item{PM}{Partitioning matrix}
#' \item{tab}{tabulation of the clustering}
#' \item{Loss}{Loss function value of the solution}
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
#' summary(multiple_output$Q_5_R_4)
#' }
#'
#'
#' @export
#'
#'
summary.CICA <- function(object, ...){


  cat('Partitioning matrix P: \n' )
  PB <- matrix(0, nrow = length(object$P), ncol = length(unique(object$P)))
  for(i in 1:nrow(PB)){
    PB[i, object$P[i]] <- 1
  }
  colnames(PB) <- paste('Cluster',sort(unique(object$P)))
  rownames(PB) <- names(object$P)
  cat('\n')
  print(PB)

  cat('\n')

  cat('Tabulation of clustering: \n')
  cat('\n')
  tab <- table(object$P)
  names(tab) <- paste('Cluster',sort(unique(object$P)))
  print( tab )

  cat('\n')

  cat('Loss function value of optimal solution is: ', object$Loss,'\n')

  # out <- list()
  # out$PM <- PB
  # out$tab <- tab
  # out$loss <- object$Loss
  #
  # return(out)
}
