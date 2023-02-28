#' Summary method for class MultipleCICA
#' @description Summarize a CICA analysis
#' @param object Object of the type produced by \code{\link{CICA}}
#' @param ... Additional arguments
#'
#' @return \code{summary.MultipleCICA} returns an overview of the estimated clustering of a \code{\link{CICA}} analysis
#' \item{PM}{Partitioning matrix}
#' \item{tab}{tabulation of the clustering}
#' \item{Loss}{Loss function value of the solution}

#' @export
#'
#' @examples
#' data('CICA_data', package = 'CICA')
#' output <- CICA(DataList = CICA_data$X, nStarts = 3, nComp = 5, nClus = 4, verbose = FALSE)
#' summary(output)
summary.MultipleCICA <- function(object, ...){

  cat('MultipleCICA object, Sequential Scree procedure used to determine optimal model\n')
  modsel <- SequentialScree(object)
  id <- paste('Q',modsel$optimalQ,'R',modsel$optimalR, sep = '_')
  cat('Optimal model: ', id, '\n')
  id <- which(id == names(object))
  object <- object[[id]]




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
