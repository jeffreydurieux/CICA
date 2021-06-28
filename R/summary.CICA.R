#' Summary method for class CICA
#' @rdname CICA
#' @param object Object of the type produced by \code{\link{CICA}}
#' @param ... Additional arguments
#'
#' @return
#' @export
#'
#' @examples
#' data('ExampleData', package = 'CICA')
#' inputdata <- ExampleData$XE
#' output <- CICA(DataList = inputdata, nStarts = 3, nComp = 5, nClus = 3, verbose = FALSE)
#' summary(output)
summary.CICA <- function(object, ...){

  names(object$P) <- 1:length(object$P)

  cat('Partitioning matrix P: \n' )
  PB <- matrix(0, nrow = length(object$P), ncol = length(unique(object$P)))
  for(i in 1:nrow(PB)){
    PB[i, object$P[i]] <- 1
  }
  colnames(PB) <- paste('Cluster',sort(unique(object$P)))

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

}
