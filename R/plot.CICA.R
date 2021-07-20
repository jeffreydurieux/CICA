#' Plot method for
#' @param what ...
#' @param res ...
#' @export
#'
#'
plot.CICA <- function(what, res){
  img <- readMNI(what, res)
  plot(img)
}

