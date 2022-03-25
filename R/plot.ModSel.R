#' Plot method for sequential model selection
#' @description Plot method for the sequential model selection option for CICA
#' @param x Object of \code{class} ModSel
#' @export
#'
#'
plot.ModSel <- function(x){
  if(class(x) != 'ModSel'){
    stop('Input object is not of class ModSel')
  }
  plot_ly(data = x$df, x = ~Q, y=~Loss, color = ~as.factor(R), mode='lines+markers', type = 'scatter')

}

