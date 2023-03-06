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

  xsel <- x$optimalQ
  rsel <- x$optimalR
  ysel <- x$df[which(x$df$Q == xsel & x$df$R == rsel),]$Loss

  plot_ly(data = x$df, x = ~Q, y=~Loss, color = ~as.factor(R), mode='lines+markers',
          type = 'scatter') %>%
    layout(annotations = list(x = xsel, y = ysel,
                              showarrow = TRUE, text = 'selected model',
                              arrowhead = 6),
           legend = list(title=list(text='Cluster')))



}

