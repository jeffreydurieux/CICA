#' Plot method for sequential model selection
#' @description Plot method for the sequential model selection option for CICA
#' @param x Object of \code{class} ModSel
#' @param ... other arguments
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
#' ModSelOutput <- SequentialScree(multiple_output)
#'
#' plot(ModSelOutput)
#' }
#'
#' @importFrom plotly plot_ly
#' @importFrom methods is
#'
#' @export
#'
#'
plot.ModSel <- function(x,...){

  if(!is(x,'ModSel')){ #equivalent to if(class(x) != 'ModSel'){
    stop('Input object is not of class ModSel')
  }

  if(length(x$optimalQ)==0){
    stop("Scree values were not computed. Check if you provided at least 3 values for the nComp input argument of CICA")
  }

  xsel <- x$optimalQ
  rsel <- x$optimalR
  ysel <- x$df[which(x$df$Q == xsel & x$df$R == rsel),]$Loss

  plot_ly(data = x$df, x = ~Q, y=~Loss, color = ~as.factor(R), mode='lines+markers',
          type = 'scatter') %>%
    add_trace(x = ~xsel, y = ~ysel,
              marker = list(symbol = 18, size = 10), showlegend=F) %>%
    layout(
      #annotations = list(x = xsel, y = ysel,
       #                       showarrow = TRUE, text = 'selected model',
        #                      arrowhead = 6),
           legend = list(title=list(text='Cluster')))



}

