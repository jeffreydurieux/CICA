#' Plot method for rstarts object
#' @rdname FindRationalStarts
#' @param x an object of \code{class} rstarts
#' @param type type of plot, 1 for a dendrogram, 2 for a multidimensional scaling configuration
#' @param mdsdim 2 for two dimensional mds configuration, 3 for a three dimensional configuration
#' @param nClus Number of clusters for rectangles in dendrogram, default NULL is based on number of clusters present in the object
#' @param ... optional arguments passed to \code{hclust} function
#' @export
#'
#'
plot.rstarts <- function(x, type = 1, mdsdim = 2, nClus = NULL,...){


  if(is.null(nClus)){
    nClus <- length( unique( x$rationalstarts[,1]) )
  }else{
    nClus <- nClus
  }


  hcl <- hclust(x$RVdist, method = ...)
  k <- cutree(hcl, k = nClus)

  if(type == 1){
    plot(hcl, xlab='')
    rect.hclust(hcl, k = nClus)
  }else{
    dots <- match.call(expand.dots = F)$...
    title <- paste('Label method: ', dots[[1]])
    mds <- cmdscale(x$RVdist, k = mdsdim)
    df <- data.frame(mds, lab = as.factor(k))
    hover <- 1:nrow(df)

    if(mdsdim == 2){
      plot_ly(data = df, x=~X1, y=~X2, color = ~lab, type = 'scatter', mode = 'markers',
              text = hover, hoverinfo = 'text') %>%
        layout(title = title, yaxis = list(scaleanchor = 'x'))
    }else{
      plot_ly(data = df, x=~X1, y=~X2, z=~X3, color = ~lab, type = 'scatter3d', mode = 'markers',
              text = hover, hoverinfo = 'text', marker = list(size = 5)) %>%
        layout(title = title)
    }
  }

}



