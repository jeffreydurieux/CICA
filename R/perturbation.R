#' Perturbate a clustering
#'
#' @param p a clustering vector
#' @param percentage percentage (in proportions)
#'
#' @return peturbated clustering vector
#' @keywords internal

perturbation <- function(p, percentage = 0.1){

  clusters <- sort(unique(p))
  sel <- ceiling(length(p) * percentage )
  selected <- sample(1:length(p), size = sel, replace = F)

  if(length(selected) == 1){
    # change one cluster
    oriclus <- p[selected]
    newclus <- which(clusters != oriclus)

    if(length(newclus) > 1){
      newclus <- sample(newclus, size = 1)
    }

    np <- replace(p, selected, newclus)

  }else{
    # change multiple clusters
    np <- p
    for(i in 1:length(selected)){
      oriclus <- p[selected[i]]
      newclus <- which(clusters != oriclus)

      if(length(newclus) > 1){
        newclus <- sample(newclus, size = 1)
      }

      np <- replace(np, selected[i], newclus)
    }
  }
  return(np)
}
