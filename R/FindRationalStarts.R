#'
#' @param DataList a list of matrices
#' @param nComp number of ICA components to extract
#' @param nClus number of clusters
#' @param scale scale each matrix to have an equal sum of squares
#' @param center mean center matrices
#' @param pseudo default is \code{NULL}
#' @param verbose print output to console
#' @param cl cl object
#' @parallel if true ICA's are run in parallel
#'
#' @return dataframe with (pseudo-) rational and dist object based on the pairwise modified RV values
#' @export
#'
#' @examples
#' ## Not run:
#' data('CICA_data', package = 'CICA')
#' rats <- FindRationalStarts(DataList = CICA_data$X, nComp = 5, nClus = 4,verbose = TRUE, pseudo = .2)
#' plot(rats, type = 1, method = 'ward.D2')
#' plot(rats, type = 2, method = 'ward.D2')
#' plot(rats, type = 2, method = 'ward.D2', mdsdim = 3)
#' ## End(Not run)
#' @references Durieux, J., & Wilderjans, T. F. (2019). Partitioning subjects based on high-dimensional fMRI data: comparison of several clustering methods and studying the influence of ICA data reduction in big data. Behaviormetrika, 46(2), 271-311.

#'
FindRationalStarts <- function(DataList, nComp, nClus, scale = TRUE,
                               center = TRUE, verbose = TRUE, pseudo = NULL, parallel = FALSE, cl = NULL){


  if(parallel == FALSE){
    ICAs <- CICA(DataList = DataList, nStarts = 1, nComp = nComp,
                 nClus = length(DataList), scale = scale, center = center, verbose = F)
    ICAs <- ICAs$Sr
    d <- computeRVmat(DataList = ICAs, dist = TRUE, verbose = verbose)
  }else{
    #cores <- makeCluster( mc <- getOption('cl.cores', detectCores() - 1))
    custica <- function(data, ncomp){
      ica <- ica::icafast(X = data, nc = ncomp)
      return(ica$S)
    }
    ICAs <- parLapply(cl = cl, X = DataList, fun = custica, ncomp = nComp)
    d <- computeRVmat(DataList = ICAs, dist = TRUE, verbose = verbose, parallel = TRUE, cl = cl)
    #stopCluster(cores)
  }



  if(verbose == TRUE){
    cat("Hierarchical cluster analysis using Ward's method \n")
    hcl1 <- hclust(d = d, method = 'ward.D2')

    cat("Hierarchical cluster analysis using complete linkage \n")
    hcl2 <- hclust(d = d, method = 'complete')

    cat("Hierarchical cluster analysis using single linkage \n")
    hcl3 <- hclust(d = d, method = 'single')

    cat("Hierarchical cluster analysis using unweighted average linkage \n")
    hcl4 <- hclust(d = d, method = 'average')

    cat("Hierarchical cluster analysis using Weighted average linkage \n")
    hcl5 <- hclust(d = d, method = 'mcquitty')

    cat("Hierarchical cluster analysis using median linkage \n")
    hcl6 <- hclust(d = d, method = 'median')

    cat("Hierarchical cluster analysis using centroid linkage \n")
    hcl7 <- hclust(d = d, method = 'centroid')

  }else{
    hcl1 <- hclust(d = d, method = 'ward.D2')
    hcl2 <- hclust(d = d, method = 'complete')
    hcl3 <- hclust(d = d, method = 'single')
    hcl4 <- hclust(d = d, method = 'average')
    hcl5 <- hclust(d = d, method = 'mcquitty')
    hcl6 <- hclust(d = d, method = 'median')
    hcl7 <- hclust(d = d, method = 'centroid')
  }

  p_ward <- cutree(hcl1, k = nClus)
  p_comp <- cutree(hcl2, k = nClus)
  p_single <- cutree(hcl3, k = nClus)
  p_average <- cutree(hcl4, k = nClus)
  p_mcquitty <- cutree(hcl5, k = nClus)
  p_median <- cutree(hcl6, k = nClus)
  p_centroid <- cutree(hcl7, k = nClus)

  if(!is.null(pseudo)){

    if(pseudo >= 1 | pseudo <= 0){
      stop('pseudo should be a value between 0 and 1')
    }

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

    if(verbose == TRUE){
      cat("Perturbing rational starts to obtain pseudo-rational starts")
    }
    perb1 <- perturbation(p_ward, percentage = pseudo)
    perb2 <- perturbation(p_comp, percentage = pseudo)
    perb3 <- perturbation(p_single, percentage = pseudo)
    perb4 <- perturbation(p_average, percentage = pseudo)
    perb5 <- perturbation(p_mcquitty, percentage = pseudo)
    perb6 <- perturbation(p_median, percentage = pseudo)
    perb7 <- perturbation(p_centroid, percentage = pseudo)

    output <- data.frame(p_ward, p_comp, p_single, p_average, p_mcquitty,
                         p_median, p_centroid,
                         perb1, perb2, perb3, perb4, perb5, perb6, perb7)
  }else{
    output <- data.frame(p_ward, p_comp, p_single, p_average, p_mcquitty,
                         p_median, p_centroid)
  }

  # check if no empty clusters are present
  temp <- output
  for(i in 1:ncol(output)){
    temp[,i] <- SearchEmptyClusters(nClus, newcluster = output[, i])
  }

  output <- temp


  out <- list()
  out$rationalstarts <- output
  out$RVdist <- d

  class(out) <- 'rstarts'

  return(out)

}
