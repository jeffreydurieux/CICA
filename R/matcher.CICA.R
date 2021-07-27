#' Match components between cluster specific spatial maps
#'
#' @param x object of class CICA
#' @param reference integer cluster index that serves as the reference. If nifti path is supplied, clusters will be matched to this template
#' @param RV compute modified-RV between cluster components
#' @param ... other arguments
#'
#' @return out
#' @export
#'
matcher.CICA <- function(x, reference = 1, RV = FALSE, ...){

  if(RV == TRUE){
    RVs <- computeRVmat(x$Sr, dist = F, verbose = T)
  }


  if(class(reference)[1] == 'numeric'){
    ncomp <- ncol(x$Sr[[1]])
    m <- 1:ncomp
    mm <- 1:ncomp
    conList <- vector(mode = 'list', length = length(x$Sr)-1)
    toSelect <- 1:length(x$Sr)
    toSelect <- toSelect[-reference]

    for(i in 1:length(toSelect)){

      con <- cor(x$Sr[[reference]], x$Sr[[ toSelect[i]]] )
      conList[[i]] <- con
      Cluster2 <- apply(abs(con) , MARGIN = 1, which.max)
      max <- apply(abs(con) , MARGIN = 1, max)
      m <- cbind(m, Cluster2)
      mm <- cbind(mm, max)

    }

    rownames(m) <- paste('Component ', 1:ncomp)
    rownames(mm) <- paste('Component ', 1:ncomp)
    colnames(m) <- paste('Cluster ', 1:length(x$Sr))
    colnames(mm) <- paste('Cluster ', 1:length(x$Sr))
  }else if(class(reference)[1] == 'character'){

    nif <- readNifti(reference)
    nif <- matrix(nif, ncol = dim(nif)[4])
    ncomp <- ncol(nif)

    m <- 1:ncomp
    mm <- 1:ncomp
    conList <- vector(mode = 'list', length = length(x$Sr))

    for(i in 1:length(x$Sr)){
      con <- cor(nif, x$Sr[[ i ]] )
      conList[[i]] <- con
      whichm <- apply(abs(con) , MARGIN = 1, which.max)
      max <- apply(abs(con) , MARGIN = 1, max)
      m <- cbind(m, whichm)
      mm <- cbind(mm, max)
    }

    rownames(m) <- paste('Component ', 1:ncomp)
    rownames(mm) <- paste('Component ', 1:ncomp)

    name <- paste('Cluster ', 1:length(x$Sr))
    name <- c('Template Nifti', name)
    colnames(m) <- name
    colnames(mm) <- name

  }else if(class(reference)[1] == 'matrix'){
    ncomp <- ncol(reference)
    m <- 1:ncomp
    mm <- 1:ncomp
    conList <- vector(mode = 'list', length = length(x$Sr))

    for(i in 1:length(x$Sr)){
      con <- cor(reference, x$Sr[[ i ]] )
      conList[[i]] <- con
      whichm <- apply(abs(con) , MARGIN = 1, which.max)
      max <- apply(abs(con) , MARGIN = 1, max)
      m <- cbind(m, whichm)
      mm <- cbind(mm, max)
    }

    rownames(m) <- paste('Component ', 1:ncomp)
    rownames(mm) <- paste('Component ', 1:ncomp)

    name <- paste('Cluster ', 1:length(x$Sr))
    name <- c('Template ', name)
    colnames(m) <- name
    colnames(mm) <- name

  }

  out <- list()

  if(RV == TRUE){
    out$RVs <- RVs
  }

  out$matchIndexMatrix <- m
  out$matchTuckerMatrix <- mm
  out$CongruenceList <- conList
  return(out)
}
