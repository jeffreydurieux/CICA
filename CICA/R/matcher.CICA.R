#' Match components between cluster specific spatial maps
#'
#' @param x object of class CICA
#' @param reference integer cluster index that serves as the reference. If nifti path is supplied, clusters will be matched to this template
#' @param RV compute modified-RV between cluster components
#' @param ... other arguments
#'
#' @return out
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
#' matcher(multiple_output$Q_5_R_4, reference = 1, RV = TRUE)
#' }
#'
#' @importFrom multiway congru
#' @import RNifti
#'
#' @export
#'
matcher.CICA <- function(x, reference = 1, RV = FALSE, ...){
  lsr <- length(x$Sr)

  if(class(reference)[1] == 'numeric'){
    ncomp <- ncol(x$Sr[[1]])
    m <- 1:ncomp
    mm <- rep(1, ncomp)
    conList <- vector(mode = 'list', length =lsr-1)
    toSelect <- 1:lsr
    toSelect <- toSelect[-reference]

    for(i in 1:length(toSelect)){

      #con <- cor(x$Sr[[reference]], x$Sr[[ toSelect[i]]] )
      con <- congru(x$Sr[[reference]], x$Sr[[ toSelect[i]]] )
      conList[[i]] <- con
      Cluster2 <- apply(abs(con) , MARGIN = 1, which.max)
      max <- apply(abs(con) , MARGIN = 1, max)
      m <- cbind(m, Cluster2)
      mm <- cbind(mm, max)
    }
    names(conList) <- paste('Cluster ', toSelect)

    rownames(m) <- paste('Component ', 1:ncomp)
    rownames(mm) <- paste('Component ', 1:ncomp)
    colnames(m) <- paste('Cluster ', c(reference, toSelect))
    colnames(mm) <- paste('Cluster ', c(reference,toSelect))
  }else if(class(reference)[1] == 'character'){

    nif <- readNifti(reference)
    nif <- matrix(nif, ncol = dim(nif)[4])
    ncomp <- ncol(nif)

    m <- 1:ncomp
    mm <- rep(1, ncomp)
    conList <- vector(mode = 'list', length = length(x$Sr))

    for(i in 1:lsr){
      #con <- cor(nif, x$Sr[[ i ]] )
      con <- congru(nif, x$Sr[[ i ]] )

      conList[[i]] <- con
      whichm <- apply(abs(con) , MARGIN = 1, which.max)
      max <- apply(abs(con) , MARGIN = 1, max)
      m <- cbind(m, whichm)
      mm <- cbind(mm, max)
    }
    names(conList) <- paste('Cluster ', 1:lsr)

    rownames(m) <- paste('Component ', 1:ncomp)
    rownames(mm) <- paste('Component ', 1:ncomp)

    name <- paste('Cluster ', 1:length(x$Sr))
    name <- c('Template Nifti', name)
    colnames(m) <- name
    colnames(mm) <- name

  }else if(class(reference)[1] == 'matrix'){
    ncomp <- ncol(reference)
    m <- 1:ncomp
    mm <- rep(1, ncomp)
    conList <- vector(mode = 'list', length = length(x$Sr))

    for(i in 1:lsr){
      con <- cor(reference, x$Sr[[ i ]] )
      con <- congru(reference, x$Sr[[ i ]] )
      conList[[i]] <- con
      whichm <- apply(abs(con) , MARGIN = 1, which.max)
      max <- apply(abs(con) , MARGIN = 1, max)
      m <- cbind(m, whichm)
      mm <- cbind(mm, max)
    }
    names(conList) <- paste('Cluster ', 1:lsr)

    rownames(m) <- paste('Component ', 1:ncomp)
    rownames(mm) <- paste('Component ', 1:ncomp)

    name <- paste('Cluster ', 1:length(x$Sr))
    name <- c('Template ', name)
    colnames(m) <- name
    colnames(mm) <- name

  }

  out <- list()
  out$matchIndexMatrix <- m
  out$matchTuckerMatrix <- round(mm, digits = 3)
  out$CongruenceList <- lapply(conList, round, digits = 3)
  if(RV == TRUE){
    RVs <- computeRVmat(x$Sr, dist = F, verbose = T)
    out$RVs <- round(RVs, digits = 3)
  }
  return(out)
}
