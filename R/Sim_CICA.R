#' Simulate CICA data
#'
#' @param Nr number of subjects per cluster
#' @param Q number of components
#' @param R number of clusters
#' @param voxels number of voxels
#' @param timepoints number of time points
#' @param E proportion of independent gaussian noise
#' @param overlap amount of overlap between S across clusters. Smaller value means more overlap
#'
#' @return a list with simulated CICA data
#' @export
#'
#' @examples Xe <- Sim_CICA(Nr = 15, Q = 5, R = 4, voxels = 100, timepoints = 10, E = .2)

Sim_CICA <- function(Nr, Q, R, voxels, timepoints, E, overlap=NULL){
  if(!is.null(overlap)){
    Sbase <- replicate(n = Q, runif(n = voxels, min = -1, max = 1))
    S <- lapply(1:R, function(x)
      replicate(n = Q, runif(n = voxels, min = -overlap, max = overlap)))

    Sr <- lapply(seq_along(S), function(x) Sbase + S[[x]])

    RVs <- computeRVmat(DataList = Sr, dist = FALSE, verbose = FALSE)

  }else{
    Sr <- lapply(1:R, function(x)
      replicate(n = Q, icasamp(dname = 'b', nsamp = voxels,query = 'rnd')))
  }

  Air <- lapply(1:R, FUN = function(x) lapply(1:Nr, function(x)
    replicate(n = Q, runif(timepoints, min = -2, max = 2))))

  Xs <- list()
  for(i in 1:length(Air)){
    Xs[[i]] <- lapply(1:length(Air[[i]]), function(x) Sr[[i]] %*% t(Air[[i]][[x]]))
  }

  X <- do.call(c, Xs)

  SSequal <- function(m1,m2){(m1/sqrt(sum(m1^2)) * sqrt(sum(m2^2)))}
  addError <- function(datablock, err){
    errorM<-replicate(ncol(datablock),rnorm(nrow(datablock)))
    errorM<-SSequal(errorM,datablock)
    errorlevel<-err/(1-err)
    res<-datablock + (errorM * sqrt(errorlevel))
    return(res)
  }

  Xe <- lapply(X, addError, err = E)

  P <- rep(1:R, each = Nr)
  out <- list()
  out$P <- P
  out$X <- Xe
  out$Sr <- Sr
  out$Air <- Air

  if(!is.null(overlap)){
    out$RVs <- RVs
  }
  return(out)
}









