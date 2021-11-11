#' CICA: Clusterwise Independent Component Analysis
#'
#'@description Main function to perform Clusterwise Independent Component Analysis
#'
#' @import ica
#' @import NMFN
#' @import plotly
#' @import papayar
#' @import RNifti
#' @importFrom stats as.dist cutree hclust rect.hclust cmdscale cor
#' @importFrom utils setTxtProgressBar txtProgressBar
#
#' @param DataList a list of matrices
#' @param nStarts number of multiple starts
#' @param nComp number of ICA components per cluster
#' @param nClus number of clusters
#' @param EVD do computations with EVD only
#' @param scale scale each matrix to have an equal sum of squares
#' @param scalevalue desired sum of squares of the block scaling procedure
#' @param center mean center matrices
#' @param rational a rational starting seed, if NULL no rational starting seed is used
#' @param maxiter maximum number of iterations for each start
#' @param verbose print loss information to console
#'
#' @return \code{CICA} returns an object of \code{\link{class}} "CICA". It contains the estimated clustering, cluster specific component matrices and subject specific time course matrices
#' \item{P}{partitioning vector of size \code{length(DataList)}}
#' \item{Sr}{list of size \code{nClus}, containing cluster specific independent components}
#' \item{Ais}{list of size \code{length(DataList)}, containing subject specific time courses}
#' \item{Loss}{loss function value of the best start}
#' \item{LossStarts}{loss function values of all starts}
#'
#' @export
#'
#'@author Jeffrey Durieux
#'
#'
#' @examples
#' data('CICA_data', package = 'CICA')
#' output <- CICA(DataList = CICA_data$X, nStarts = 3, nComp = 5, nClus = 4, verbose = FALSE)
#' summary(output)
CICA <- function(DataList, nStarts, nComp, nClus, EVD = FALSE,scale = TRUE, scalevalue = 1000, center = TRUE,
                 rational = NULL, maxiter = 100, verbose = TRUE){

  #### input arguments check ####

  if (is.list(DataList) == FALSE){
    stop('Provided DataList is not a list object')
  }


  #if( all(sapply(DataList, class)[1,] == 'matrix') == FALSE){
  #  stop('Please check input DataList, elements are not matrices')
  #}

  # check if dimensions are equal
  if (all( rowSums(sapply(DataList,dim)) == dim(DataList[[1]]) * length(DataList) ) == FALSE ){
    stop('Please check input DataList, dimensions are not equal over all matrices')
  }

  if( nComp > ncol(DataList[[1]]) ){
    stop('Number of components to extract is larger than the number of variables in each data matrix')
  }


  #### Preprocessing ####

  if(center == TRUE){
    DataList <- lapply(seq_along(DataList), function(i){
      sweep(DataList[[i]], 1, rowMeans( DataList[[i]] ) )
    })
  }

  # scale datamatrices such that they have an equal sum of squares
  if(scale == TRUE){
    DataList <- lapply(DataList, FUN = xscale, value = scalevalue)
  }

  nBlocks <- length(DataList)

  # total loss
  Losstotal <- sum( sapply( seq_along(DataList),
                            function(i) sum( DataList[[i]]^2)) ) + 1

  LossStarts <- numeric()
  TempOutput <- list()


  for(st in 1:nStarts){
    if(verbose == TRUE){
      cat('Start number: ',st, '\n')
    }
    iter <- 1

    #### step 1 initialize P ####
    if(!is.null(rational)){

      if(class(rational) == 'rstarts'){

        if(st <= dim(rational$rationalstarts)[2]){
          if(verbose == TRUE){
            cat('Type of start: Rational \n')
          }
          newclus <- rational$rationalstarts[,st]
        }else{
          if(verbose == TRUE){
            cat('Type of start: Random \n')
          }
          newclus <- clusf(nBlocks, nClus)
        }
      }else{
        if(st == 1){
          if(verbose == TRUE){
            cat('Type of start: Rational \n')
          }
          newclus <- rational
        }else{
          if(verbose == TRUE){
            cat('Type of start: Random \n')
          }
          newclus <- clusf(nBlocks, nClus)
        }
      }
    }else{
      if(verbose == TRUE){
        cat('Type of start: Random \n')
      }
      newclus <- clusf(nBlocks, nClus)
    }


    repeat{
      # and concatenate datablocks according to clustering
      SortedDataList <- ConcData(DataList = DataList, ClusVec = newclus)

      #### Step 2 extract group ICA parameters (only Sr is necessary ####

      ICAparams <- ExtractICA(DataList = SortedDataList, nComp = nComp, EVD=EVD)

      #### Step 3 update P ####
      UpdatedPInfo <- Reclus(DataList = DataList, SrList = ICAparams$Sr)

      # check empty clusters
      newclus <- SearchEmptyClusters(nClus = nClus,
                                     newcluster = UpdatedPInfo$newclus,
                                     SSminVec = UpdatedPInfo$SSminVec)

      if(length(unique(newclus)) != nClus & verbose == TRUE){
        cat('empty cluster, checkempties\n')
      }

      # add new loss to lossvector
      if(iter == 1){
        Loss <- c(Losstotal, UpdatedPInfo$Loss)
      }else{
        Loss <- c(Loss, UpdatedPInfo$Loss)
      }

      if(verbose == TRUE){
        cat(Loss,'\n')
      }


      #### step 4 convergence ####
      iter <- iter + 1
      if( Loss[iter-1] - Loss[iter]  < .000001 | iter == maxiter ){
        if(EVD == TRUE){
          SortedDataList <- ConcData(DataList = DataList, ClusVec = UpdatedPInfo$newclus)

          ICAparams <- ExtractICA(SortedDataList,
                                  nComp = nComp, EVD = FALSE )
        }
        if(verbose == TRUE){
          if(iter == maxiter){
            cat('Maximum number of iterations reached \n')
            cat('\n')
          }else{
            cat('Convergence \n')
            cat('\n')
          }
        }
        break()
      }

    }# end repeat

    LossStarts[st] <- Loss[iter]

    # procedure to only save current best start on first position of TempOutput
    if(st == 1){
      TempOutput$`1`$P <- newclus
      TempOutput$`1`$Sr <- ICAparams$Sr
      TempOutput$`1`$Loss <- utils::tail(LossStarts, n = 1)
    }else if(st >= 2){
      TempOutput$`2`$P <- newclus
      TempOutput$`2`$Sr <- ICAparams$Sr
      TempOutput$`2`$Loss <- utils::tail(LossStarts, n = 1)

      if(TempOutput$`2`$Loss <= TempOutput$`1`$Loss){
        TempOutput$`1`$P <- TempOutput$`2`$P
        TempOutput$`1`$Sr <- TempOutput$`2`$Sr
        TempOutput$`1`$Loss <- TempOutput$`2`$Loss
      }
    }

  }# end for nstarts

  #### output ####

  P <- TempOutput$`1`$P
  Sr <- TempOutput$`1`$Sr
  Ais <- lapply( seq_along(DataList), function(anom){
    crossprod(DataList[[anom]], Sr[[ P[anom] ]]) %*% NMFN::mpinv( t(Sr[[ P[anom] ]]) %*% Sr[[ P[anom] ]])
  })

  output <- list()
  output$P <- P
  output$Sr <- Sr
  output$Ais <- Ais
  output$Loss <- TempOutput$`1`$Loss
  output$LossStarts <- LossStarts


  class(output) <- 'CICA'
  return(output)

}
