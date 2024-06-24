#' CICA: Clusterwise Independent Component Analysis
#'
#'@description Main function to perform Clusterwise Independent Component Analysis
#'
#' @import ica
#' @importFrom plotly plot_ly
#' @import RNifti
#' @importFrom stats as.dist cutree hclust rect.hclust cmdscale cor runif
#' @importFrom utils setTxtProgressBar txtProgressBar combn tail
#' @importFrom mclust adjustedRandIndex
#' @importFrom methods hasArg
#
#' @param DataList a list of matrices
#' @param RanStarts number of random starts
#' @param RatStarts Generate rational starts. Either 'all' or a specific linkage method name (e.g., 'complete'). Use NULL to indicate that Rational starts should not be used.
#' @param pseudo  percentage value for perturbating rational starts to obtain pseudo rational starts
#' @param pseudoFac factor to multiply the number of rational starts (7 in total) to obtain pseudorational starts
#' @param nComp number or vector of ICA components per cluster
#' @param nClus number or vector of clusters
#' @param method Component method, default is \code{fastICA}. \code{SCA} for Simultaneous Component Analysis (SCA-p version, no rotation)
#' @param userGrid user supplied data.frame for multiple model CICA. First column are the requested components. Second column are the requested clusters
#' @param scalevalue desired sum of squares of the block scaling procedure
#' @param center mean center matrices
#' @param userDef a user-defined starting seed stored in a data.frame, if NULL no userDef starting partition is used
#' @param maxiter maximum number of iterations for each start
#' @param verbose print loss information to console
#' @param ctol tolerance value for convergence criterion
#' @param checks boolean parameter that indicates whether the input checks should be run (TRUE) or not (FALSE).
#'
#' @return \code{CICA} returns an object of \code{\link{class}} "CICA". It contains the estimated clustering, cluster specific component matrices and subject specific time course matrices
#' \item{P}{partitioning vector of size \code{length(DataList)}}
#' \item{Sr}{list of size \code{nClus}, containing cluster specific independent components}
#' \item{Ais}{list of size \code{length(DataList)}, containing subject specific time courses}
#' \item{Loss}{loss function value of the best start}
#' \item{FinalLossDiff}{value of the loss difference between the last two iterations of the algorithm.}
#' \item{IndLoss}{a vector with containing the individual loss function values}
#' \item{LossStarts}{loss function values of all starts}
#' \item{Iterations}{Number of iterations}
#' \item{starts}{dataframe with the used starting partitions}
#'
#'
#'
#' @author Jeffrey Durieux
#'
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
#' summary(multiple_output$Q_5_R_4)
#'
#' plot(multiple_output$Q_5_R_4)
#' }
#'
#' @export
#'
CICA <- function(DataList, nComp, nClus, method = 'fastICA', RanStarts, RatStarts=NULL, pseudo=NULL, pseudoFac, userDef = NULL,  userGrid = NULL, scalevalue = 1000, center = TRUE, maxiter = 100, verbose = TRUE, ctol = .000001, checks = TRUE){

  #### input arguments check ####
  
  if(is.null(names(DataList))){
    filenames <- 1:length(DataList)
  }else{
    filenames <- names(DataList)
  }
  
  # Feasible randomstart number check (limit is 10% of Stirling number of the 2nd kind)
  if(checks==TRUE){

  if(RanStarts > 1){
    m <- 0
    minClus <- min(nClus)
    if(minClus == 1){
      minClus <- 2
    }
    for (j in 0:minClus){
      m <- m + (-1)^(minClus - j) * choose(minClus, j) * j^length(DataList)
    }
    stirNum <- m/factorial(minClus)

    if(RanStarts >= stirNum*.1){
      stop('Too many RanStarts requested, lower the number of RanStarts')
    }
  }

  # if(exists("pseudoFac")){
  #   if(!is.null(pseudoFac)){
  #     if(length(pseudoFac)>1){
  #       stop("pseudoFac must be a unique value, not a vector")
  #     }
  #   }
  # }

  if(!is.null(RatStarts)){
    if(length(RatStarts)>1){
      stop("Provide a unique method or use 'all' if you want to use all the available methods")
    }
    METHODS <- c("ward.D", "single", "complete", "average", "mcquitty",
                 "median", "centroid", "ward.D2", 'all')
    i.meth <- pmatch(RatStarts, METHODS)
    if(is.na(i.meth)){
      stop('Invalid RatStarts argument')
    }
  }



  if(is.null(userGrid)){
    if(hasArg(nComp) == FALSE){
      stop('nComp or userGrid not provided')
    }
    if(hasArg(nClus) == FALSE){
      stop('nClus or userGrid not provided')
    }
  }

  if (is.list(DataList) == FALSE){
    stop('Provided DataList is not a list object')
  }

  if(!is.null(pseudo)){

    if(any(pseudo==0)){#Pseudo==0 does not make sense. Transform it to 0
      pseudo<-NULL
      warning("0 is not a possible value for pseudo. Pseudo-rational starts are not computed.")
    }

    if(all(pseudo > 0 & pseudo <=1) == FALSE){
      stop('pseudo should be a value between 0 and 1')
    }
  }

  if(!is.null(userDef) && length(nClus)!=1){
    stop("User-defined partitions are only allowed when a unique value for the number of clusters is provided")
  }



  #if( all(sapply(DataList, class)[1,] == 'matrix') == FALSE){
  #  stop('Please check input DataList, elements are not matrices')
  #}

  # check if dimensions are equal
  if (all( rowSums(sapply(DataList,dim)) == dim(DataList[[1]]) * length(DataList) ) == FALSE ){
    stop('Please check input DataList, dimensions are not equal over all matrices')
  }


  if(!is.null(userDef)){
    userDef<-as.matrix(userDef, nrow = NROW(DataList))
    for (i in 1:NCOL(userDef)) {
      udCluters<-length(unique(userDef[,i]))
      if(udCluters!=nClus){
        stop(paste0("The number of clusters provided in the user defined partition ", i," is not equal to the number of clusters requested"))
      }
    }
  }

  if (!(method %in% c('fastICA','SCA'))){
    stop('Provided method is not fastICA or SCA')
  }  
    
  #if( nComp > ncol(DataList[[1]]) ){
  #  stop('Number of components to extract is larger than the number of variables in each data matrix')
  #}

  }#end checking input arguments

  #### Preprocessing ####

  if(center == TRUE){
    DataList <- lapply(seq_along(DataList), function(i){
      sweep(DataList[[i]], 1, rowMeans( DataList[[i]] ) )
    })
  }

  # scale datamatrices such that they have an equal sum of squares
  if(!is.null(scalevalue)){
    DataList <- lapply(DataList, FUN = xscale, value = scalevalue)
  }

  nBlocks <- length(DataList)

  
  # if SCA is used: first compute cov
  if(method == 'SCA'){
    groups <- rep(1:nBlocks, each = (nBlocks*ncol(DataList[[1]]))/(nBlocks))
    indexList <- split(1:(nBlocks*ncol(DataList[[1]])), groups)
    
    XsL <- ConcData(DataList, rep(1, length(CICA_data$P)))
    XsL <- XsL[[1]]
    covL <- Rfast::cova(XsL, large = TRUE)
  }
  
  
  ##### multiple models part ########
  if(!is.null(userGrid) ){
    if(!is.data.frame(userGrid)){
      stop('Provided userGrid is not a data.frame object')
    }
    if(ncol(userGrid) != 2){
      stop('Structure of provided userGrid is not correct')
    }
    colnames(userGrid) <- c('nComp', 'nClus')
    grid <- userGrid
  }else if((length(nComp) != 1 | length(nClus) != 1) & is.null(userGrid) ){
    grid <- expand.grid(nComp=nComp, nClus=nClus)
  }else{
    grid <- data.frame(nComp,nClus)
  }


  # total loss
  Losstotal <- sum( sapply( seq_along(DataList),
                            function(i) sum( DataList[[i]]^2)) ) + 1

  multioutput <- list()
  for(ng in 1:nrow(grid)){

    LossStarts <- numeric()
    FinalLossDiff <- numeric()
    TempOutput <- list()

    if(grid$nClus[ng]==1){
      nS <- 1
      startvecs <- matrix(rep(1,length(DataList)))
    }else{

      startvecs <- NULL

      ##### define userDef, rational and random starts ####
      if(!is.null(userDef)){

        startvecs <- userDef
        colnames(startvecs)<-paste0("UserDefined",1:NCOL(userDef))
      }

      if(RanStarts != 0){
        randomstarts <- GenRanStarts(RanStarts = RanStarts,
                                            nClus = grid$nClus[ng],
                                            nBlocks = length(DataList),
                                            ARIlim = .2, itmax = 1000,
                                            verbose = verbose)
        if(is.null(startvecs)){
          startvecs <- randomstarts$rs
        }else{
          startvecs <- cbind(startvecs, randomstarts$rs)
        }

      }
      if(!is.null(RatStarts)){
        rationalstarts <- GenRatStarts(DataList = DataList,RatStarts = RatStarts, nComp = grid$nComp[ng],
                                              nClus = grid$nClus[ng],
                                              scalevalue = scalevalue,
                                              center = center,verbose = verbose,
                                              pseudo = pseudo , pseudoFac = pseudoFac)

        if(is.null(startvecs)){
          startvecs <- rationalstarts$rat$rationalstarts
        }else{
          startvecs <- cbind(startvecs, rationalstarts$rat$rationalstarts)
        }
      }



      nS <- ncol(startvecs)
    }

    for(st in 1:nS){
      if(verbose == TRUE){
        cat('Computing Q ', grid$nComp[ng], ' and R ', grid$nClus[ng], fill = TRUE)
        cat('Start number: ',st, '\n')
      }
      iter <- 1

      #### step 1 initialize P ####
      newclus <- startvecs[ ,st]
      # if(!is.null(userDef)){
      #
      #   if(class(userDef) == 'rstarts'){
      #
      #     if(st <= dim(userDef$rationalstarts)[2]){
      #       if(verbose == TRUE){
      #         cat('Type of start: userDef \n')
      #       }
      #       newclus <- userDef$rationalstarts[,st]
      #     }else{
      #       if(verbose == TRUE){
      #         cat('Type of start: Random \n')
      #       }
      #       newclus <- clusf(nBlocks, grid$nClus[ng])
      #     }
      #   }else{
      #     if(st == 1){
      #       if(verbose == TRUE){
      #         cat('Type of start: Rational \n')
      #       }
      #       newclus <- userDef
      #     }else{
      #       if(verbose == TRUE){
      #         cat('Type of start: Random \n')
      #       }
      #       newclus <- clusf(nBlocks, grid$nClus[ng])
      #     }
      #   }
      # }else{
      #   if(verbose == TRUE){
      #     cat('Type of start: Random \n')
      #   }
      #   newclus <- clusf(nBlocks, grid$nClus[ng])
      # }


      repeat{
        # and concatenate datablocks according to clustering
        SortedDataList <- ConcData(DataList = DataList, ClusVec = newclus)

        #### Step 2 extract group ICA parameters (only Sr is necessary ####
        if(method == 'SCA'){
          idl <- lapply(seq_along(1:nClus), function(i) c(indexList[newclus==i]))
          idl <- lapply(seq_along(idl), function(i) unlist(idl[[i]]))
        }else{
          covL <- NULL
          idl <- NULL
        }
        ICAparams <- ExtractICA(DataList = SortedDataList, nComp = grid$nComp[ng], method, covL, indexList = idl)

        #### Step 3 update P ####
        UpdatedPInfo <- Reclus(DataList = DataList, SrList = ICAparams$Sr)

        # check empty clusters
        newclus <- SearchEmptyClusters(nClus = grid$nClus[ng],
                                       newcluster = UpdatedPInfo$newclus,
                                       SSminVec = UpdatedPInfo$SSminVec)

        # UpdatedPInfo$Loss <- sum(sapply(seq_along(UpdatedPInfo$SSList), function(i) UpdatedPInfo$SSList[[i]][newclus[i]]))

        if(length(unique(newclus)) != grid$nClus[ng] & verbose == TRUE){
          cat('empty cluster, checkempties\n')
        }

        # add new loss to lossvector
        if(iter == 1){
          Loss <- c(Losstotal, UpdatedPInfo$Loss)
        }else{
          Loss <- c(Loss, UpdatedPInfo$Loss)
        }

        if(verbose == TRUE){
          cat(Loss[-1],'\n')
        }


        #### step 4 convergence ####
        iter <- iter + 1
        if( Loss[iter-1] - Loss[iter]  < ctol | iter == maxiter ){

          if(verbose == TRUE){
            if(iter == maxiter){
              cat('Maximum number of iterations reached \n')
              cat('\n')
              if( Loss[iter-1] - Loss[iter] >= ctol ){
                cat('Convergence not reached \n')
                cat('\n')
              }

            }else{
              cat('Convergence \n')
              cat('\n')
            }
          }
          break()
        }

      }# end repeat

      LossStarts[st] <- Loss[iter]
      FinalLossDiff[st] <- Loss[iter-1] - Loss[iter]

      # procedure to only save current best start on first position of TempOutput
      if(st == 1){
        TempOutput$`1`$P <- newclus
        TempOutput$`1`$Sr <- ICAparams$Sr
        TempOutput$`1`$Loss <- tail(LossStarts, n = 1)
        TempOutput$`1`$FinalLossDiff <- tail(FinalLossDiff, n = 1)
        TempOutput$`1`$iterations <- iter - 1
        TempOutput$`1`$SSmin <- UpdatedPInfo$SSminVec
      }else if(st >= 2){
        TempOutput$`2`$P <- newclus
        TempOutput$`2`$Sr <- ICAparams$Sr
        TempOutput$`2`$Loss <- tail(LossStarts, n = 1)
        TempOutput$`2`$FinalLossDiff <- tail(FinalLossDiff, n = 1)
        TempOutput$`2`$iterations <- iter - 1
        TempOutput$`2`$SSmin <- UpdatedPInfo$SSminVec

        if(TempOutput$`2`$Loss <= TempOutput$`1`$Loss){
          TempOutput$`1`$P <- TempOutput$`2`$P
          TempOutput$`1`$Sr <- TempOutput$`2`$Sr
          TempOutput$`1`$Loss <- TempOutput$`2`$Loss
          TempOutput$`1`$FinalLossDiff <-TempOutput$`2`$FinalLossDiff
          TempOutput$`1`$iterations <- iter - 1
          TempOutput$`1`$SSmin <- TempOutput$`2`$SSmin
        }
      }

    }# end for nstarts
    #### output ####

    P <- TempOutput$`1`$P
    Sr <- TempOutput$`1`$Sr
    Ais <- lapply( seq_along(DataList), function(anom){
      crossprod(DataList[[anom]], Sr[[ P[anom] ]]) %*% mpinv( t(Sr[[ P[anom] ]]) %*% Sr[[ P[anom] ]])
    })

    # if(is.null(names(DataList))){
    #   names(P) <- 1:length(DataList)
    # }else{
    #   cat('test')
    #   names(P) <- names(DataList)
    # }
    names(P) <- filenames

    output <- list()
    output$P <- P
    output$Sr <- Sr
    output$Ais <- Ais
    output$Loss <- TempOutput$`1`$Loss
    output$FinalLossDiff <- TempOutput$`1`$FinalLossDiff
    output$IndLoss <- TempOutput$`1`$SSmin
    output$LossStarts <- LossStarts
    output$iterations <- TempOutput$`1`$iterations
    output$starts <- startvecs


    class(output) <- 'CICA'
    multioutput[[ng]] <- output
  }# end for ng - grid

  gridnames <- paste('Q_', grid$nComp,'_R_', grid$nClus, sep = '')
  names(multioutput) <- gridnames

  if(length(multioutput) > 1){
    class(multioutput) <- 'MultipleCICA'
  }else{
    multioutput <- multioutput[[1]]
  }


  return(multioutput)

}
