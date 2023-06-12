#' Search for empty clusters
#'@description Internal function for CICA package
#' @param oldcluster previous clustering vector P
#' @param newcluster newly updated clustering vector P
#' @param SSminvec loss function values per data block
#'
#' @keywords internal
#' @return a numeric vector indicating the new partitioning vector without empty clusters
SearchEmptyClusters <- function(nClus, newcluster, SSminVec = NULL) {

  if(is.null(SSminVec)){
    SSminVec <- rep(100, length(newcluster))
  }

  OriCluster <- 1:nClus

  test <- sapply(OriCluster, FUN = '%in%', newcluster)

  #test result = no empty clusters so return original newcluster

  if ( all( test == TRUE) ){
    newcluster <- newcluster
  }else{

    # check singletons, these are not candidates to choose from
    EmptyClusters <- which(test == FALSE)
    singletonnames <- names(which( (table(newcluster)  == 1) == TRUE))
    singletons <- as.numeric(singletonnames)
    id <- which(newcluster %in% singletons == T)

    SSminVec[id] <- 0


    worst <- sort( SSminVec, decreasing = TRUE)

    #remove worst of singletons, otherwise empties will occur

    Index <- sapply( seq_along(EmptyClusters),
                     function(i) FUN = which( SSminVec == worst[i] ) )

    # if ties occur in SSminVec, pick one at random
    if( is.null(ncol(Index)) == FALSE ){
      Index <- Index[,1]
      Index <- sample(Index, size = length(EmptyClusters))
    }

    for(i in 1:length(Index)){
      newcluster <- replace(newcluster, Index[i], EmptyClusters[i])

    }


  }# else some emptyclusters

  if( length(unique(newcluster)) != nClus ){
    stop('In function SearchEmptyCluster, empty/ies occurred')
  }


  # check min number of clusters
  # if(!is.null(minclus)){
  #   test2 <- table(newcluster) < minclus
  #
  #   if ( all( test2 == TRUE) ){
  #     newcluster <- newcluster
  #   }else{
  #     SparseClusters <- which(test2 == TRUE)
  #
  #     worst <- sort( SSminVec, decreasing = TRUE)
  #
  #     Index <- sapply( seq_along(1:minclus),
  #                      function(i) FUN = which( SSminVec == worst[i] ) )
  #     newcluster[Index] <- SparseClusters
  #   }
  # }



  return(newcluster)
}
