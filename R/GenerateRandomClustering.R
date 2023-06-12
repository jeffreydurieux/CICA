#' Generate random clustering
#' @description Internal function for CICA package
#' @param nElement a positive integer number of elements to be clusterd
#' @param nClust a positive integer: Number of clusters
#' @param Prob numeric vector: Portion of elements in each cluster
#'
#'
#'@keywords internal
#'
#' @return a numeric vector indicating the clustering
GenerateRandomClustering <- function(nElement , nClust , Prob = NULL)
{
  ####GenerateRandomClustering = for Random Starts

  # Author: Tom F. Wilderjans
  # nElement: number of elements to be clustered
  # nClust: number of clusters
  # Prob (1 x nClust): proportion of elements in each cluster

  # Added by Jeffrey Durieux: default Prob = equal cluster prob
  # This done to adjust code later on for potential cluster perbutation

  if(is.null(Prob))
  {
    Prob <- rep(x = (1/nClust) , nClust)
  }


  BestClust = NULL
  ErrorEncountered = F

  if (!(length(Prob) == nClust))
  {
    ErrorEncountered = T
  }

  if ((abs(sum(Prob) - 1) > .000000001) | (any(Prob < 0)))
  {

    ErrorEncountered = T
  }

  if (!(any(nClust == 1:nElement)))
  {
    ErrorEncountered = T
  }

  if (!(ErrorEncountered))
  {
    if (nElement > nClust)
    {
      if (nClust == 1)
      {
        BestClust = rep(1 , times = nElement)
      }
      else
      {
        ProbVV = round(Prob * nElement)
        if (!(sum(ProbVV) == nElement) |
            (any(ProbVV < 1)))
          #not enough elements, or empty clusters
        {
          ProbVV = AdjustProb(ProbVV , nElement)
        }

        tempclus = rep(1:length(ProbVV) , ProbVV)
        BestClust = tempclus[sample(1:nElement,size = nElement,replace =
                                      FALSE)]
      }
    }
    else
    {
      BestClust = 1:nClust
    }
  }

  if (!(length(unique(BestClust)) == nClust))
  {
    BestClust = NULL
  }

  return(BestClust)
}
