#' AdjustProb
#'
#' @param v a numeric vector of probabilities of length nClust
#' @param MaxElem number of elements
#'
#' @keywords internal
#'
#' @return a numeric vector with adjusted probabilities
AdjustProb <- function(v , MaxElem)
{
  # INPUT
  #   v (1 x nElem): vector
  #   MaxElem: number of elements
  #
  # OUTPUT
  #   v (1 x nElem): vector with 'sum(out)=MaxElem' and no 0-cells

  nElem = length(v)

  if (any(v < 1))
    #add 1 to 0-cells
  {
    tempv = rep(0,nElem)
    tempv[v < 1] = rep(1 , length(which(v < 1)))
    v = v + tempv
  }#### replace(v,which(v<1),1)

  it <- 1
  while (!(sum(v) == MaxElem) | it == 500000)
  {
    if(it == 499999){
      stop('No suitable start clustering found, select fewer clusters or select clusters as max number of objects (i.e., length(DataList)')
    }

    it <- it + 1
    diff = sum(v) - MaxElem
    if (diff < 0)
      #add elements
    {
      if (abs(diff) < (nElem - sum(v == 1)))
      {
        for (tel in 1:abs(diff))
        {
          tempcl = ceiling(runif(1) * nElem)
          while (v[tempcl] == 1)
          {
            tempcl = ceiling(runif(1) * nElem)
          }
          v[tempcl] = v[tempcl] + 1
        }
      }
      else
      {
        v[which(!(v == 1))] = v[which(!(v == 1))] + rep(1 , length(which(!(v == 1))))
      }
    }
    else
      #delete elements
    {
      if (abs(diff) < (nElem - sum(v == 1)))
      {
        for (tel in 1:abs(diff))
        {
          tempcl = ceiling(runif(1) * nElem)
          while (v[tempcl] == 1)
          {
            tempcl = ceiling(runif(1) * nElem)
          }
          v[tempcl] = v[tempcl] - 1
        }
      }
      else
      {
        v[which(!(v == 1))] = v[which(!(v == 1))] - rep(1 , length(which(!(v == 1))))
      }
    }
  }

  return(v)
}

