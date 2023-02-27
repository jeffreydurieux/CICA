# Mon Feb 27 11:03:45 2023 ------------------------------
# Script for generating a CICA example data set


library(CICA)

# Generate four Sr's
set.seed(1)

Sr <- lapply(1:4, function(x)
  replicate(n = 5, icasamp(dname = 'b', nsamp = 100,query = 'rnd')))

Ai <- lapply(1:60, function(x)
             replicate(n = 5, runif(10, min = -2, max = 2)))

X1 <- lapply(1:15, function(x) Sr[[1]] %*% t(Ai[[ x ]]))
X2 <- lapply(1:15, function(x) Sr[[2]] %*% t(Ai[[ x + 15 ]]))
X3 <- lapply(1:15, function(x) Sr[[3]] %*% t(Ai[[ x + 30]]))
X4 <- lapply(1:15, function(x) Sr[[4]] %*% t(Ai[[ x + 45]]))

X <- c(X1, X2, X3, X4)

SSequal <- function(m1,m2){(m1/sqrt(sum(m1^2)) * sqrt(sum(m2^2)))}
addError <- function(datablock, err){
    errorM<-replicate(ncol(datablock),rnorm(nrow(datablock)))
    errorM<-SSequal(errorM,datablock)
    errorlevel<-err/(1-err)
    res<-datablock + (errorM * sqrt(errorlevel))
    return(res)
}

Xe <- lapply(X, addError, err = .2)

P <- rep(1:4, each = 15)

