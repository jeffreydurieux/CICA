# # Thu Mar 31 16:15:57 2022
# # Author: Jeffrey Durieux, MSc
#
# # CICA using only svd using rsvd
#
# # ICA and svd --> center data and they have same SSQ
#
# library(ica)
# library(multiway)
# library(plotly)
# library(microbenchmark)
# library(rsvd)
#
# addError<-function(datablock,error, type = "Gaussian", additiontype = 1)
# {
#   if(type == "Gaussian"){
#     errorM<-replicate(ncol(datablock),rnorm(nrow(datablock)))
#   }else if(type == "AR"){
#     nscan <- ncol(datablock)
#     vdim <- nrow(datablock)^(1/3)
#     dim <- rep(vdim, 3)
#     errorM <- neuRosim::temporalnoise(dim = dim, nscan = nscan, sigma = 1,rho = 0.2)
#     errorM <- matrix(errorM, ncol = nscan)
#   }
#
#   errorM<-SSequal(errorM,datablock)
#   errorlevel<-error/(1-error)
#
#   if(additiontype == 1){
#     res<-datablock + (errorM * sqrt(errorlevel))
#   }else{
#     res<-datablock + (errorM * errorlevel)
#   }
#   return(res)
# }
#
# SSequal<-function(m1,m2)
# {
#   #res<-(m1/sqrt(SSsuga(m1)) * sqrt(SSsuga(m2))) #c++
#   res<-(m1/sqrt(sum(m1^2)) * sqrt(sum(m2^2))) #R
#   return(res)
# }
#
#
# nobs <- 100000
# Amat <- cbind(icasamp("c","rnd",nobs),icasamp("c","rnd",nobs),icasamp("c","rnd",nobs), icasamp("c","rnd",nobs) )
# Bmat <- matrix(2*runif(8),4,4)
# Xmat <- tcrossprod(Amat,Bmat)
# X <- Xmat
# rm(Amat,Bmat,Xmat)
# X <- addError(X, error = .01)
# X <- scale(X, center = T)
# Q = 2
#
#
# microbenchmark(icafast(X, nc = Q), rsvd(X, k = Q), svd(X), times = 100)
#
# s <- rsvd(X)
# Ff5 <- s$u[,1:Q] %*% diag(s$d[1:Q]) #component scores
#
# #mixing matrix regression step
# At5 <- solve(t(Ff5)%*%Ff5) %*% t(Ff5) %*% X
# At5
# t(s$v[,1:Q])
# sum( (X - Ff5%*%At5)^2) %>% round(digits = 3)
