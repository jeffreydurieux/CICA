# library(clue)
#
# x <- matrix(c(5, 1, 4, 3, 5, 2, 2, 4, 4), nrow = 3)
# x
# solve_LSAP(x)
# solve_LSAP(x, maximum = TRUE)
# ## To get the optimal value (for now):
# y <- solve_LSAP(x)
# sum(x[cbind(seq_along(y), y)])
#
# library(ica)
#
# set.seed(123)
# nobs <- 1000
# Smat <- cbind(icasamp("b","rnd",nobs),icasamp("b","rnd",nobs), icasamp('b','rnd',nobs),
#               icasamp("b","rnd",nobs),icasamp("b","rnd",nobs), icasamp('b','rnd',nobs))
# Amat <- matrix(2*runif(6*6),6,6)
# Xmat <- tcrossprod(Smat,Amat)
# Xmat <- aE(Xmat,error = .8)
# # ICA via FastICA with 9 components
# imod <- icafast(Xmat,6)
# Sest <- imod$S
# r <- abs(multiway::congru(Smat,Sest))
# r
# y <- solve_LSAP(r, maximum = TRUE)
# y
#
# diag(cor(Smat,Sest[,y]))
#
# library(microbenchmark)
#
# microbenchmark(solve_LSAP(r, maximum = TRUE), CICA:::modRV(Smat,Sest), times = 20 , unit = 'relative')
