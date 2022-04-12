# Wed Mar 23 10:39:01 2022
# Author: Jeffrey Durieux, MSc

# What: function to generate a matrix of starts
#nClus = 4; nBlocks = 20;RanStarts = 100; RatStarts = 2;ARIlimRan = 0.2
#DataList=CICA_data$X; scale=T; center=T; pseudo=.2;nComp=5;verbose = T;pseudoFac=5
# GenRanStarts <- function(RanStarts, nClus, nBlocks, ARIlimRan = 0.2,
#                          itmax = 1000, verbose = FALSE){
#   # random starts
#    rs <- matrix(CICA:::clusf(nBlocks = nBlocks, nClus = nClus) )
#
#    it <- 0
#    while(ncol(rs) != RanStarts & it < itmax){
#        it <- it + 1
#        candidate <- CICA:::clusf(nBlocks = nBlocks, nClus = nClus)
#        if(all(abs(apply(rs, MARGIN = 2, adjustedRandIndex, y = candidate)) < ARIlim)){
#          rs <- cbind(rs, candidate)
#        }
#        if(verbose){
#          cat(ncol(rs), fill = TRUE)
#        }
#
#     }
#
#    if(it == itmax){
#      warning(paste('Not enough random starts found with ARIlimRan of:',ARIlimRan))
#    }
#
#    # heatmap/image plot option
#    comb <- t(utils::combn(1:ncol(rs), 2))
#
#    rsARI <- matrix(data = NA, nrow = ncol(rs) , ncol = ncol(rs))
#    rsi <- numeric()
#
#    for(i in 1:nrow(comb)){
#      idx <- comb[i,]
#      rsi<- adjustedRandIndex( rs[,idx[1]], rs[,idx[2]] )
#      res <- c(comb[i , ] , rsi)
#      rsARI[res[1]  , res[2] ] <- res[3]
#    }
#
#     rsARI[lower.tri(rsARI)] = t(rsARI)[lower.tri(rsARI)]
#
#    out <- list()
#    out$rs <- rs
#    out$ARIs <- rsARI
#    return(out)
# }

# random <- GenStarts(20, 2, 20)
# m <- random$ARIs
# diag(m) <- 1
# fields::image.plot(m)

# GenRatStarts <- function(DataList, nComp, nClus, scale, center, verbose, pseudo,pseudoFac){
#
#
#   rat <- FindRationalStarts(DataList = DataList, nComp = nComp, nClus = nClus,
#                             scale = scale, center = center, verbose = verbose,
#                             pseudo = pseudo, pseudoFac = 5)
#
#   comb <- t(utils::combn(1:ncol(rat$rationalstarts), 2))
#
#   rsARI <- matrix(data = NA, nrow = ncol(rat$rationalstarts) , ncol = ncol(rat$rationalstarts))
#   rsi <- numeric()
#
#   for(i in 1:nrow(comb)){
#     idx <- comb[i,]
#     rsi<- adjustedRandIndex( rat$rationalstarts[,idx[1]], rat$rationalstarts[,idx[2]] )
#     res <- c(comb[i , ] , rsi)
#     rsARI[res[1]  , res[2] ] <- res[3]
#   }
#
#   rsARI[lower.tri(rsARI)] = t(rsARI)[lower.tri(rsARI)]
#
#   out <- list()
#   out$rat <- rat
#   out$ARIs <- rsARI
#   return(out)
# }
# res <- GenRatStarts(DataList = CICA_data$X, nComp = 5, nClus = 4, scale = T,
#                     center = T, verbose = T, pseudo = .05, pseudoFac = 5)
# m <- res$ARIs[-c(1:7),-c(1:7)]
# diag(m) <- 1
# m <- res$ARIs
# diag(m) <- 1
# fields::image.plot(m)
