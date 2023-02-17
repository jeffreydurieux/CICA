library(CICA)
?CICA

data('CICA_data', package = 'CICA')
attach(CICA_data)

# multi cica
multiple_output = CICA(DataList = CICA_data$X, nComp = 2:6, nClus = 1:5,
                       userGrid = NULL, RanStarts = 30, RatStarts = FALSE, pseudo = c(0.1, 0.2),  pseudoFac = 2, rational = NULL, scalevalue = 1000, center = TRUE, maxiter = 100, verbose = TRUE, ctol = .000001)


# defining rational starts

multiple_output2 = CICA(DataList = CICA_data$X, nComp = 2:6, nClus = 1:5,
                       userGrid = NULL, RanStarts = 30, RatStarts = TRUE, pseudo = c(0.1, 0.2),  pseudoFac = 2, rational = NULL, scalevalue = 1000, center = TRUE, maxiter = 100, verbose = TRUE, ctol = .000001)

# warnings: : In if (pseudo >= 1 | pseudo <= 0) { ... :
# the condition has length > 1 and only the first element will be used

#### model selection
ModSelOutput <- SequentialScree(multiple_output)

#### plot model selection
plot(ModSelOutput)
summary(ModSelOutput) # this does not exist yet
# so make a summary method?

#### interpreting cica output
summary(multiple_output$Q_5_R_4)
# maybe make ifelse statement in summary function
# such that modsel will be done automatically
# in that way only summary(multiple_output) is only necessary

mclust::adjustedRandIndex(CICA_data$P , multiple_output$Q_5_R_4$P )

table(CICA_data$P , multiple_output$Q_5_R_4$P )

apply(multiple_output$Q_5_R_4$starts, MARGIN = 2, FUN = mclust::adjustedRandIndex, y = CICA_data$P)

round(apply(multiple_output2$Q_5_R_4$starts, MARGIN = 2, FUN = mclust::adjustedRandIndex, y = CICA_data$P), digits = 2) # make a function to extract only rationals and pseudorationals

plot(multiple_output$Q_5_R_4, cluster = 2)

matcher(multiple_output$Q_5_R_4, reference = 1, RV = TRUE)

####### old ########
##### CICA part with s3 methods #####

data('CICA_data', package = 'CICA')
attach(CICA_data)

set.seed(42)
output <- CICA(DataList = CICA_data$X, RanStarts = 10, nComp = 5, nClus = 4
               ,scalevalue = 1000, center = T)

output$LossStarts
output$Loss

summary(output)

plot(output)

matcher(output, reference = 1, RV = TRUE)

### using nifti

nifs <- loadNIfTIs('~/Repo_temp/P3/nifti/', toMatrix = T)

outnif <- CICA(DataList = nifs, nStarts = 2, nComp = 10, nClus = 2, parallel = F)


setwd('~/Downloads/')
test <- Sr_to_nifti(outnif,write = T, datatype = 'int16', version = 2)

outnif$P
plot(outnif)
matcher(outnif, reference = '~/Documents/templates/23x_all_beckmann_8_and_csf_and_wm.nii.gz')

data <- list()
setwd('~/Repo_temp/P3/datatv/')
files <- dir()
for(i in 1:length(files)){
  load(files[i])
  data[[i]] <- t(data_tv)
}

outdatatv <- CICA(DataList = data, nStarts = 10, nComp = 4, nClus = 2)
outdatatv$Loss
plot(outdatatv)

lab <- c(rep(1,5), rep(2,5))
outdatatv2 <- CICA(DataList = data, nStarts = 1, nComp = 4, nClus = 2, rational = lab)
outdatatv2$Loss

### find rational starts ####

rat <- FindRationalStarts(DataList = CICA_data$X, nComp = 5, nClus = 4
                          ,verbose = T, pseudo = .2, parallel = F)

microbenchmark::microbenchmark(
  rat <- FindRationalStarts(DataList = CICA_data$X, nComp = 5, nClus = 4
                            ,verbose = F, pseudo = .2, parallel = F), times = 1
)

microbenchmark::microbenchmark(
  rat <- FindRationalStarts(DataList = CICA_data$X, nComp = 5, nClus = 4
                            ,verbose = T, pseudo = .2, parallel = F), times = 1
)

microbenchmark::microbenchmark(
  rat <- FindRationalStarts(DataList = CICA_data$X, nComp = 5, nClus = 4
                            ,verbose = F, pseudo = .2, parallel = T), times = 1
)

microbenchmark::microbenchmark(
  rat <- FindRationalStarts(DataList = CICA_data$X, nComp = 5, nClus = 4
                            ,verbose = T, pseudo = .2, parallel = T), times = 1
)



class(rat)
apply(rat$rationalstarts, 2, mclust::adjustedRandIndex ,y = CICA_data$P)

plot(rat, mdsdim = 2, type = 2, method = 'ward.D2')
library(Rtsne)
rtsne <- Rtsne(rat$RVdist, perplexity = 2, dims = 2)
plot_ly(data = data.frame(rtsne$Y), x = ~X1,y=~X2,z=~X3, color = rat$rationalstarts$p_ward)

plot_ly(data = data.frame(rtsne$Y), x = ~X1,y=~X2, color = rat$rationalstarts$p_ward)


cicaout <- CICA::CICA(DataList = CICA_data$X, nStarts = 30, nComp = 5, nClus = 4, rational = rat , verbose = T)
summary(cicaout)

devtools::source_url('https://raw.githubusercontent.com/jeffreydurieux/usefulcode/master/FindOptimalClusPermut.R')
devtools::source_url('https://raw.githubusercontent.com/jeffreydurieux/usefulcode/master/FindOptimalPermutSources.R')

library(gtools)
opt <- FindOptimalClusPermut(Pest = cicaout$P, Ptrue = CICA_data$P)
FindOptimalPermutSingle(cicaout$Sr[[1]], CICA_data$Sr[[opt$BestPerm[[1]]]])
FindOptimalPermutSingle(cicaout$Sr[[2]], CICA_data$Sr[[opt$BestPerm[[2]]]])
FindOptimalPermutSingle(cicaout$Sr[[3]], CICA_data$Sr[[opt$BestPerm[[3]]]])
FindOptimalPermutSingle(cicaout$Sr[[4]], CICA_data$Sr[[opt$BestPerm[[4]]]])
mclust::adjustedRandIndex(cicaout$P, CICA_data$P)



###### code for section 3.4 in article #####

options(prompt = "R> ", continue = "+  ", width = 70,
        useFancyQuotes = FALSE)
# Thu Feb 16 11:31:22 2023
# Author: Jeffrey Durieux, MSc

#JD: script examples for paper


library(CICA)

data('CICA_data', package = 'CICA')

rstarts <- FindRationalStarts(DataList = CICA_data$X, nComp = 5, nClus = 4, scale = TRUE, center = TRUE, verbose = FALSE, pseudo = 0.2)

summary(rstarts)

plot(rstarts, type = 1, method = 'complete')
plot(rstarts, type = 2, method = 'ward.D2', mdsdim = 3)
apply(rstarts$rationalstarts, 2, mclust::adjustedRandIndex, y = CICA_data$P)

output <- CICA(DataList = CICA_data$X, nStarts = 50, nComp = 5, nClus = 4, rational = rstarts)

plot(output, cluster = 2)

#### compute ARI ###
apply(rstarts$rationalstarts[,1:7], MARGIN = 2, FUN = adjustedRandIndex, y = CICA_data$P) %>% round(digits = 3)

