#### note: papayar needs to be installed but is no longer available on CRAN
#### this will lead to an error when installing CICA package
devtools::install_github('jeffreydurieux/CICA')
library(CICA)
?CICA

data('CICA_data', package = 'CICA')
attach(CICA_data)

# multi cica
multiple_output = CICA(DataList = CICA_data$X, nComp = 2:6, nClus = 1:5,
                       userGrid = NULL, RanStarts = 30, RatStarts = FALSE, pseudo = c(0.1, 0.2),  pseudoFac = 2, rational = NULL, scalevalue = 1000, center = TRUE, maxiter = 100, verbose = TRUE, ctol = .000001)


# defining rational starts

multiple_output2 = CICA(DataList = CICA_data$X, nComp = 2:6, nClus = 1:5,
                       userGrid = NULL, RanStarts = 30, RatStarts = TRUE, pseudo = c(0.1,0.2),  pseudoFac = 2, rational = NULL, scalevalue = 1000, center = TRUE, maxiter = 100, verbose = TRUE, ctol = .000001)

# warnings: : In if (pseudo >= 1 | pseudo <= 0) { ... :
# the condition has length > 1 and only the first element will be used
#this is an error in windows

#### model selection
ModSelOutput <- SequentialScree(multiple_output)

#### plot model selection
plot(ModSelOutput)#does not work in windows? Somehow normal plot is called. Mac it works

plot.ModSel(ModSelOutput)



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
# default plot method is called not s3 method?
plot.CICA(multiple_output$Q_5_R_4)

matcher(multiple_output$Q_5_R_4, reference = 1, RV = TRUE)
# on windows: Error in UseMethod("matcher") :
#no applicable method for 'matcher' applied to an object of class "CICA"
matcher.CICA(multiple_output$Q_5_R_4, reference = 1, RV = TRUE)

### using nifti

#nifs <- loadNIfTIs('~/Repo_temp/P3/nifti/', toMatrix = T)
nifs <- loadNIfTIs('C:/Users/jeffr/OneDrive - Erasmus University Rotterdam/Data/nifti/', toMatrix = T)

outnif <- CICA(DataList = nifs, RanStarts = 2, nComp = 10, nClus = 2)


setwd('C:/Users/jeffr/Downloads/')
test <- Sr_to_nifti(outnif,write = T, datatype = 'int16', version = 2)

outnif$P
plot(outnif) # windows: default plot method
plot.CICA(outnif)

matcher(outnif, reference = '~/Documents/templates/23x_all_beckmann_8_and_csf_and_wm.nii.gz')# windows: default plot method
matcher.CICA(outnif, RV = TRUE)
matcher.CICA(outnif, reference = 'C:/Users/jeffr/OneDrive - Erasmus University Rotterdam/Data/23x_all_beckmann_8_and_csf_and_wm.nii.gz')


##### two step functions #####
Out_starts <- FindRationalStarts(DataList = CICA_data$X,nComp = 5,nClus = 4,scalevalue = 1000)

plot.rstarts(Out_starts)
plot.rstarts(Out_starts, type = 2)
plot.rstarts(Out_starts, type = 2,mdsdim = 3, method = 'ward.D2')
# note: if no method is specified, title of label method is empty
# add default labeling method to the default plot

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

