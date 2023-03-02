#' Sequential Model Selection for Multiple CICA model
#'
#' @param x an object of class MultipleCICA
#'
#' @return a list object
#' @export
#'
SequentialScree <- function(x){

  if(class(x) != 'MultipleCICA'){
    stop('Input object should be of class MultipleCICA')
  }

  models <- names(x)
  split <- strsplit(models, split = '_')
  Q <- as.numeric(sapply(seq_along(split), function(anom) split[[anom]][2]))
  R <- as.numeric(sapply(seq_along(split), function(anom) split[[anom]][4]))

  Loss <- sapply(seq_along(x), function(anom) x[[anom]]$Loss)

  df <- data.frame(Q = Q, R = R, Loss = Loss)

  SR_rq <- function(Lq){

    Screes <- numeric()

    for(i in 1:length(Lq)){
      if(i == 1){
        Screes[i] <- NA
      }
      else if(i == length(Lq) ){
        Screes[i] <- NA
      }else{
        Screes[i] <- (Lq[i-1] - Lq[i]) / (Lq[i] - Lq[i+1])
      }
    }
    return(Screes)
  }

  SR_qR <- function(LqR){
    Screes <- numeric()

    for(i in 1:length(LqR)){
      if(i == 1){
        Screes[i] <- NA
      }
      else if(i == length(LqR) ){
        Screes[i] <- NA
      }else{
        Screes[i] <- (LqR[i-1] - LqR[i]) / (LqR[i] - LqR[i+1])
      }
    }
    return(Screes)
  }


  if( (length(unique(df$Q))>=3) & (length(unique(df$R))>=3) ){
    # if both R and Q
    Qu <- unique(df$Q)
    Screes <- SR_rq( df[df$Q==Qu[1], ]$Loss )
    for(i in 2:length(Qu)){
      Screes <- rbind(Screes, SR_rq(df[df$Q==Qu[i], ]$Loss ))
    }

    ColMeanScrees <- colMeans(Screes, na.rm = TRUE)
    Rid <- which.max(ColMeanScrees)
    Ru <- unique(df$R)
    Rselect <- Ru[Rid]

    ScreeConditionalonR <- SR_qR(df[df$R==Rselect,]$Loss)
    Qid <- which.max(ScreeConditionalonR)
    Qselect <- Qu[Qid]

    out <- list()
    out$optimalQ <- Qselect
    out$optimalR <- Rselect
    out$df <- df
    out$Screes_step1 <- Screes
    out$ColMeanScrees <- ColMeanScrees
    out$ScreeConditionalonR <- ScreeConditionalonR
    class(out) <- 'ModSel'
  }else if( (length(unique(df$R))>=3) & (length(unique(df$Q))==1) ){

    dff <- data.frame(R=df$R, Loss=df$Loss)
    Screes <- numeric()

    for(i in 1:length(dff$Loss)){
      if(i == 1){
        Screes[i] <- NA
      }
      else if(i == length(dff$Loss) ){
        Screes[i] <- NA
      }else{
        Screes[i] <- (dff$Loss[i-1] - dff$Loss[i]) / (dff$Loss[i] - dff$Loss[i+1])
      }
    }

    out <- list()
    out$optimalR <- df$R[which.max(Screes)]
    out$df <- dff
    class(out) <- 'ModSel'

  }else if( (length(unique(df$Q))>=3) & (length(unique(df$R))==1) ){
    # if only one R
    dff <- data.frame(Q=df$Q, Loss=df$Loss)
    Screes <- numeric()

    for(i in 1:length(dff$Loss)){
      if(i == 1){
        Screes[i] <- NA
      }
      else if(i == length(dff$Loss) ){
        Screes[i] <- NA
      }else{
        Screes[i] <- (dff$Loss[i-1] - dff$Loss[i]) / (dff$Loss[i] - dff$Loss[i+1])
      }
    }
    out <- list()
    out$optimalQ <- df$Q[which.max(Screes)]
    out$df <- dff
    class(out) <- 'ModSel'
  }
  return(out)
}
