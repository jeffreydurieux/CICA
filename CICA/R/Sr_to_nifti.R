#' Convert Cluster specific independent components to NIFTI format
#'
#' @param x an object of \code{class} CICA
#' @param write if TRUE, NIfTI files are written to current working directory
#' @param ... other arguments passed to RNifti::writeNifti
#' @return a list with niftiImage files
#' @export
#'
Sr_to_nifti <- function(x, write = FALSE, ...){

  dims <- dim(x$Sr[[1]])

  if(dims[1] == 14812){
    img <- readNifti(file = system.file(package = 'CICA', 'extdata', '8mm.nii.gz', mustWork = TRUE) )

  }else if(dims[1] == 116380){
    img <- readNifti(file = system.file(package = 'CICA', 'extdata', '4mm.nii.gz', mustWork = TRUE) )

  }else if(dims[1] == 902629){
    img <- readNifti(file = system.file(package = 'CICA', 'extdata', '2mm.nii.gz', mustWork = TRUE) )
  }else{
    warning('Voxel dimension not equal to a the size of a MNI brain template')
    d <- ceiling(dims[1] ^ (1/3))
    img <- array(data = 0, dim = c(d,d,d) )
  }

  dimnif <- dim(img)
  dimnif[4] <- dim(x$Sr[[1]])[2]

  NIFS <- vector(mode = 'list', length = length(x$Sr))
  for(i in 1:length(x$Sr)){
    arr <- array(x$Sr[[i]], dim = dimnif)
    img <- asNifti(arr, reference = img)
    NIFS[[i]] <- img
  }

  names(NIFS) <- paste('Sr_', 1:length(x$Sr), sep='')

  if(write == TRUE){
    filenames <- names(NIFS)
    lapply(X = seq_along(NIFS), function(i)
      writeNifti(NIFS[[i]], file = filenames[i] ,datatype = ...) )
  }

  return(NIFS)
}



