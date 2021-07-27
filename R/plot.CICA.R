#' Plot method for CICA
#' @description Plot method for CICA. This function shows the cluster specific
#' independent components in an interactive viewer using the papayar package
#' @param x Object of \code{class} CICA
#' @param brain auto
#' @param ... other arguments
#' @export
#'
#'
plot.CICA <- function(x, brain = 'auto', ...){

  dims <- dim(x$Sr[[1]])

  if(dims[1] == 14812){
    img <- readNifti(file = system.file(package = 'CICA', 'extdata',
                                        '8mm.nii.gz', mustWork = TRUE) )
    papaya( c(list(img), Sr_to_nifti(x) ) )
  }else if(dims[1] == 116380){
    img <- readNifti(file = system.file(package = 'CICA', 'extdata',
                                        '4mm.nii.gz', mustWork = TRUE) )
    papaya( c(list(img), Sr_to_nifti(x) ) )
  }else if(dims[1] == 902629){
    img <- readNifti(file = system.file(package = 'CICA', 'extdata',
                                        '2mm.nii.gz', mustWork = TRUE) )
    papaya( c(list(img), Sr_to_nifti(x) ) )
  }else{
    stop('Data does not correspond to MNI coordinates, no plot returned')
  }
}

