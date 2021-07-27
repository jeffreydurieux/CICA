#' Load Nifti files from directory
#'
#' @param dir Input directory containing nifti files
#' @param toMatrix logical if TRUE nifti's are converted to matrices
#'
#' @return list object containing Voxel by Time course matrices
#' @export
#'

loadNiftis <- function(dir, toMatrix = TRUE){

  fs <- dir(dir, pattern = '.nii.gz')
  files <- paste(dir, fs, sep = '/')

  NifList <- vector(mode = 'list', length = length(files))

  for(nif in 1:length(files) ){
    n <- readNifti(files[nif])

    if(length(dim(n)) != 4){
      stop('Nifti file number ', nif, ' does not have 4 dimensions')
    }

    # Voxel by Time matrix
    if(toMatrix == TRUE){
      NifList[[nif]] <- matrix(n, ncol = dim(n)[4])
    }else{
      NifList[[nif]] <- n
    }


  }

  names(NifList) <- fs
  return(NifList)
}


