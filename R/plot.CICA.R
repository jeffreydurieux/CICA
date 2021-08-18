#' Plot method for CICA
#' @description Plot method for CICA. This function shows the cluster specific
#' independent components in an interactive viewer using the papayar package
#' @param x Object of \code{class} CICA
#' @param brain auto
#' @param cluster Components of cluster to plot. Only used when non fMRI related data is used
#' @param ... other arguments
#' @export
#'
#'
plot.CICA <- function(x, brain = 'auto', cluster = 1, ...){

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
    #warning('Data does not correspond to MNI coordinates, no interactive plot returned')
    df <- data.frame(x$Sr[[cluster]])
    y_axis_var_names <- paste('Comp' , 1:ncol(x$Sr[[cluster]]), sep = '_')
    colnames(df) <- y_axis_var_names
    create_buttons <- function(df, y_axis_var_names) {
      lapply(
        y_axis_var_names,
        FUN = function(var_name, df) {
          button <- list(
            method = 'restyle',
            args = list('x', list(df[, var_name])),
            label = var_name
          )
        },
        df
      )
    }

    fig1 <- plot_ly(df, x = ~Comp_1, alpha = 0.3, type = 'histogram') %>%
      layout(title = paste('Cluster', cluster),
             updatemenus = list(
               list(
                 buttons = create_buttons(df = df, y_axis_var_names = y_axis_var_names)
               )
             ))
    fig1

  }
}

