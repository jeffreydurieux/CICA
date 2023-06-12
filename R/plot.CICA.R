#' Plot method for CICA
#' @description Plot method for CICA. This function shows the cluster specific
#' independent components in an interactive viewer using the papayar package
#' @param x Object of \code{class} CICA
#' @param brain auto
#' @param cluster Components of cluster to plot. Only used when non fMRI related data is used
#' @param ... other arguments
#'
#' @examples
#' \dontrun{
#' CICA_data <- Sim_CICA(Nr = 15, Q = 5, R = 4, voxels = 100, timepoints = 10,
#' E = 0.4, overlap = .25, externalscore = TRUE)
#'
#' multiple_output = CICA(DataList = CICA_data$X, nComp = 2:6, nClus = 1:5,
#' userGrid = NULL, RanStarts = 30, RatStarts = NULL, pseudo = c(0.1, 0.2),
#' pseudoFac = 2, userDef = NULL, scalevalue = 1000, center = TRUE,
#' maxiter = 100, verbose = TRUE, ctol = .000001)
#'
#' plot(multiple_output$Q_5_R_4, cluster = 2)
#' }
#'
#' @importFrom plotly plot_ly layout add_histogram
#' @importFrom magrittr %>%
#' @import RNifti
#'
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

    create_buttons2 <- function(df, y_axis_var_names) {
      lapply(
        1:length(y_axis_var_names),
        FUN = function(id, y_axis_var_names, df) {
          position<-rep(FALSE,length(y_axis_var_names))
          position[id]<-TRUE
          button <- list(
            method = 'update',
            args = list(list(visible = position),
                        list(xaxis = list(title = colnames(y_axis_var_names)[id]))),
            label = colnames(y_axis_var_names)[id]
          )
        },
        df
      )
    }

    df <- data.frame(x$Sr[[cluster]])
    y_axis_var_names <- paste('Comp' , 1:ncol(x$Sr[[cluster]]), sep = '_')
    colnames(df) <- y_axis_var_names



    fig<-plot_ly(df, x = ~Comp_1, alpha = 0.3, type = 'histogram')
    if(length(y_axis_var_names)>1){
      for (i in 2:length(y_axis_var_names)) {
        dfi <- data.frame(y=df[y_axis_var_names[i]])
        colnames(dfi)<-"y"
        fig<-fig %>% add_histogram(x = ~y, data=dfi, visible = FALSE, name = y_axis_var_names[i]) #Something wrong here
      }
    }
    fig<-fig %>% layout(
      title = paste('Cluster', cluster),
      showlegend = FALSE
      ,
      updatemenus = list(
        list(
          buttons = create_buttons2(df = df, y_axis_var_names = y_axis_var_names)
        )
      )
    )

    fig

  }
}




# #warning('Data does not correspond to MNI coordinates, no interactive plot returned')
# df <- data.frame(x$Sr[[cluster]])
# y_axis_var_names <- paste('Comp' , 1:ncol(x$Sr[[cluster]]), sep = '_')
# colnames(df) <- y_axis_var_names
# create_buttons <- function(df, y_axis_var_names) {
#   lapply(
#     y_axis_var_names,
#     FUN = function(var_name, df) {
#       button <- list(
#         method = 'restyle',
#         args = list('x', list(df[, var_name])),
#         label = var_name
#       )
#     },
#     df
#   )
# }
#
# fig1 <- plot_ly(df, x = ~Comp_1, alpha = 0.3, type = 'histogram') %>%
#   layout(title = paste('Cluster', cluster),
#          xaxis = list(title = "Component"),
#          updatemenus = list(
#            list(
#              buttons = create_buttons(df = df, y_axis_var_names = y_axis_var_names)
#            )
#          ))
# fig1
