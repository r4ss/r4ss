#' SSutils
#'
#' The SSutils package provides additional utilities for working with
#' Stock Synthesis that are not found in the r4ss package.
#' 
#' @section SSutils functions:
#' \itemize{
#'   \item \code{\link{copy_SS_inputs}}: a function to copy inputs from
#' a single model into a new folder
#'   \item \code{\link{populate_multiple_folders}}: run copy_SS_inputs
#' over for a set of folders
#'   \item \code{\link{run_SS_models}}: run SS on each directory 
#'   \item \code{\link{run_SS_models_parallel}}: run SS on each directory in 
#' parallel using future and furrr (experimental)
#' }
#' @author Ian G. Taylor
#' @import r4ss
#' @docType package
#' @name SSutils
#'
#' @examples
#' \dontrun{
#'   devtools::install_github('r4ss/SSutils')
#'   
#'   # copy model files to a bunch of new directories and
#'   # copy a new executable into each one
#'   outerdir.old <- 'c:/SS/modeltesting/Version_3.30.12.00_June22'
#'   outerdir.new <- 'c:/SS/modeltesting/Version_3.30.13_beta_Dec18'
#'   dir.info <- populate_multiple_folders(
#'       outerdir.old = outerdir.old,
#'       outerdir.new = outerdir.new,
#'       exe.dir = 'c:/SS/SSv3.30.13_beta_Dec18', overwrite=FALSE)
#' 
#'   # run the models in each directory
#'   run_SS_models(dirvec = dir(outerdir.new, full.names=TRUE))
#'   
#'   # read output
#'   mods.dir <- dir(outerdir.new, full.names=TRUE)
#'   mods.out <- SSgetoutput(dirvec=mods.dir)
#' 
#'   # run plotting functions
#'   for(imod in 1:length(mods.out)){
#'     print(imod)
#'     print(mods.dir[imod])
#'     graphics.off()
#'     SS_plots(mods.out[[imod]])
#'   }
#' 
#'   # check for successful completion of plots
#'   for(imod in 1:length(mods.dir)){
#'     print(imod)
#'     print(mods.dir[imod])
#'     print(file.info(file.path(mods.dir[imod], "plots/SS_output.html"))$size)
#'   }
#' }
#' 
NULL
