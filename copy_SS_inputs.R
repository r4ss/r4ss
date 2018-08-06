#' Copy a the Stock Synthesis input files from one directory to another
#'
#' Reads the starter.ss file to figure out the names of the control and
#' data files, than copies those files along with starter.ss, forecast.ss,
#' and wtatage.ss (if present) to a new directory, as specified.
#'
#' @param dir.old Location of model files to be copied.
#' @param dir.new New location to which the files should be copied.
#' @param create.dir.new Create dir.new directory if it doesn't exist already?
#' @param overwrite Overwrite existing files with matching names?
#' @param use_ss_new Use .ss_new files instead of original inputs?
#' @param verbose Return updates of function progress to the R console?
#' @return Doesn't return anything
#' @author Ian Taylor
#' @export
#' @examples
#'
#'   \dontrun{
#'     copy_SS_inputs(dir.old='c:/SS/old_model',
#'                    dir.new='c:/SS/new_model')
#'   }
#'

copy_SS_inputs <- function(dir.old=NULL,
                           dir.new=NULL,
                           create.dir.new=TRUE,
                           overwrite=FALSE,
                           use_ss_new=FALSE,
                           verbose=TRUE){
  # check for presence of old directory
  if(!dir.exists(dir.old)){
    stop("dir.old doesn't exist:", dir.new)
  }
  # check for presence of new directory, and create if requested
  if(!dir.exists(dir.new)){
    if(create.dir.new){
      dir.create(dir.new)
    }else{
      stop("dir.new doesn't exist:", dir.new)
    }
  }
  # read starter file to figure out what other inputs are
  starter <- SS_readstarter(file.path(dir.old, "starter.ss"), verbose=FALSE)

  if(verbose){
    message("copying files from\n ", dir.old, "\nto\n ", dir.new)
  }
  if(!use_ss_new){ # copy original input files
    file.copy(from=file.path(dir.old, starter$ctlfile),
              to = file.path(dir.new, starter$ctlfile), overwrite=overwrite)
    file.copy(from=file.path(dir.old, starter$datfile),
              to = file.path(dir.new, starter$datfile), overwrite=overwrite)
    file.copy(from=file.path(dir.old, "forecast.ss"),
              to = file.path(dir.new, "forecast.ss"), overwrite=overwrite)
    file.copy(from=file.path(dir.old, "starter.ss"),
              to = file.path(dir.new, "starter.ss"), overwrite=overwrite)
    if(file.exists(file.path(dir.old, "wtatage.ss"))){
      file.copy(from=file.path(dir.old, "wtatage.ss"),
                to = file.path(dir.new, "wtatage.ss"), overwrite=overwrite)
    }
  }else{ # copy ss_new files
    file.copy(from=file.path(dir.old, "control.ss_new"),
              to = file.path(dir.new, starter$ctlfile), overwrite=overwrite)
    file.copy(from=file.path(dir.old, "data.ss_new"),
              to = file.path(dir.new, starter$datfile), overwrite=overwrite)
    file.copy(from=file.path(dir.old, "forecast.ss_new"),
              to = file.path(dir.new, "forecast.ss"), overwrite=overwrite)
    file.copy(from=file.path(dir.old, "starter.ss_new"),
              to = file.path(dir.new, "starter.ss"), overwrite=overwrite)
    if(file.exists(file.path(dir.old, "wtatage.ss"))){
      file.copy(from=file.path(dir.old, "wtatage.ss_new"),
                to = file.path(dir.new, "wtatage.ss"), overwrite=overwrite)
    }
  }
  if(verbose){
    message("copying complete")
  }
}
