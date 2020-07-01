#' Copy a the Stock Synthesis input files from one directory to another
#'
#' Reads the starter.ss file to figure out the names of the control and
#' data files, than copies those files along with starter.ss, forecast.ss,
#' and wtatage.ss (if present) to a new directory, as specified.
#'
#' @param dir.old Location of model files to be copied.
#' @param dir.new New location to which the files should be copied.
#' @param create.dir Create dir.new directory if it doesn't exist already?
#' @param overwrite Overwrite existing files with matching names?
#' @param recursive logical. Should elements of the path other than the last be
#'        created?
#' @param use_ss_new Use .ss_new files instead of original inputs?
#' @param copy_exe Copy any executables found in dir.old to dir.new?
#' @param copy_par Copy any .par files found in dir.old to dir.new?
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
                           create.dir=TRUE,
                           overwrite=FALSE,
                           recursive=FALSE,
                           use_ss_new=FALSE,
                           copy_exe=FALSE,
                           copy_par=FALSE,
                           verbose=TRUE){

  # check to make sure the first input is in the correct format
  if(!is.character(dir.old) | length(dir.old)!=1){
    stop("Input 'dir.old' should be a character string for a directory")
  }

  # check for presence of old directory
  if(dir.old != "" & !dir.exists(dir.old)){
    stop("dir.old doesn't exist:", dir.old)
  }
  # check for presence of new directory, and create if requested
  if(!dir.exists(dir.new)){
    if(create.dir){
      dir.create(dir.new, recursive=recursive)
    }else{
      stop("'dir.create=FALSE' and dir.new doesn't exist:", dir.new)
    }
  }
  # read starter file to figure out what other inputs are
  starter <- SS_readstarter(file.path(dir.old, "starter.ss"), verbose=FALSE)

  if(verbose){
    message("copying files from\n ", dir.old, "\nto\n ", dir.new)
  }

  results <- rep(NA, 6)
  if(!use_ss_new){ # copy original input files
    results[1] <- file.copy(from=file.path(dir.old, starter$ctlfile),
                            to = file.path(dir.new, starter$ctlfile),
                            overwrite=overwrite)
    results[2] <- file.copy(from=file.path(dir.old, starter$datfile),
                            to = file.path(dir.new, starter$datfile),
                            overwrite=overwrite)
    results[3] <- file.copy(from=file.path(dir.old, "forecast.ss"),
                            to = file.path(dir.new, "forecast.ss"),
                            overwrite=overwrite)
    results[4] <- file.copy(from=file.path(dir.old, "starter.ss"),
                            to = file.path(dir.new, "starter.ss"),
                            overwrite=overwrite)
    if(file.exists(file.path(dir.old, "wtatage.ss"))){
      results[5] <- file.copy(from=file.path(dir.old, "wtatage.ss"),
                              to = file.path(dir.new, "wtatage.ss"),
                              overwrite=overwrite)
    }
  }else{ # copy ss_new files
    results[1] <- file.copy(from=file.path(dir.old, "control.ss_new"),
              to = file.path(dir.new, starter$ctlfile),
              overwrite=overwrite)
    results[2] <- file.copy(from=file.path(dir.old, "data.ss_new"),
              to = file.path(dir.new, starter$datfile),
              overwrite=overwrite)
    results[3] <- file.copy(from=file.path(dir.old, "forecast.ss_new"),
              to = file.path(dir.new, "forecast.ss"),
              overwrite=overwrite)
    results[4] <- file.copy(from=file.path(dir.old, "starter.ss_new"),
              to = file.path(dir.new, "starter.ss"),
              overwrite=overwrite)
    if(file.exists(file.path(dir.old, "wtatage.ss"))){
      results[5] <- file.copy(from=file.path(dir.old, "wtatage.ss_new"),
                to = file.path(dir.new, "wtatage.ss"),
                overwrite=overwrite)
    }
  }
  # copy executables(s) if requested
  if(copy_exe){
    # figure out which files are executables
    is.exe <- file.info(dir(dir.old, full.names=TRUE))$exe
    exefiles <- dir(dir.old)[is.exe!="no"]
    if(length(exefiles)==0){
      warning("No executable files found in ", dir.old)
    }
    if(length(exefiles) > 1){
      warning("Copying multiple executable files")
    }
    for(file in exefiles){
      results[6] <- file.copy(from=file.path(dir.old, file),
                              to = file.path(dir.new, file),
                              overwrite=overwrite)
    }
  }

  # copy par file(s) if requested
  if(copy_par){
    # figure out which files are executables
    parfiles <- dir(dir.old)[grep(".par$", dir(dir.old))]
    if(length(parfiles)==0){
      warning("No .par files found in ", dir.old)
    }
    if(length(parfiles) > 1){
      warning("Copying multiple .par files")
    }
    for(file in parfiles){
      results[7] <- file.copy(from=file.path(dir.old, file),
                              to = file.path(dir.new, file),
                              overwrite=overwrite)
    }
  }
  
  # check for successful copying
  if(all(results, na.rm=TRUE)){
    if(verbose){
      message("copying complete")
    }
    return(TRUE)
  }else{
    if(verbose){
      if(overwrite){
        warning("at least 1 file failed to copy")
      }else{
        warning("at least 1 file failed to copy, try 'overwrite = TRUE'")
      }
    }
    return(FALSE)
  }
}
