#' Populate multiple Stock Synthesis folders with input files
#'
#' Creates a set of multiple folders and populate each with SS input files
#' such as for the purpose of running a new version of SS for an existing
#' set of test models.
#'
#' @param outerdir.old Location of existing outer directory containing
#' subdirectories for each model.
#' @param outerdir.new New outer directory into which the subfolders
#' should be created.
#' @param create.dir Create new outer directory if it doesn't exist already?
#' @param overwrite Overwrite existing files with matching names?
#' @param use_ss_new Use .ss_new files instead of original inputs?
#' @param exe.dir Path to executable to copy into all the subfolders.
#' @param exe.file Filename of executable to copy into all the subfolders.
#' A value of NULL will skip copying the executable.
#' @param exe.only Only copy exe files, don't copy input files
#' @param verbose Return updates of function progress to the R console?
#' @return Returns table of results indicating which directories were
#' successfully populated with the model input files and/or executables
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{copy_SS_inputs}}
#' @examples
#'
#'   \dontrun{
#'     populate_multiple_folders(outerdir.old = 'c:/SS/old_models',
#'                               outerdir.new = 'c:/SS/new_models',
#'                               exe.dir = 'c:/SS/SSv3.30.12.00')
#'   }
#'

populate_multiple_folders <- function(outerdir.old = NULL,
                                      outerdir.new = NULL,
                                      create.dir = TRUE,
                                      overwrite = FALSE,
                                      use_ss_new = FALSE,
                                      exe.dir = NULL,
                                      exe.file = "ss.exe",
                                      exe.only = FALSE,
                                      verbose = TRUE){
  # check to make sure the first input is in the correct format
  if(!is.character(outerdir.old) | length(outerdir.old) != 1){
    stop("Input 'outerdir.old' should be a character string for a directory")
  }

  # check for presence of old directory
  if(outerdir.old != "" & !dir.exists(outerdir.old)){
    stop("outerdir.old doesn't exist: ", outerdir.old)
  }
  # check for presence of new directory, and create if requested
  if(!dir.exists(outerdir.new)){
    if(create.dir){
      dir.create(outerdir.new)
    }else{
      stop("'dir.create=FALSE' and outerdir.new doesn't exist:", outerdir.new)
    }
  }

  # check for presence of executable (if requested)
  if(!is.null(exe.dir) && !file.exists(file.path(exe.dir, exe.file))){
    warning("Executable not found: ", file.path(exe.dir, exe.file))
  }

  # note source and destination directories
  if(verbose){
    message("copying files from\n ", outerdir.old, "\nto\n ", outerdir.new)
  }

  # empty data frame to attach things to
  dir.info <- NULL
  
  # figure out the inner directories
  innerdirs <- dir(outerdir.old)
  # loop over possibilities
  for(idir in 1:length(innerdirs)){
    # directory in question
    dir <- innerdirs[idir]
    # check to make sure it's a directory
    if(dir.exists(file.path(outerdir.old, dir))){
      message("copying ", dir)
      # check for presence of starter file
      if("starter.ss" %in% tolower(dir(file.path(outerdir.old, dir))) ){
        # copy all SS input files
        results.files <- copy_SS_inputs(dir.old    = file.path(outerdir.old, dir),
                                        dir.new    = file.path(outerdir.new, dir),
                                        create.dir = TRUE,
                                        use_ss_new = use_ss_new,
                                        overwrite  = overwrite,
                                        verbose    = FALSE)
        if(!results.files){
          warning("at least 1 input file failed to copy")
        }
        # copy executable
        if(!is.null(exe.dir)){
          results.exe <- file.copy(from = file.path(exe.dir, exe.file),
                                   to =   file.path(outerdir.new, dir, exe.file),
                                   overwrite = overwrite)
          if(!results.exe){
            warning("executable failed to copy")
          }
        }else{
          results.exe <- NA
        }
        dir.info <- rbind(dir.info, data.frame(dir           = dir,
                                               results.files = results.files,
                                               results.exe   = results.exe))
      }else{
        # warn if starter file is missing
        warning("skipping ", dir, " which doesn't contain a starter.ss file") 
      }
    }
  }
  return(dir.info)
}
