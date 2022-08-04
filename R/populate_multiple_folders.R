#' Populate multiple Stock Synthesis folders with input files
#'
#' Creates a set of multiple folders and populates each with SS input files
#' such as for the purpose of running a new version of SS for an existing
#' set of test models.
#'
#' @param outerdir.old Location of existing outer directory containing
#' subdirectories for each model.
#' @param outerdir.new New outer directory into which the subfolders
#' should be created.
#' @param create.dir Create new outer directory if it doesn't exist already?
#' @template overwrite
#' @param use_ss_new Use .ss_new files instead of original inputs?
#' @param copy_exe Copy any executables found in the individual
#' directories or in exe.dir (if provided)? To copy a new executable
#' from a central location, use `exe.dir` instead.
#' @param copy_par Copy any .par files found in the individual directories?
#' @param exe.dir Optional path to executable to copy into all the subfolders.
#' @param exe.file Filename of executable to copy into all the subfolders.
#' @template verbose
#' @return Returns table of results indicating which directories were
#' successfully populated with the model input files and/or executables
#' @author Ian G. Taylor
#' @export
#' @family run functions
#' @examples
#' \dontrun{
#' populate_multiple_folders(
#'   outerdir.old = "c:/SS/old_models",
#'   outerdir.new = "c:/SS/new_models",
#'   copy_exe = TRUE
#' )
#'
#' # copy and rename the ss_new files and
#' # get an executable file from a central location
#' populate_multiple_folders(
#'   outerdir.old = "old_models",
#'   outerdir.new = "new_models",
#'   use_ss_new = TRUE,
#'   exe.dir = "c:/SS/SSv3.30.19.01",
#' )
#' }
#'
populate_multiple_folders <- function(outerdir.old,
                                      outerdir.new,
                                      create.dir = TRUE,
                                      overwrite = FALSE,
                                      use_ss_new = FALSE,
                                      copy_exe = FALSE,
                                      copy_par = FALSE,
                                      exe.dir = NULL,
                                      exe.file = "ss",
                                      verbose = TRUE) {
  # check to make sure the first input is in the correct format
  if (!is.character(outerdir.old) | length(outerdir.old) != 1) {
    stop("Input 'outerdir.old' should be a character string")
  }

  # check for presence of old directory
  if (outerdir.old != "" & !dir.exists(outerdir.old)) {
    stop("outerdir.old doesn't exist: ", outerdir.old)
  }

  # check for presence of new directory, and create if requested
  if (!dir.exists(outerdir.new)) {
    if (create.dir) {
      dir.create(outerdir.new)
    } else {
      stop("'dir.create=FALSE' and outerdir.new doesn't exist:", outerdir.new)
    }
  }

  # note source and destination directories
  if (verbose) {
    message("copying files from\n ", outerdir.old, "\nto\n ", outerdir.new)
  }

  # empty data frame to attach things to
  dir.info <- NULL

  # figure out the inner directories
  innerdirs <- dir(outerdir.old)
  # loop over possibilities
  for (idir in seq_along(innerdirs)) {
    # directory in question
    dir <- innerdirs[idir]
    # check to make sure it's a directory
    if (dir.exists(file.path(outerdir.old, dir))) {
      message("copying ", dir)
      # check for presence of starter file
      if ("starter.ss" %in% tolower(dir(file.path(outerdir.old, dir)))) {
        # copy all SS input files
        results.files <- copy_SS_inputs(
          dir.old = file.path(outerdir.old, dir),
          dir.new = file.path(outerdir.new, dir),
          create.dir = TRUE,
          use_ss_new = use_ss_new,
          # only copy executable from individual folder if exe.dir = NULL
          copy_exe = ifelse(is.null(exe.dir), copy_exe, FALSE),
          copy_par = copy_par,
          overwrite = overwrite,
          verbose = FALSE
        )
        if (!results.files) {
          warning("at least 1 input file failed to copy")
        }
        # copy executable
        if (!is.null(exe.dir)) {
          # clean up exe name (e.g. add ".exe" on Windows)
          exe.file <- check_exe(exe = exe.file, dir = exe.dir)[["exe"]]
          results.exe <- file.copy(
            from = file.path(exe.dir, exe.file),
            to = file.path(outerdir.new, dir, exe.file),
            overwrite = overwrite
          )
          if (!results.exe) {
            warning("executable failed to copy")
          }
        } else {
          results.exe <- NA
        }
        dir.info <- rbind(dir.info, data.frame(
          dir = dir,
          results.files = results.files,
          results.exe = results.exe
        ))
      } else {
        # warn if starter file is missing
        warning("skipping ", dir, " which doesn't contain a starter.ss file")
      }
    }
  }
  return(dir.info)
}
