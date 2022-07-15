#' Run retrospective analyses
#'
#' Do retrospective analyses by creating new directories, copying model files,
#' and iteratively changing the starter file to set the number of years of data
#' to exclude. Note that there was a  bug for retrospectives in 3.30.01;
#' the user should update their model to a newer version of Stock Synthesis to
#' run retrospectives
#'
#' @param dir Directory where everything takes place.
#' @param masterdir Deprecated. Use `dir` instead.
#' @param oldsubdir Subdirectory within `masterdir` with existing model
#' files.
#' @param newsubdir Subdirectory within `masterdir` where retrospectives
#' will be run. Default is 'retrospectives'.
#' @param subdirstart First part of the pattern of names for the directories in
#' which the models will actually be run.
#' @param years Vector of values to iteratively enter into the starter file for
#' retrospective year. Should be zero or negative values.
#' @param overwrite Overwrite any input files with matching names in the
#' subdirectories where models will be run.
#' @param RemoveBlocks Logical switch determining whether specifications of
#' blocks is removed from top of control file. Blocks can cause problems for
#' retrospective analyses, but the method for removing them is overly
#' simplistic and probably won't work in most cases. Default=FALSE.
#' @template exe
#' @template verbose
#' @param ... Additional arguments passed to r4ss::run(), such as
#' `extras`, and `show_in_console`.
#'
#' @author Ian G. Taylor, James T. Thorson
#' @export
#' @seealso [SSgetoutput()]
#' @examples
#' \dontrun{
#' # note: don't run this in your main directory--make a copy in case something
#' # goes wrong
#' mydir <- "C:/Simple"
#'
#' ## retrospective analyses
#' SS_doRetro(
#'   masterdir = mydir,
#'   years = 0:-5
#' )
#'
#' retroModels <- SSgetoutput(
#'   dirvec = file.path(mydir, "retrospectives", paste("retro", 0:-5, sep = ""))
#' )
#' retroSummary <- SSsummarize(retroModels)
#' endyrvec <- retroSummary[["endyrs"]] + 0:-5
#' SSplotComparisons(retroSummary,
#'   endyrvec = endyrvec,
#'   legendlabels = paste("Data", 0:-5, "years")
#' )
#' }
#'
SS_doRetro <- function(dir = getwd(), oldsubdir = "", newsubdir = "retrospectives",
                       subdirstart = "retro", years = 0:-5, overwrite = TRUE,
                       RemoveBlocks = FALSE, verbose = FALSE, exe = "ss", ...) {
  # deprecated variable warnings -----
  # soft deprecated for now, but fully deprecate in the future.
  if (lifecycle::is_present(masterdir)) {
    lifecycle::deprecate_warn(
      when = "1.46.0",
      what = "SS_doRetro(masterdir)",
      details = "Please use 'dir' instead"
    )
    dir <- masterdir
  }

  olddir <- file.path(dir, oldsubdir)
  newdir <- file.path(dir, newsubdir)

  # get model file names from olddir
  startfile <- dir(olddir)[tolower(dir(olddir)) == "starter.ss"]
  if (length(startfile) == 0) {
    stop("No starter.ss file found in ", olddir)
  }

  # read original starter (later written to each folder)
  startfile <- file.path(olddir, startfile)
  starter <- SS_readstarter(startfile, verbose = FALSE)
  subdirnames <- paste0(subdirstart, years)

  # check for executable
  check_exe(exe = exe, dir = subdirnames, verbose = verbose)

  # loop over retrospective years
  for (iyr in 1:length(years)) {
    newdir_iyr <- file.path(newdir, subdirnames[iyr])
    message("Running retrospective in ", newdir_iyr)

    # copy original input files to retro folder
    copy_SS_inputs(
      dir.old = olddir,
      dir.new = newdir_iyr,
      create.dir = TRUE,
      recursive = TRUE,
      overwrite = TRUE,
      verbose = verbose
    )

    # change starter file to do retrospectives
    starter[["retro_yr"]] <- years[iyr]
    starter[["init_values_src"]] <- 0
    SS_writestarter(starter,
      dir = newdir_iyr,
      verbose = FALSE,
      overwrite = TRUE
    )

    # delete covar file to avoid using file from previous model run
    # (not sure if this is necessary)
    if (file.exists("covar.sso")) {
      file.remove("covar.sso")
    }

    # run model
    run(dir = newdir_iyr, verbose = verbose, ...)

    # add rough check for if the model ran (although a report file may exist if
    # if the model only ran part of the way through). Warn the user in this case.
    if (!file.exists("Report.sso")) {
      warning("The retrospective model run failed in ", getwd())
    }
  }
}
