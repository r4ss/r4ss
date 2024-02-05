#' Write all Stock Synthesis input files for a model
#'
#' Writes all the input files for a Stock Synthesis model using the list
#' created by [SS_read()]
#' (presumably after modification of one or more elements)
#' using the `SS_write*()` functions for the four to
#' six model input files.
#'
#' @param inputlist list created by [SS_read()]
#' @template dir
#' @template overwrite
#' @template verbose
#'
#' @author Ian G. Taylor
#'
#' @export
#' @seealso [SS_read()] creates the list that is used by this function.
#' @family read/write functions
#'
#' @examples
#' \dontrun{
#' # read inputlist to modify the data file
#' inputlist <- SS_read(
#'   dir = system.file("extdata", "simple_small", package = "r4ss")
#' )
#'
#' # modify the starter file (use the par file)
#' inputlist[["start"]][["init_values_src"]] <- 1
#'
#' # modify the data file (remove age comps from years prior to 1990)
#' inputlist[["dat"]][["agecomp"]] <- inputlist[["dat"]][["agecomp"]] %>%
#'   dplyr::filter(Yr >= 1990)
#'
#' # modify the control file (turn off early recdevs and change range of yrs)
#' inputlist[["ctl"]][["recdev_early_phase"]] <-
#'   -abs(inputlist[["ctl"]][["recdev_early_phase"]])
#' inputlist[["ctl"]][["MainRdevYrFirst"]] <- 1980
#'
#' # write the files to a new folder within the source directory
#' SS_write(
#'   inputlist = inputlist,
#'   dir = file.path(inputlist[["dir"]], "modified_inputs")
#' )
#' }
SS_write <- function(inputlist,
                     dir = "",
                     overwrite = FALSE,
                     verbose = FALSE) {
  # check for contents of inputlist
  check_inputlist(inputlist)

  # create directory if not already there
  if (dir != "") {
    if (!dir.exists(dir)) {
      if (verbose) {
        message("Creating new directory: ", dir)
      }
      dir.create(dir, recursive = TRUE)
    }
  }

  # write starter file
  if ("start" %in% names(inputlist)) {
    r4ss::SS_writestarter(
      mylist = inputlist[["start"]],
      dir = dir,
      overwrite = overwrite,
      verbose = verbose
    )
  } else {
    stop(
      '"start" element of input list not available ',
      "(required to get names of data and control files)"
    )
  }

  # write data file
  if ("dat" %in% names(inputlist)) {
    r4ss::SS_writedat(
      datlist = inputlist[["dat"]],
      outfile = file.path(dir, inputlist[["start"]][["datfile"]]),
      overwrite = overwrite,
      verbose = verbose
    )
  }

  # write control file
  if ("ctl" %in% names(inputlist)) {
    r4ss::SS_writectl(
      ctllist = inputlist[["ctl"]],
      outfile = file.path(dir, inputlist[["start"]][["ctlfile"]]),
      overwrite = overwrite,
      verbose = verbose
    )
  }

  # write forecast file
  if ("fore" %in% names(inputlist)) {
    r4ss::SS_writeforecast(
      mylist = inputlist[["fore"]],
      dir = dir,
      overwrite = overwrite,
      verbose = verbose
    )
  }

  # write wtatage file (if needed)
  if ("wtatage" %in% names(inputlist) &
    inputlist[["ctl"]][["EmpiricalWAA"]]) {
    r4ss::SS_writewtatage(
      mylist = inputlist[["wtatage"]],
      dir = dir,
      overwrite = overwrite,
      verbose = verbose
    )
  }

  if ("par" %in% names(inputlist)) {
    if (is.null(inputlist[["par"]][["parfile"]])) {
      inputlist[["par"]][["parfile"]] <- "ss3.par"
      warning("parfile name assumed to be 'ss3.par' because it was not specified",
      " by a recent version of SS_write()")
    }
    if (!is.null(inputlist[["par"]])) {
      try(
        {
          if (inputlist[["ctl"]][["ReadVersion"]] == "3.24") {
            par <- r4ss::SS_writepar_3.24(inputlist[["par"]],
              outfile = file.path(dir, inputlist[["par"]][["parfile"]]),
              verbose = verbose
            )
          } else {
            par <- r4ss::SS_writepar_3.30(inputlist[["par"]],
              outfile = file.path(dir, inputlist[["par"]][["parfile"]]),
              verbose = verbose
            )
          }
        },
        silent = !verbose
      )
    }
  }

  # message noting that all files have been written
  if (verbose) {
    message("Wrote all input files to ", dir)
  }
}
