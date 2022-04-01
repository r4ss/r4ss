#' Write all Stock Synthesis input files for a model
#'
#' Writes all the input files for a Stock Synthesis model using the list
#' created by [SS_read()]
#' (presumably after modification of one or more elements)
#' using the `SS_write*()` functions for the four or
#' five model input files.
#'
#' @param inputlist list created by [SS_read()]
#' @template dir
#' @param files vector of file types to write. The default is the four
#' standard input files plus wtatage.ss if used. The ss.par file is not
#' yet supported by this function or [SS_read()].
#' @template overwrite
#' @template verbose
#'
#' @author Ian G. Taylor
#'
#' @export
#' @seealso
#' * [SS_read()] creates the list that is used by this function.
#' * [SS_writestarter()], [SS_writedat()], [SS_writectl()],
#' [SS_writeforecast()], and [SS_writewtatage()] are used to write the
#' input files.

#' @examples
#' \dontrun{
#' # read inputs to modify the data file
#' inputs <- SS_read(
#'   dir = system.file("extdata", "simple_3.30.13", package = "r4ss")
#' )
#'
#' # modify the starter file (use the par file)
#' inputs$start$init_values_src <- 1
#'
#' # modify the data file (remove age comps from years prior to 1990)
#' inputs$dat$agecomp <- inputs$dat$agecomp %>% dplyr::filter(Yr >= 1990)
#'
#' # modify the control file (turn off early recdevs and change range of yrs)
#' inputs$ctl$recdev_early_phase <- -abs(inputs$ctl$recdev_early_phase)
#' inputs$ctl$MainRdevYrFirst <- 1980
#'
#' # write the files to a new folder within the source directory
#' SS_write(inputs = inputs,
#'          dir = file.path(inputs$dir, "modified_inputs"))
#' }

SS_write <- function(inputlist,
                     dir = "",
                     files = c("dat", "ctl", "start", "fore", "wtatage"),
                     overwrite = FALSE,
                     verbose = FALSE) {
  # create directory if not already there
  if (dir != "") {
    if (!dir.exists(dir)) {
      if (verbose) {
        message("Creating new directory: ", dir)
      }
      dir.create(dir)
    }
  }

  # write data file
  if ("dat" %in% files) {
    r4ss::SS_writedat(inputs$dat,
      outfile = file.path(dir, inputs$start$datfile),
      overwrite = overwrite,
      verbose = verbose
    )
  }
  # write control file
  if ("ctl" %in% files) {
    r4ss::SS_writectl(inputs$ctl,
      outfile = file.path(dir, inputs$start$ctlfile),
      overwrite = overwrite,
      verbose = verbose
    )
  }
  # write starter file
  if ("start" %in% files) {
    r4ss::SS_writestarter(inputs$start,
      dir = dir,
      overwrite = overwrite,
      verbose = verbose,
      warn = FALSE
    )
  }
  # write forecast file
  if ("fore" %in% files) {
    r4ss::SS_writeforecast(inputs$fore,
      dir = dir,
      overwrite = overwrite,
      verbose = verbose
    )
  }
  # write wtatage file (if needed)
  if ("wtatage" %in% files & inputs$ctl$EmpiricalWAA) {
    r4ss::SS_writewtatage()(inputs$wtatage,
      dir = dir,
      overwrite = overwrite,
      verbose = verbose
    )
  }
  # message noting that all files have been written
  if (verbose) {
    message("Wrote all input files to ", dir)
  }
}
