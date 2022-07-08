#' Read all Stock Synthesis input files for a model
#'
#' Read all the input files for a Stock Synthesis model into R as a list object.
#' These files will be in a single directory on your machine, i.e., `dir`.
#' Functionality comes from the `r4ss::SS_read*()` functions.
#' This function simplifies the number of lines of code you need to write by
#' using all of the read functions to read in the
#' starter, control, data, and forecast files and if requested, the
#' weight-at-age file. The starter file is helpful because it provides names for
#' the control and data files.
#'
#' @param dir A file path to the directory of interest or a raw github URL (see
#' example). The default is the current working directory, `dir = getwd()`.
#' @param ss_new A logical that controls if the `.ss_new` files or
#'   the original input files are read in.
#'   The default is to read the original files.
#' @template verbose
#' @author Ian G. Taylor, Kelli F. Johnson
#' @return An invisible list is returned.
#' The first element is the directory that was provided in the argument `dir`.
#' The second element is the result of `normalizePath(dir)`,
#' which gives the full path.
#' The remaining four to six elements are list objects from reading in
#' the following input files:
#' * data
#' * control
#' * starter
#' * forecast
#' * wtatage (will be NULL if not required by the model)
#' * par (will be null if control and par do not match)
#'
#' @export
#' @seealso
#' * [SS_write()] can be used to write the input files using the list
#'   created by this function.
#' * [SS_readstarter()], [SS_readdat()], [SS_readctl()],
#'   [SS_readforecast()], [SS_readwtatage()],
#'   [SS_readpar_3.30()], and [SS_readpar_3.24()] used by this
#'   function to read in the input files.
#' * [SS_output()] to read in equivalent SS3 output files.
#'
#' @examples
#' # Read in the 'simple' example model stored in {r4ss}
#' inputs <- SS_read(
#'   dir = system.file("extdata", "simple_3.30.13", package = "r4ss")
#' )
#' # Read in an example from GitHub stored in user-examples,
#' # wrapped in `dontrun` because it requires an Internet connection
#' \dontrun{
#' webexample <- SS_read(dir = file.path(
#'   "https://raw.githubusercontent.com",
#'   "nmfs-stock-synthesis",
#'   "user-examples",
#'   "main",
#'   "model_files",
#'   "simple_long_wtatage"
#' ))
#' }
SS_read <- function(dir = getwd(),
                    ss_new = FALSE,
                    verbose = FALSE) {

  # Read in starter first to find the names of the input files
  start <- SS_readstarter(
    file = file.path(dir, ifelse(ss_new, "starter.ss_new", "starter.ss")),
    verbose = verbose
  )

  if (ss_new) {
    datname <- get_dat_new_name(dir)
    ctlname <- "control.ss_new"
  } else {
    datname <- start[["datfile"]]
    ctlname <- start[["ctlfile"]]
  }

  # Must read in dat file to read in ctl file with less commands
  dat <- r4ss::SS_readdat(
    file = file.path(dir, datname),
    verbose = verbose
  )
  ctl <- r4ss::SS_readctl(
    file = file.path(dir, ctlname),
    datlist = dat,
    verbose = verbose
  )
  fore <- r4ss::SS_readforecast(
    file = file.path(dir, ifelse(ss_new, "forecast.ss_new", "forecast.ss")),
    verbose = verbose
  )
  if (ctl[["EmpiricalWAA"]]) {
    wtatage <- r4ss::SS_readwtatage(
      file = file.path(dir, ifelse(ss_new, "wtatage.ss_new", "wtatage.ss")),
      verbose = verbose
    )
  } else {
    wtatage <- NULL
  }

  par <- NULL
  if (file.exists(file.path(dir, "ss.par"))) {
    try(
      {
        if (ctl[["ReadVersion"]] == "3.24") {
          par <- r4ss::SS_readpar_3.24(file.path(dir, "ss.par"),
            datsource = dat,
            ctlsource = ctl,
            verbose = verbose
          )
        } else {
          par <- r4ss::SS_readpar_3.30(file.path(dir, "ss.par"),
            datsource = dat,
            ctlsource = ctl,
            verbose = verbose
          )
        }
      },
      silent = !verbose
    )
  }

  # return a list of the lists for each file
  invisible(list(
    dir = dir,
    path = normalizePath(dir, mustWork = FALSE),
    dat = dat,
    ctl = ctl,
    start = start,
    fore = fore,
    wtatage = wtatage,
    par = par
  ))
}
