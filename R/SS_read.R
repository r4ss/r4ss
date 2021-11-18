#' Get Stock Synthesis input files for a model
#'
#' Read all the input files for a Stock Synthesis model into R as a list object.
#' These files will be in a single directory on your machine, i.e., `dir`.
#' Functionality comes from the `r4ss::SS_read*()` functions.
#' This function simplifies the number of lines of code you need to write by
#' using all of the read functions to read in the
#' starter, control, data, and forecast files.
#' The starter file is helpful because it provides names for the
#' control and data files.
#'
#' @template dir
#' @param ss_new A logical that controls if the `.ss_new` files or
#'   the original input files are read in.
#'   The default is to read the original files.
#' @template verbose
#'
#' @author Ian G. Taylor created this function while
#' conducting the 2021 lingcod stock assessment.
#'
#' @return An invisible list is returned.
#' The first element is the directory that was provided in the argument `dir`.
#' The second element is the result of [normalizePath(dir)],
#' which gives the full path.
#' The remaining four elements are list objects from reading in
#' the following input files:
#' * data
#' * control
#' * starter
#' * forecast
#'
#' @export
#' @seealso
#' * [SS_readstarter()], [SS_readdat()], [SS_readctl()], and [SS_readforecast()]
#'   are used to read in the input files.
#' * [SS_output()] to read in equivalent SS output files.
#'
#' @examples
#' # Read in the 'simple' example model stored in {r4ss}
#' inputs <- SS_read(
#'   dir = system.file("extdata", "simple_3.30.13", package = "r4ss")
#' )

SS_read <- function(dir = NULL,
                    ss_new = FALSE,
                    verbose = FALSE) {

  if (is.null(dir)) {
    dir <- getwd()
  }

  # Read in starter first to find the names of the input files
  start <- SS_readstarter(
    file = file.path(dir, ifelse(ss_new, "starter.ss_new", "starter.ss")),
    verbose = verbose
  )

  if (ss_new) {
    datname <- "data.ss_new"
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

  # return a list of the lists for each file
  invisible(list(
    dir = dir,
    path = normalizePath(dir),
    dat = dat,
    ctl = ctl,
    start = start,
    fore = fore
  ))
}
