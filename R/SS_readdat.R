#' Read Stock Synthesis data file
#'
#' Read Stock Synthesis data file into list object in R. This function is a
#' wrapper that calls
#' [SS_readdat_2.00()], [SS_readdat_3.00()],
#' [SS_readdat_3.24()], or [SS_readdat_3.30()]
#' (and potentially additional functions in the future). This setup allows those
#' functions to be cleaner (if somewhat redundant) than a single function that
#' attempts to do everything.
#' The returned list is mostly consistent across versions.
#'
#' @template file
#' @param version Deprecated. SS3 version number, where only versions
#'   "3.24" or newer are supported and this can be found in the input file
#'   rather than supplying it as an argument.
#' @param verbose Should there be verbose output while running the file?
#' Default=TRUE.
#' @param echoall Debugging tool (not fully implemented) of echoing blocks of
#' data as it is being read.
#' @param section Which data set to read. Only applies for a data.ss_new file
#' created by Stock Synthesis. Allows the choice of either expected values
#' (section=2) or bootstrap data (section=3+). Leaving default of section=NULL
#' will read input data, (equivalent to section=1).
#' @author Ian G. Taylor, Allan C. Hicks, Neil L. Klaer, Kelli F. Johnson,
#' Chantel R. Wetzel
#' @export
#' @seealso [SS_readdat_2.00()], [SS_readdat_3.00()],
#' [SS_readdat_3.24()], [SS_readdat_3.30()],
#' [SS_readctl()], [SS_readctl_3.24()]
#' [SS_readstarter()], [SS_readforecast()],
#' [SS_writestarter()],
#' [SS_writeforecast()], [SS_writedat()]

SS_readdat <- function(file,
                       version = lifecycle::deprecated(),
                       verbose = TRUE,
                       echoall = FALSE,
                       section = NULL) {

  # warn about soft deprecated arguments ----
  if (lifecycle::is_present(version)) {
    lifecycle::deprecate_warn(
      when = "1.43.0",
      what = "SS_readctl(version)",
      details = "`version` is replaced by [version_search(file)] that finds the version."
    )
  }
  nver <- get_version_search(file,
    allow = c("2.00", "3.00", "3.24", "3.30"),
    verbose = verbose
  )

  # call function for SS version 2.00 ----
  if (nver < 3) {
    datlist <- SS_readdat_2.00(
      file = file, verbose = verbose,
      echoall = echoall, section = section
    )
  }

  # call function for SS version 3.00 ----
  if ((nver >= 3) && (nver < 3.2)) {
    datlist <- SS_readdat_3.00(
      file = file, verbose = verbose,
      echoall = echoall, section = section
    )
  }

  # call function for SS version 3.24 ----
  if ((nver >= 3.2) && (nver < 3.3)) {
    datlist <- SS_readdat_3.24(
      file = file, verbose = verbose,
      echoall = echoall, section = section
    )
  }
  # call function for SS version 3.30 ----
  if (nver >= 3.3) {
    datlist <- SS_readdat_3.30(
      file = file, verbose = verbose,
      echoall = echoall, section = section
    )
  }
  # return the result
  return(datlist)
}
