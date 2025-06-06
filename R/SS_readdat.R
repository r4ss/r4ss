#' read Stock Synthesis data file
#'
#' Read Stock Synthesis data file into list object in R. This function is a
#' wrapper which calls SS_readdat_3.30 (previously additional functions,
#' but they have been deprecated).
#'
#' @template file
#' @param version SS version number.
#'  Currently "2.00", "3.00", "3.24" or "3.30" are supported, but all
#'  versions prior to "3.30" have been deprecated.
#'  either as character or numeric values (noting that numeric 3.30  = 3.3). If
#'  version is NULL, the version (3.24 or 3.30) will be looked for on the first
#'  line of the file.
#' @template verbose
#' @param echoall Deprecated.
#' @param section Which data set to read. Only applies for a data.ss_new file
#' created by Stock Synthesis. Allows the choice of either expected values
#' (section=2) or bootstrap data (section=3+). Leaving default of section=NULL
#' will read input data, (equivalent to section=1).
#' @author Ian G. Taylor, Allan C. Hicks, Neil L. Klaer, Kelli F. Johnson,
#' Chantel R. Wetzel, Kathryn L. Doering, Nathan R. Vaughan
#' @export
#' @family read/write functions

SS_readdat <- function(
  file,
  version = "3.30",
  verbose = TRUE,
  echoall = lifecycle::deprecated(),
  section = NULL
) {
  if (is.null(version)) {
    lifecycle::deprecate_warn(
      when = "1.44.1",
      what = "SS_readctl(version = 'must be 3.24 or 3.30')"
    )
    version <- "3.30"
  }
  if (lifecycle::is_present(echoall)) {
    lifecycle::deprecate_warn(
      when = "1.45.0",
      what = "SS_readdat(echoall)",
      with = "SS_readdat(verbose)"
    )
  }
  nver <- as.numeric(substring(version, 1, 4))

  # call function for SS version 2.00 ----
  if (nver < 3) {
    datlist <- SS_readdat_2.00(
      file = file,
      verbose = verbose,
      section = section
    )
  }

  # call function for SS version 3.00 ----
  if ((nver >= 3) && (nver < 3.2)) {
    datlist <- SS_readdat_3.00(
      file = file,
      verbose = verbose,
      section = section
    )
  }

  # call function for SS version 3.24 ----
  if ((nver >= 3.2) && (nver < 3.3)) {
    datlist <- SS_readdat_3.24(
      file = file,
      verbose = verbose,
      section = section
    )
  }
  # call function for SS version 3.30 ----
  if (nver >= 3.3) {
    datlist <- SS_readdat_3.30(
      file = file,
      verbose = verbose,
      section = section
    )
  }
  # return the result
  return(datlist)
}
