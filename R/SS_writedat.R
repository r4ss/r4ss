#' write Stock Synthesis data file
#'
#' Write Stock Synthesis data file from list object in R which was probably
#' created using [SS_readdat()]. This function is a
#' wrapper which calls either SS_writedat_3.30()(previously also
#' SS_writedat_3.24(), but that function has been deprecated).
#'
#' @param datlist List object created by [SS_readdat()]
#' (or by [SS_readdat_3.24()] or [SS_readdat_3.24()])
#' @param outfile Filename for where to write new data file.
#' @template version
#' @template overwrite
#' @param faster Deprecated. Speed up writing by writing length and age comps without aligning
#' the columns (by using write.table instead of print.data.frame)
#' @template verbose
#' @author Ian G. Taylor, Yukio Takeuchi, Gwladys I. Lambert
#' @export
#' @family read/write functions

SS_writedat <- function(
  datlist,
  outfile,
  version = "3.30",
  overwrite = FALSE,
  faster = lifecycle::deprecated(),
  verbose = TRUE
) {
  # function to write Stock Synthesis data files
  if (lifecycle::is_present(faster)) {
    lifecycle::deprecate_warn(
      when = "1.45.0",
      what = "SS_writedat(faster)"
    )
  }
  if (verbose) {
    message("running SS_writedat")
  }

  # check datlist
  if (datlist[["type"]] != "Stock_Synthesis_data_file") {
    stop(
      "input 'datlist' should be a list with $type=='Stock_Synthesis_data_file'"
    )
  }

  # check version input
  if (!(version == "3.24" | version == "3.30" | version == 3.3)) {
    stop("Input 'version' should be either '3.24' or '3.30'")
  }

  # call function for SS version 3.24
  if (version == "3.24") {
    # should work whether "version" is character or numeric
    SS_writedat_3.24(datlist, outfile, overwrite = overwrite, verbose = verbose)
  }

  # call function for SS version 3.30
  if (version == "3.30" | version == 3.3) {
    # turns out 3.30 != "3.30" in R
    SS_writedat_3.30(datlist, outfile, overwrite = overwrite, verbose = verbose)
  }
}
