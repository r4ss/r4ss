#' Write Stock Synthesis control file
#'
#' Write Stock Synthesis control file from list object in R which was probably
#' created using [SS_readctl()]. This function is a
#' wrapper which calls either SS_writectl_3.24 or SS_writectl_3.30
#' (and potentially additional functions in the future).
#'
#' @param ctllist List object created by [SS_readdat()].
#' @param outfile Filename for where to write new control file.
#' @template version
#' @param overwrite Should existing files be overwritten? Defaults to FALSE.
#' @param verbose Should there be verbose output while running the file?
#' Defaults to FALSE.
#' @author Ian G. Taylor, Yukio Takeuchi, Gwladys I. Lambert, Kathryn Doering
#' @export
#' @seealso [SS_writedat_3.24()], [SS_writedat_3.30()],
#' [SS_readdat()],
#' [SS_readstarter()], [SS_writestarter()],
#' [SS_readforecast()], [SS_writeforecast()]
SS_writectl <- function(ctllist,
                        outfile,
                        version = "3.30",
                        overwrite = FALSE,
                        verbose = FALSE) {
  # function to write Stock Synthesis data files
  if (verbose) {
    message("Running SS_writectl")
  }
  # Check user inputs are valid to avoid issues with functions.
  # check ctllist
  stopifnot(is.list(ctllist))
  stopifnot("type" %in% names(ctllist))
  if (ctllist[["type"]] != "Stock_Synthesis_control_file") {
    stop("Input 'ctllist' should be a list with component type == 'Stock_Synthesis_control_file")
  }
  if (is.null(version)) {
    lifecycle::deprecate_stop(
      when = "1.44.1",
      what = "SS_readctl(version = 'must be 3.24 or 3.30')"
    )
    version <- "3.30"
  }
  if (ifelse(version == "3.3", "3.30", version) != ctllist[["ReadVersion"]]) {
    stop(
      "Input 'version' does not match ctllist[['ReadVersion']] of ",
      "'", ctllist[["ReadVersion"]], "'."
    )
  }
  if (!(version == "3.24" | version == "3.30" | version == 3.3)) {
    stop("Input 'version' should be either '3.24' or '3.30'")
  }
  # Check user inputs and/or prepare the file to be overwitten.
  if (file.exists(outfile)) {
    if (!overwrite) {
      stop(
        "Outfile called ", outfile, " exists and input 'overwrite'= FALSE.",
        "Please set overwrite = TRUE if you wish to overwrite the file."
      )
    } else if (overwrite) {
      file.remove(outfile)
    }
  }

  # function call depends on user version
  if (version == "3.24") {
    # Specify nseas, N_areas, and Do_AgeKey as input.
    SS_writectl_3.24(ctllist, outfile, overwrite = overwrite, verbose = verbose)
  }
  if (version == "3.30" | version == 3.3) {
    # This function will get nseas, N_areas, and Do_AgeKey from ctllist.
    SS_writectl_3.30(
      ctllist = ctllist,
      outfile = outfile,
      overwrite = overwrite,
      verbose = verbose
    )
  }
  # because this function is used for its side effects (i.e., writing to disk),
  # return its first argument invisibly.
  invisible(ctllist)
}
