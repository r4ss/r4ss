#' Srite Stock Synthesis control file
#'
#' Write Stock Synthesis control file from list object in R which was probably
#' created using \code{\link{SS_readctl}}. This function is a
#' wrapper which calls either SS_writectl_3.24 or SS_writectl_3.30
#' (and potentially additional functions in the future).
#'
#' @param ctllist List object created by \code{\link{SS_readdat}}.
#' @param outfile Filename for where to write new control file.
#' @param version SS version number. Currently only "3.24" or "3.30" are supported,
#' either as character or numeric values (noting that numeric 3.30 = 3.3).
#' Defaults to NULL, which means that the function will attempt to determine the
#' version from \code{ctllist}.
#' @param overwrite Should existing files be overwritten? Defaults to FALSE.
#' @param verbose Should there be verbose output while running the file?
#' Defaults to TRUE.
#' @author Ian G. Taylor, Yukio Takeuchi, Gwladys I. Lambert, Kathryn Doering
#' @export
#' @seealso \code{\link{SS_writedat_3.24}}, \code{\link{SS_writedat_3.30}},
#' \code{\link{SS_readdat}}, \code{\link{SS_makedatlist}},
#' \code{\link{SS_readstarter}}, \code{\link{SS_writestarter}},
#' \code{\link{SS_readforecast}}, \code{\link{SS_writeforecast}}
SS_writectl <- function(ctllist, outfile, version = NULL, overwrite = FALSE,
                        verbose = TRUE) {
  # function to write Stock Synthesis data files
  if (verbose) {
    message("Running SS_writectl")
  }
  # Check user inputs are valid to avoid issues with functions.
  # check ctllist
  if (ctllist$type != "Stock_Synthesis_control_file") {
    stop("Input 'ctllist' should be a list with component type == 'Stock_Synthesis_control_file")
  }
  # check version input
  if(is.null(version)) {
  version <- ctllist$ReadVersion
  }
  if(!(version == "3.24" | version == "3.30" | version == 3.3)) {
    stop("Input 'version' should be either '3.24' or '3.30'")
  }
  # Check user inputs and/or prepare the file to be overwitten.
  if(file.exists(outfile)) {
    if(!overwrite) {
      stop("Outfile called ", outfile," exists and input 'overwrite'= FALSE.",
           "Please set overwrite = TRUE if you wish to overwrite the file.")
    } else if(overwrite) {
      file.remove(outfile)
    }
  }
  
  # function call depends on user version
  if(version == "3.24") {
    # Specify nseas, N_areas, and Do_AgeKey as input.
    SS_writectl_3.24(ctllist, outfile, overwrite = overwrite, verbose = verbose)
  }
  if(version == "3.30" | version == 3.3) {
    # This function will get nseas, N_areas, and Do_AgeKey from ctllist.
    SS_writectl_3.30(ctllist, outfile, overwrite, verbose)
  }
  # because this function is used for its side effects (i.e., writing to disk),
  # return its first argument invisibly.
  invisible(ctllist) 
}
