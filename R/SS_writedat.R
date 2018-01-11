#' write Stock Synthesis data file
#'
#' Write Stock Synthesis data file from list object in R which was probably
#' created using \code{\link{SS_readdat}}. This function is a
#' wrapper which calls either SS_writedat_3.24 or SS_writedat_3.30
#' (and potentially additional functions in the future). This setup allows those
#' functions to be cleaner (if somewhat redundant) than a single function that
#' attempts to do everything.
#'
#'
#' @param datlist List object created by \code{\link{SS_readdat}}
#' (or by \code{\link{SS_readdat_3.24}} or \code{\link{SS_readdat_3.24}})
#' @param outfile Filename for where to write new data file.
#' @param version SS version number. Currently only "3.24" or "3.30" are supported,
#' either as character or numeric values (noting that numeric 3.30 = 3.3).
#' @param overwrite Should existing files be overwritten? Default=FALSE.
#' @param faster Speed up writing by writing length and age comps without aligning
#' the columns (by using write.table instead of print.data.frame)
#' @param verbose Should there be verbose output while running the file?
#' @author Ian G. Taylor, Yukio Takeuchi, Gwladys I. Lambert
#' @export
#' @seealso \code{\link{SS_writedat_3.24}}, \code{\link{SS_writedat_3.30}},
#' \code{\link{SS_readdat}}, \code{\link{SS_makedatlist}},
#' \code{\link{SS_readstarter}}, \code{\link{SS_writestarter}},
#' \code{\link{SS_readforecast}}, \code{\link{SS_writeforecast}}
#' 
#'
SS_writedat <- function(datlist,
                        outfile,
                        version = "3.30",
                        overwrite = FALSE,
                        faster = FALSE,
                        verbose = TRUE) {
  # function to write Stock Synthesis data files
  if (verbose){
    message("running SS_writedat")
  }

  # check datlist
  if (datlist$type != "Stock_Synthesis_data_file") {
    stop("input 'datlist' should be a list with $type=='Stock_Synthesis_data_file'")
  }

  # check version input
  if(!(version=="3.24" | version=="3.30" | version==3.3)){
    stop("Input 'version' should be either '3.24' or '3.30'")
  }

  # call function for SS version 3.24
  if(version=="3.24"){ # should work whether "version" is character or numeric
    SS_writedat_3.24(datlist, outfile, overwrite = overwrite,
                     faster = faster, verbose = verbose)
  }

  # call function for SS version 3.30
  if(version=="3.30" | version==3.3){ # turns out 3.30 != "3.30" in R
    SS_writedat_3.30(datlist, outfile, overwrite = overwrite,
                     faster = faster, verbose = verbose)
  }
 
}
