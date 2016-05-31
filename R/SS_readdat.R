#' read data file from SS
#' 
#' Read Stock Synthesis data file into list object in R. This function is a
#' wrapper which calls either SS_readdat_3.24 or SS_readdat_3.30
#' (and potentially additional functions in the future). This setup allows those
#' functions to be cleaner (if somewhat redundant) than a single function that
#' attempts to do everything.
#' 
#' 
#' @param file Filename either with full path or relative to working directory.
#' @param verbose Should there be verbose output while running the file?
#' Default=TRUE.
#' @param echoall Debugging tool (not fully implemented) of echoing blocks of
#' data as it is being read.
#' @param section Which data set to read. Only applies for a data.ss_new file
#' created by Stock Synthesis. Allows the choice of either expected values
#' (section=2) or bootstrap data (section=3+). Leaving default of section=NULL
#' will read input data, (equivalent to section=1).
#' @author Ian G. Taylor
#' @export
#' @seealso \code{\link{SS_readdat_3.24}}, \code{\link{SS_readdat_3.30}}
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_readctl}}, \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}},
#' \code{\link{SS_writectl}}
#' @keywords data

SS_readdat <- function(version="3.24", ...){
  # wrapper function to call old or new version of SS_readdat

  # call function for SS version 3.24
  if(version=="3.24"){ # should work whether "version" is character or numeric
    datlist <- SS_readdat_3.24(...)
  }

  # call function for SS version 3.30
  if(version=="3.30" | version==3.3){ # turns out 3.30 != "3.30" in R
    datlist <- SS_readdat_3.30(...)
  }

  # return the result
  return(datlist)
}
