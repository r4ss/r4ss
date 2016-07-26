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
#' @param version SS version number. Currently only "3.24" or "3.30" are supported,
#' either as character or numeric values (noting that numeric 3.30  = 3.3).
#' @param verbose Should there be verbose output while running the file?
#' Default=TRUE.
#' @param echoall Debugging tool (not fully implemented) of echoing blocks of
#' data as it is being read.
#' @param section Which data set to read. Only applies for a data.ss_new file
#' created by Stock Synthesis. Allows the choice of either expected values
#' (section=2) or bootstrap data (section=3+). Leaving default of section=NULL
#' will read input data, (equivalent to section=1).
#' @author Ian G. Taylor, Allan C. Hicks
#' @export
#' @seealso \code{\link{SS_readdat_3.24}}, \code{\link{SS_readdat_3.30}}
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}}

SS_readdat <- function(file, version="3.24", verbose=TRUE,echoall=FALSE,section=NULL){
  # wrapper function to call old or new version of SS_readdat

  # automatic testing of version number (not yet used by default)
  if(is.null(version)) {
    # look for 3.24 or 3.30 at the top of the chosen file
    version <- scan(file, what=character(), nlines=1)
    version <- substring(version,3,6)
    # if that fails, look for data.ss_new file in the same directory
    if(version %in% c("3.24", "3.30")){
      cat("assuming version", version, "based on first line of data file\n")
    }else{
      newfile <- file.path(dirname(file), "data.ss_new")
      if(file.exists(newfile)){
        version <- scan(newfile, what=character(), nlines=1)
        version <- substring(version,3,6)
        cat("assuming version", version, "based on first line of data.ss_new\n")
      }else{
        stop("input 'version' required due to missing value at top of", file)
      }
    }
  }

  # call function for SS version 3.24
  if(version=="3.24"){ # should work whether "version" is character or numeric
    datlist <- SS_readdat_3.24(file=file, verbose=verbose,
                               echoall=echoall, section=section)
  }

  # call function for SS version 3.30
  if(version=="3.30" | version==3.3){ # turns out 3.30 != "3.30" in R
    datlist <- SS_readdat_3.30(file=file, verbose=verbose,
                               echoall=echoall, section=section)
  }

  # return the result
  return(datlist)
}
