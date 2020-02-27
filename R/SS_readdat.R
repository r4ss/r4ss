#' read Stock Synthesis data file
#'
#' Read Stock Synthesis data file into list object in R. This function is a
#' wrapper which calls SS_readdat_2.00, SS_readdat_3.00, SS_readdat_3.24, or SS_readdat_3.30
#' (and potentially additional functions in the future). This setup allows those
#' functions to be cleaner (if somewhat redundant) than a single function that
#' attempts to do everything. Returned datlist is mostly consistent across versions.
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param version SS version number.
#' Currently "2.00", "3.00", "3.24" or "3.30" are supported,
#' either as character or numeric values (noting that numeric 3.30  = 3.3).
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
#' @seealso \code{\link{SS_readdat_2.00}}, \code{\link{SS_readdat_3.00}},
#' \code{\link{SS_readdat_3.24}}, \code{\link{SS_readdat_3.30}},
#' \code{\link{SS_readctl}}, \code{\link{SS_readctl_3.24}}
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}}

SS_readdat <- function(file, version=NULL, verbose=TRUE,echoall=FALSE,section=NULL){

  # automatic testing of version number ----
  if(is.null(version)) {
    # look for 3.24 or 3.30 at the top of the chosen file
    version <- scan(file, what=character(), nlines=5, quiet=!verbose)
    version <- substring(version,3,6)
    version <- version[version %in% c("3.24", "3.30")]
    # if that fails, look for data.ss_new file in the same directory
    if(length(version) > 0){
      if(verbose)cat("assuming version", version, "based on first five lines of data file\n")
    }else{
      newfile <- file.path(dirname(file), "data.ss_new")
      if(file.exists(newfile)){
        version <- scan(newfile, what=character(), nlines=1, quiet=!verbose)
        version <- substring(version,3,6)
        if(verbose)cat("assuming version", version, "based on first line of data.ss_new\n")
      }else{
        stop("input 'version' required due to missing value at top of", file)
      }
    }
  }
  nver <- as.numeric(substring(version,1,4))
  if(verbose) cat("Char version is ", version, "\n")
  if(verbose) cat("Numeric version is ", nver, "\n")

  # call function for SS version 2.00 ----
  if(nver<3){
    datlist <- SS_readdat_2.00(file=file, verbose=verbose,
                               echoall=echoall, section=section)
  }

  # call function for SS version 3.00 ----
  if((nver>=3)&&(nver<3.2)){
    datlist <- SS_readdat_3.00(file=file, verbose=verbose,
                               echoall=echoall, section=section)
  }
  
  # call function for SS version 3.24 ----
  if((nver>=3.2)&&(nver<3.3)){
    datlist <- SS_readdat_3.24(file=file, verbose=verbose,
                               echoall=echoall, section=section)
  }
  # call function for SS version 3.30 ----
  if(nver>=3.3){
    datlist <- SS_readdat_3.30(file=file, verbose=verbose,
                               echoall=echoall, section=section)
  }
  # return the result
  return(datlist)
}
