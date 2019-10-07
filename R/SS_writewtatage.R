#' Write weight-at-age file
#'
#' Write Stock Synthesis weight-at-age file from R object that was probably
#' created using \code{\link{SS_readwtatage}}
#'
#' @param mylist Object created by \code{\link{SS_readwtatage}}.
#' @template dir
#' @param file Filename for new weight-at-age file, which 
#' will be appended to \code{dir} to create a full file path.
#' Default="wtatage.ss".
#' @template overwrite
#' @template verbose
#' @template warn
#' @author Kelli Faye Johnson
#' @export
#' @seealso \code{\link{SS_readwtatage}}
#' 
SS_writewtatage <- function(mylist, dir=NULL, file="wtatage.ss",
                            overwrite=FALSE, verbose=TRUE, warn=TRUE){
  if(verbose) message("running SS_writewtatage\n")

  # Prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  on.exit({if(sink.number()>0) sink()})

  if(is.null(dir)) dir <- getwd() # set to working directory if no input provided
  if(grepl("/$", dir)) {
    outfile <- paste0(dir, file) # bc trailing backslash
  } else {
    outfile <- file.path(dir,file)
  }
  if(file.exists(outfile)){
    if(!overwrite){
      stop("file exists:",outfile,"\n  set overwrite=TRUE to replace\n")
    }else{
      if(warn) {message("overwriting file:",outfile,"\n")}
      file.remove(outfile)
    }
  }else{
    if(verbose)message("writing new file:",outfile,"\n")
  }

  # record current max characters per line and then expand in case of long lines
  oldwidth <- options()$width
  options(width=1000)

  if(verbose) message("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  on.exit(close(zz))
  sink(zz)

  writeLines(paste(NCOL(mylist)-7, "# maxage"))
  writeLines("# if Yr is negative, then fill remaining years for that Seas, growpattern, Bio_Pattern, Fleet")
  writeLines("# if season is negative, then fill remaining fleets for that Seas, Bio_Pattern, Sex, Fleet")
  writeLines("# will fill through forecast years, so be careful")
  writeLines("# fleet 0 contains begin season pop WT")
  writeLines("# fleet -1 contains mid season pop WT")
  writeLines("# fleet -2 contains maturity*fecundity")
  
  # Check for terminal line in data frame
  mylist <- mylist[order(mylist$Yr, mylist$Fleet, mylist$Seas), ]
  if(any(mylist$Yr < -998)){
    mylist <- mylist[c(
      which(mylist$Yr >= -998),
      which(mylist$Yr < -998)), ]
  }else{
    mylist <- rbind(mylist, mylist[1,])
    mylist[NROW(mylist), "Yr"] <- -9999
  }
  colnames(mylist)[1] <- paste0("#", colnames(mylist)[1])
  print.data.frame(mylist, row.names=FALSE, strip.white=TRUE)

  # restore printing width to whatever the user had before
  options(width=oldwidth)
  sink()
  if(verbose)message("file written to",outfile,"\n")
}
