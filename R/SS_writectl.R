#' write control file
#' 
#' Write Stock Synthesis control file. Like \code{\link{SS_readctl}}, this
#' function is not fully developed.
#' 
#' 
#' @param ctllist List object created by \code{\link{SS_readctl}}.
#' @param outfile Filename for where to write new control file.
#' @param overwrite Should existing files be overwritten? Default=F.
#' @param verbose Should there be verbose output while running the file?
#' Default=T.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_readdat}}, \code{\link{SS_readctl}},
#' \code{\link{SS_writestarter}}, \code{\link{SS_writeforecast}},
#' \code{\link{SS_writedat}}, \code{\link{SS_writectl}}
SS_writectl <- function(ctllist,outfile,overwrite=F,verbose=T){
  # function to write Stock Synthesis control files

  stop("sorry, SS_writectl function is not written yet!")

  if(verbose) cat("running SS_writectl\n")

  if(ctllist$type!="Stock_Synthesis_control_file"){
    stop("input 'ctllist' should be a list with $type=='Stock_Synthesis_control_file'")
  }


  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  if(file.exists(outfile)){
    if(!overwrite){
      stop(paste("File exists and input 'overwrite'=F:",outfile))
    }else{
      file.remove(outfile)
    }
  }
  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print(dataframe, row.names=F, strip.white=T)
  }
  oldwidth <- options()$width
  options(width=1000)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)
  wl <- function(name){
    # simple function to clean up many repeated commands
    value = ctllist[names(ctllist)==name]
    writeLines(paste(value," #_",name,sep=""),con=zz)
  }

  writeLines("#C control file created using the SS_writectl function in the r4ss package")
  writeLines(paste("#C should work with SS version:",ctllist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")
  writeLines("# function not written yet")
  writeLines("#")
  writeLines("999")
  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}
