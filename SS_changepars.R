SS_changepars <-
function(
         dir="C:/myfiles/mymodels/myrun/",
         ctlfile="control.ss_new",
         newctlfile="control_modified.ss",
         linenums=NULL, strings=NULL, newvals=NULL,
         estimate=FALSE, verbose=TRUE
         )
{
################################################################################
#
# SS_changepars November 19, 2008.
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: To change one or more parameter values in the Control file for SSv3
# Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
# Returns: writes a new control file and returns a table of the changes made
# Notes:   requires SS_parlines
#          See users guide for documentation: http://code.google.com/p/r4ss/wiki/
# Required packages: none
#
################################################################################

  # read control file
  fullctlfile <- paste(dir,ctlfile,sep="/")
  ctl = readLines(fullctlfile)

  if(is.null(linenums) & !is.null(strings) & class(strings)=="character")
  {
    ctltable <- SS_parlines(ctlfile=fullctlfile)
    allnames <- ctltable$Label
    goodnames <- NULL
    if(!is.null(strings)){
      for(i in 1:length(strings)) goodnames <- c(goodnames,allnames[grep(strings[i],allnames)])
      goodnames <- unique(goodnames)
      cat("parameter names in control file matching input vector 'strings' (n=",length(goodnames),"):\n",sep="")
      print(goodnames)
      if(length(goodnames)==0){
        stop("No parameters names match input vector 'strings'")
      }
    }
    nvals <- length(goodnames)
    cat('These are the ctl file lines as they currently exist:\n')
    print(ctltable[ctltable$Label %in% goodnames,])
    for(i in 1:nvals) linenums[i] <- ctltable$Linenum[ctltable$Label==goodnames[i]]
  }else{
    if(is.null(linenums)) stop("valid input needed for either 'linenums' or 'strings'")
  }
  ctlsubset <- ctl[linenums]
  cat("line numbers in control file (n=",length(linenums),"):\n",sep="")
  print(linenums)
  # define objects to store changes
  newctlsubset <- NULL
  cmntvec <- NULL
  nvals <- length(linenums)
  oldvals <- oldphase <- newphase <- rep(NA,nvals)

  # check inputs
  if(!is.null(newvals) & length(newvals)!=nvals) stop("'newvals' and either 'linenums' or 'strings' should have the same number of elements")
  if(!(length(estimate) %in% c(1,nvals))) stop("'estimate' should have 1 element or same number as 'newvals'")
  if(length(estimate)==1) estimate <- rep(estimate, nvals)

  if(is.data.frame(newvals)) newvals <- as.numeric(newvals)
  if(is.null(newvals)) stop("Nothing input for 'newvals'")
  
  # loop over line numbers to replace parameter values
  for(i in 1:nvals)
  {
    splitline <- strsplit(ctlsubset[i], "#")[[1]]
    cmnt <- paste("#",paste(splitline[-1],collapse="#"),sep='')
    cmntvec <- c(cmntvec, cmnt)
    vecstrings <- strsplit(splitline[1],split="[[:blank:]]+")[[1]]
    vec <- as.numeric(vecstrings[vecstrings!=""])
    if(max(is.na(vec))==1) stop("There's a problem with a non-numeric value in line",linenums[i])
    oldvals[i] <- vec[3]
    if(!is.null(newvals)) vec[3] <- newvals[i]
    oldphase[i] <- as.numeric(vec[7])
    if(estimate[i]){
      vec[7] <- abs(oldphase[i])
    }else{
      vec[7] <- -abs(oldphase[i])
    }
    if(vec[3] < vec[1])
      cat("!warning: new value",vec[3],"is below lower bound ",vec[1],"for",cmnt,"\n")
    if(vec[3] > vec[2])
      cat("!warning: new value",vec[3],"is above upper bound ",vec[2],"for",cmnt,"\n")

    newphase[i] <- vec[7]
    newline <- paste("",paste(vec, collapse=" "), cmnt)
    newctlsubset <- rbind(newctlsubset, newline)
  }
  # write new file
  newctl <- ctl
  newctl[linenums] <- newctlsubset
  writeLines(newctl, paste(dir,newctlfile,sep="/"))
  if(verbose) cat('\nwrote new file to',newctlfile,'with the following changes:\n')
  results <- data.frame(oldvals, newvals, oldphase, newphase, comment=cmntvec)
  # output table of changes
  if(is.null(newvals)) newvals <- NA
  if(verbose) print(results)
  return(invisible(results))

} # end function

