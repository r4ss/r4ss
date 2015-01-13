#' Change parameters in the control file.
#' 
#' A function to take advantage of \code{\link{SS_parlines}} that could be used
#' to create a series of control files with different parameter values. This is
#' used by \code{\link{SS_profile}}, but may also be useful for simulation
#' work.
#' 
#' 
#' @param dir Directory with control file to change.
#' @param ctlfile Control file name. Default="control.ss_new".
#' @param newctlfile Name of new control file to be written.
#' Default="control_modified.ss".
#' @param linenums Line numbers of control file to be modified. Either this or
#' the Strings input are needed. Default=NULL.
#' @param strings Strings (with optional partial matching) indicating which
#' parameters to be modified. This is an alternative to linenums.  Strings
#' correspond to the commented parameter names included in control.ss_new, or
#' whatever is written as comment at the end of the 14 number parameter lines.
#' Default=NULL.
#' @param newvals Vector of new parameter values. Default=NULL.
#' @param repeat.vals If multiple parameter lines match criteria, repeat the
#' \code{newvals} input for each line
#' @param estimate Vector of TRUE/FALSE for which changed parameters are to be
#' estimated. Default=FALSE.
#' @param verbose More detailed output to command line. Default=TRUE.
#' @author Ian Taylor
#' @seealso \code{\link{SS_parlines}}, \code{\link{SS_profile}}
#' @export
#' @keywords data manip
#' @examples
#' 
#' \dontrun{
#' SS_changepars(dir='Y:/ss/SSv3.03a/Simple/',ctlfile='Control.SS_New',
#'               strings=c('SR_steep','SR_sigmaR'),newvals=c(.35,.6))
#' # [1] wrote new file to Control_Modified.SS
#' #    oldvals newvals oldphase newphase     comment
#' # 1 0.609048    0.35        4       -4  # SR_steep
#' # 2 0.600000    0.60       -4       -4 # SR_sigmaR
#' }
#' 
SS_changepars <-
function(
         dir="C:/myfiles/mymodels/myrun/",
         ctlfile="control.ss_new",
         newctlfile="control_modified.ss",
         linenums=NULL, strings=NULL, newvals=NULL, repeat.vals=FALSE,
         estimate=FALSE, verbose=TRUE
         )
{

  # read control file
  fullctlfile <- paste(dir,ctlfile,sep="/")
  ctl = readLines(fullctlfile)

  # check for valid input
  if(is.null(linenums) & !is.null(strings) & class(strings)=="character")
  {
    # get table of parameter lines
    ctltable <- SS_parlines(ctlfile=fullctlfile)
    # list of all parameter labels
    allnames <- ctltable$Label
    # empty list of "good" labels to be added to 
    goodnames <- NULL
    # if strings are provided, look for matching subset of labels
    if(!is.null(strings)){
      # loop over vector of strings to add to goodnames vector
      for(i in 1:length(strings)){
        # fixed matching on string
        goodnames <- c(goodnames, allnames[grep(strings[i], allnames, fixed=TRUE)])
      }
      # remove duplicates and print some feedback
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
  if(!is.null(newvals) & length(newvals)!=nvals){
    if(repeat.vals){
      newvals <- rep(newvals, nvals)
    }else{
      stop("'newvals' and either 'linenums' or 'strings' should have the same number of elements")
    }
  }     
  if(!(length(estimate) %in% c(1,nvals)))
    stop("'estimate' should have 1 element or same number as 'newvals'")
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

