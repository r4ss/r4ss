#' Change parameters, bounds, or phases in the control file.
#'
#' Loops over a subset of control file to change parameter lines.
#' Current initial value, lower and upper bounds, and sign of phase can be modified,
#' but function could be expanded to control other columns.
#' Depends on \code{\link{SS_parlines}}.
#' Used by \code{\link{SS_profile}} and the \code{ss3sim} package.
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
#' estimated. Default=FALSE. Can also be NULL.
#' @param newlos Vector of new lo bounds. Default=NULL.
#' @param newhis Vector of new hi bounds. Must be the same length as newhis
#'   Default=NULL.
#' @param verbose More detailed output to command line. Default=TRUE.
#' @author Ian Taylor, Christine Stawitz
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
         newlos=NULL, newhis=NULL, estimate=FALSE, verbose=TRUE
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
      if(verbose){
        cat("parameter names in control file matching input vector 'strings' (n=",
            length(goodnames),"):\n",sep="")
        print(goodnames)
      }
      if(length(goodnames)==0){
        stop("No parameters names match input vector 'strings'")
      }
    }
    nvals <- length(goodnames)
    if(verbose){
      cat('These are the ctl file lines as they currently exist:\n')
      print(ctltable[ctltable$Label %in% goodnames,])
    }
    for(i in 1:nvals){
      linenums[i] <- ctltable$Linenum[ctltable$Label==goodnames[i]]
    }
  }else{
    if(is.null(linenums)){
      stop("valid input needed for either 'linenums' or 'strings'")
    }
  }
  ctlsubset <- ctl[linenums]
  if(verbose){
    cat("line numbers in control file (n=",length(linenums),"):\n",sep="")
    print(linenums)
  }
  # define objects to store changes
  newctlsubset <- NULL
  cmntvec <- NULL
  nvals <- length(linenums)
  oldvals <- oldlos <- oldhis <- oldphase <- newphase <- rep(NA, nvals)

  # check all inputs
  # check values and make repeate if requested
  if (!is.null(newvals) & length(newvals)!=nvals){
    if (repeat.vals){
      newvals <- rep(newvals, nvals)
    }else{
      stop("'newvals' and either 'linenums' or 'strings' should have",
           "the same number of elements")
    }
  }
  # check bounds
  # lower and upper bounds don't yet have option for repeat.vals=TRUE
  if (!is.null(newlos) & length(newlos) != nvals) {
    stop("'newlos' and either 'linenums' or 'strings' should have",
         "the same number of elements")
  }
  if (!is.null(newhis) & length(newhis) != nvals) {
    stop("'newhis' and either 'linenums' or 'strings' should have",
         "the same number of elements")
  }
  if (is.data.frame(newlos)){
    newlos <- as.numeric(newlos)
  }
  if (is.data.frame(newhis)){
    newhis <- as.numeric(newhis)
  }
  if (!is.null(estimate)){
    if (!(length(estimate) %in% c(1,nvals))){
      stop("'estimate' should have 1 element or same number as 'newvals'")
    }
    if (length(estimate)==1){
      estimate <- rep(estimate, nvals)
    }
  }
  if (is.data.frame(newvals)){
    newvals <- as.numeric(newvals)
  }
  #### if inputs are NULL, allow newlows and newhis to be replaced by old values
  ## if (is.null(newlos)){
  ##   stop("Nothing input for 'newlos'")
  ## }
  ## if (is.null(newhis)){
  ##   stop("Nothing input for 'newhis'")
  ## }
  ## if(is.null(newvals)){
  ##   stop("Nothing input for 'newvals'")
  ## }

  # loop over line numbers to replace parameter values
  for(i in 1:nvals)
  {
    # parse comment at end of line
    splitline <- strsplit(ctlsubset[i], "#")[[1]]
    #
    cmnt <- paste("#",paste(splitline[-1],collapse="#"),sep='')
    cmntvec <- c(cmntvec, cmnt)
    # split line and convert to numeric
    vecstrings <- strsplit(splitline[1],split="[[:blank:]]+")[[1]]
    vec <- as.numeric(vecstrings[vecstrings!=""])
    if(max(is.na(vec))==1){
      stop("There's a problem with a non-numeric value in line",linenums[i])
    }
    # store information on old value and replace with new value (unless NULL)
    oldvals[i] <- vec[3]
    if(!is.null(newvals)){
      vec[3] <- newvals[i]
    }
    # store information on old bounds and replace with new bounds (unless NULL)
    oldlos[i] <- vec[1]
    oldhis[i] <- vec[2]
    if(!is.null(newlos)){
      vec[1] <- newlos[i]
    }
    if (!is.null(newhis)){
      vec[2] <- newhis[i]
    }
    
    # change phase (unless NULL)
    oldphase[i] <- as.numeric(vec[7])
    if (!is.null(estimate)){
      if (estimate[i]){
        vec[7] <- abs(oldphase[i])
      }else{
        vec[7] <- -abs(oldphase[i])
      }
    }
    # check bounds relative to new values
    if(vec[3] < vec[1]){
      warning("value ",vec[3]," is now below lower bound ",vec[1]," for ",cmnt,"\n")
    }
    if(vec[3] > vec[2]){
      warning("value ",vec[3]," is now above upper bound ",vec[2]," for ",cmnt,"\n")
    }

    newphase[i] <- vec[7]
    newline <- paste("",paste(vec, collapse=" "), cmnt)
    newctlsubset <- rbind(newctlsubset, newline)
  }
  # write new file
  newctl <- ctl
  newctl[linenums] <- newctlsubset
  writeLines(newctl, paste(dir,newctlfile,sep="/"))
  if(verbose){
    cat('\nwrote new file to',newctlfile,'with the following changes:\n')
  }
  # if no changed made, repeat old values in output
  if (is.null(newvals)){
    newvals <- oldvals
  }
  if (is.null(newlos)){
    newlos <- oldlos
  }
  if (is.null(newhis)){
    newhis <- oldhis
  }
  results <- data.frame(oldvals, newvals, oldphase, newphase,
                        oldlos, newlos, oldhis, newhis, comment=cmntvec)
  # output table of changes
  if (is.null(newvals)) {
    newvals <- NA
  }
  if (verbose) {
    print(results)
  }
  return(invisible(results))

} # end function

