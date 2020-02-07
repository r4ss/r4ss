#' Change parameters, bounds, or phases in the control file.
#'
#' Loops over a subset of control file to change parameter lines.
#' Current initial value, lower and upper bounds, and phase can be modified,
#' but function could be expanded to control other columns.
#' Depends on \code{\link{SS_parlines}}.
#' Used by \code{\link{SS_profile}} and the \pkg{ss3sim} package.
#'
#'
#' @param dir Directory with control file to change.
#' @param ctlfile Control file name. Default="control.ss_new".
#' @param newctlfile Name of new control file to be written.
#'   Default="control_modified.ss".
#' @param linenums Line numbers of control file to be modified. Either this or
#'   the \code{strings} argument are needed. Default=NULL.
#' @param strings Strings (with optional partial matching) indicating which
#'   parameters to be modified. This is an alternative to \code{linenums}.
#'   \code{strings} correspond to the commented parameter names included in
#'   \code{control.ss_new}, or whatever is written as comment at the end
#'   of the 14 number parameter lines. Default=NULL.
#' @param newvals Vector of new parameter values. Default=NULL.
#'   The vector can contain \code{NA} values, which will assign the original
#'   value to the given parameter but change the remainder parameters, where
#'   the vector of values needs to be in the same order as either
#'   \code{linenums} or \code{strings}.
#' @param repeat.vals If multiple parameter lines match criteria, repeat the
#'   \code{newvals} input for each line.
#' @param estimate Optional vector or single value of TRUE/FALSE for which
#'   parameters are to be estimated. Changes sign of phase to be positive or
#'   negative. Default \code{NULL} causes no change to phase.
#' @param newlos Vector of new lower bounds. Default=NULL.
#'   The vector can contain \code{NA} values, which will assign the original
#'   value to the given parameter but change the remainder parameters, where
#'   the vector of values needs to be in the same order as either
#'   \code{linenums} or \code{strings}.
#' @param newhis Vector of new high bounds. Must be the same length as newhis
#'   Default=NULL.
#'   The vector can contain \code{NA} values, which will assign the original
#'   value to the given parameter but change the remainder parameters, where
#'   the vector of values needs to be in the same order as either
#'   \code{linenums} or \code{strings}.
#' @param newprior Vector of new prior values. 
#'   Default=NULL.
#'   The vector can contain \code{NA} values, which will assign the original
#'   value to the given parameter but change the remainder parameters, where
#'   the vector of values needs to be in the same order as either
#'   \code{linenums} or \code{strings}.
#' @param newprsd Vector of new prior sd values. 
#'   Default=NULL.
#'   The vector can contain \code{NA} values, which will assign the original
#'   value to the given parameter but change the remainder parameters, where
#'   the vector of values needs to be in the same order as either
#'   \code{linenums} or \code{strings}.
#' @param newprtype Vector of new prior type. 
#'   Default=NULL.
#'   The vector can contain \code{NA} values, which will assign the original
#'   value to the given parameter but change the remainder parameters, where
#'   the vector of values needs to be in the same order as either
#'   \code{linenums} or \code{strings}.
#' @param newphs Vector of new phases. Can be a single value, which will be
#'   repeated for each parameter, the same length as newvals, where each
#'   value corresponds to a single parameter, or \code{NULL}, where the
#'   phases will not be changed. If one wants to strictly turn parameters
#'   on or off and not change the phase in which they are estimated use
#'   \code{estimate = TRUE} or \code{estimate = FALSE}, respectively.
#'   The vector can contain \code{NA} values, which will assign the original
#'   value to the given parameter but change the remaining parameters, where
#'   the vector of values needs to be in the same order as either
#'   \code{linenums} or \code{strings}.
#' @param verbose More detailed output to command line. Default=TRUE.
#' @author Ian Taylor, Christine Stawitz, Chantel Wetzel
#' @seealso \code{\link{SS_parlines}}, \code{\link{SS_profile}}
#' @export
#' @examples
#'
#' \dontrun{
#' SS_changepars(dir='C:/ss/SSv3.30.03.05_May11/Simple - Copy',
#'               strings=c("steep","sigmaR"), newvals=c(.4,.6))
#' ## parameter names in control file matching input vector 'strings' (n=2):
#' ## [1] "SR_BH_steep" "SR_sigmaR"  
#' ## These are the ctl file lines as they currently exist:
#' ##     LO HI     INIT PRIOR PR_type SD PHASE env_var&link dev_link dev_minyr dev_maxyr
#' ## 95 0.2  1 0.613717   0.7    0.05  1     4       0       0         0         0
#' ## 96 0.0  2 0.600000   0.8    0.80  0    -4       0       0         0         0
#' ##        dev_PH Block Block_Fxn       Label Linenum
#' ## 95          0     0         0 SR_BH_steep      95
#' ## 96          0     0         0   SR_sigmaR      96
#' ## line numbers in control file (n=2):
#' ## [1] 95 96
#' ## 
#' ## wrote new file to control_modified.ss with the following changes:
#' ##    oldvals newvals oldphase newphase oldlos newlos oldhis newhis       comment
#' ## 1 0.613717     0.4        4       -4    0.2    0.2      1      1 # SR_BH_steep
#' ## 2 0.600000     0.6       -4       -4    0.0    0.0      2      2   # SR_sigmaR
#' }
SS_changepars <-
function(
         dir=NULL,
         ctlfile="control.ss_new",
         newctlfile="control_modified.ss",
         linenums=NULL, strings=NULL, newvals=NULL, repeat.vals=FALSE,
         newlos=NULL, newhis=NULL, newprior=NULL, newprsd=NULL, newprtype=NULL,
         estimate=NULL, verbose=TRUE,
         newphs = NULL
         )
{
  # set directory to working directory if not provided
  if (is.null(dir)){
    dir <- getwd()
  }
  # read control file
  fullctlfile <- file.path(dir, ctlfile)
  ctl <- readLines(fullctlfile)

# check for valid input
  inargs <- list("newvals" = newvals, "newlos" = newlos, "newhis" = newhis, 
    "newprior" = newprior, "newprsd" = newprsd, "newprtype" = newprtype, 
    "estimate" = estimate, "newphs" = newphs)
  if(is.null(linenums) & !is.null(strings) & class(strings)=="character")
  {
    # get table of parameter lines
    ctltable <- SS_parlines(ctlfile=fullctlfile)
    
    # list of all parameter labels
    allnames <- ctltable$Label
    # empty list of "good" labels to be added to
    goodnames <- list()
    # if strings are provided, look for matching subset of labels
    if(!is.null(strings)){
      # loop over vector of strings to add to goodnames vector
      for(i in 1:length(strings)){
        # fixed matching on string
        goodnames[[i]] <- allnames[grep(strings[i], allnames, fixed=TRUE)]
      }
      # remove duplicates and print some feedback
      if (any(duplicated(unlist(goodnames))) & 
        (repeat.vals & any(sapply(inargs, length) > 1))) {
        stop("Entries in 'strings' did not map to unique parameters and\n",
          "it is unclear how to order the par names to match the order\n",
          "of other arguments provided to SS_changepars.\n",
          "E.g., strings = c('CV', 'Mal') each return 'CV_young_Mal_GP_1'\n",
          "and should be changed to strings = c('young_Fem', 'old_Fem', 'Mal')\n",
          "to get all CV and all Male parameters.")
      }
      goodnames <- unique(unlist(goodnames))
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
    cat(paste(linenums, collapse = ", "))
  }
  # define objects to store changes
  newctlsubset <- NULL
  cmntvec <- NULL
  nvals <- length(linenums)
  # make vectors of NA values for old and new quantities
  oldvals <- oldlos <- oldhis <- oldphase <- rep(NA, nvals)
  oldprior <- oldprsd <- oldprtype <- newphase <- rep(NA, nvals)
  # check all inputs
  # check values and make repeat if requested
  for (ii in names(inargs)) {
    tmp <- get(ii)
    if (is.null(tmp)) next
    if (is.data.frame(tmp) & ii!="estimate") tmp <- as.numeric(tmp)
    if (length(tmp)!=nvals & repeat.vals) {
      if (length(tmp) > 1) stop("SS_changepars doesn't yet accommodate ",
        "repeat.vals=TRUE and of length(.) > 1")
      assign(ii, rep(tmp, nvals))
    }
    if (length(get(ii))!=nvals) {
      stop(paste0("'", ii, "'"), " and either 'linenums' or 'strings'",
        " should have the same number of elements,\n",
        "instead of ", length(get(ii)), " and ", length(linenums), ".\n",
        "Note: a string can map to multiple parameters, here are your pars,\n",
        paste(goodnames, collapse = "\n"))
    }
  }

  navar <- c(NA, "NA", "NAN", "Nan")

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
      stop("There's a problem with a non-numeric value in line ",linenums[i])
    }
    # store information on old value and replace with new value (unless NULL)
    oldvals[i] <- vec[3]
    if(!is.null(newvals)){
      if (newvals[i] %in% navar) {
        newvals[i] <- vec[3]
      }
      vec[3] <- newvals[i]
    }
    # store information on old bounds and replace with new bounds (unless NULL)
    oldlos[i] <- vec[1]
    oldhis[i] <- vec[2]
    if(!is.null(newlos)){
      if (newlos[i] %in% navar) {
        newlos[i] <- vec[1]
      }
      vec[1] <- newlos[i]
    }
    if (!is.null(newhis)){
      if (newhis[i] %in% navar) {
        newhis[i] <- vec[2]
      }
      vec[2] <- newhis[i]
    }
    oldprior[i] <- vec[4]
    oldprsd[i]  <- vec[5]
    oldprtype[i]<- vec[6]
    if (!is.null(newprior)){
      if (newprior[i] %in% navar) {
        newprior[i] <- vec[4]
      }
      vec[4] <- newprior[i]
    }
    if (!is.null(newprsd)){
      if (newprsd[i] %in% navar) {
        newprsd[i] <- vec[5]
      }
      vec[5] <- newprsd[i]
    }
    if (!is.null(newprtype)){
      if (newprtype[i] %in% navar) {
        newprtype[i] <- vec[6]
      }
      vec[6] <- newprtype[i]
    }

    # change phase (unless NULL)
    oldphase[i] <- as.numeric(vec[7])
    if (!is.null(newphs)) {
      if (newphs[i] %in% navar) {
        newphs[i] <- vec[7]
      }
      vec[7] <- newphs[i]
    }
    if (!is.null(estimate)){
      if (estimate[i]){
        vec[7] <- abs(as.numeric(vec[7]))
      }else{
        vec[7] <- -abs(as.numeric(vec[7]))
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
  writeLines(newctl, file.path(dir, newctlfile))
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
  if (is.null(newprior)){
    newprior <- oldprior
  }
  if (is.null(newprsd)){
    newprsd <- oldprsd
  }
  if (is.null(newprtype)){
    newprtype <- oldprtype
  }
  results <- data.frame(oldvals, newvals, oldphase, newphase,
                        oldlos, newlos, oldhis, newhis, 
                        oldprior, newprior, oldprsd, newprsd, 
                        oldprtype, newprtype, comment=cmntvec)
  # output table of changes
  if (is.null(newvals)) {
    newvals <- NA
  }
  if (verbose) {
    print(results)
  }
  return(invisible(results))

} # end function
