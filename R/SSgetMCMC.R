#' Read MCMC output.
#' 
#' Reads the MCMC output (in the posteriors.sso and derived_posteriors.sso
#' files) from one or more models.
#' 
#' 
#' @param dir A string (or vector of strings) of the directory (or directories)
#' with MCMC output.
#' @param verbose TRUE/FALSE switch to get more or less information about the
#' progress of the function.
#' @param writecsv Write key parameters and certainty nuisance quantities to a
#' CSV file.
#' @param csv1 First CSV file for key parameters.
#' @param csv2 Second CSV file for nuisance quantities.
#' @param keystrings Vector of strings that partially match parameter names to
#' write to the file csv1. This file intended to feed into
#' \code{\link{mcmc.out}}.
#' @param nuisancestrings Vector of strings that partially match derived
#' quantity names to write to the file csv2. This file intended to feed into
#' \code{\link{mcmc.nuisance}}.
#' @param modelnames Either "default" or a vector of names to use in naming
#' elements of list that is output by the function. Default is "model1",
#' "model2", etc.
#' @param burnin Optional burn-in value to apply on top of the option in the
#' starter file.
#' @param thin Optional thinning value to apply on top of the option in the
#' starter file and in the \code{-mcsave} runtime command.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{mcmc.out}}, \code{\link{mcmc.nuisance}},
#' \code{\link{SSplotPars}}
#' @keywords data
SSgetMCMC <-
function(dir=NULL,verbose=TRUE, writecsv=FALSE,
         csv1="keyposteriors.csv",
         csv2="nuisanceposteriors.csv",
         keystrings=c(       # values that get written to csv1
           "NatM",
           "R0",
           "steep",
           "RecrDev_2008",
           "Q_extraSD"),
         nuisancestrings=c(  # values that get written to csv2
           "Objective_function",
           "SPB_",
           "InitAge",
           "RecrDev"),
         modelnames="default",
         burnin = 0,            #the number of values to discard for burnin
         thin = 1               #the thinning interval
         )

{
  # a function to get MCMC output for 1 or more models
  # dir: a string or vector of strings pointing to directories with MCMC output
  # verbose: more information in the console as the model runs

  postname <- "posteriors.sso"
  derpostname <- "derived_posteriors.sso"
  
  n <- length(dir)

  postlist <- list()
  # loop over directories
  for(imodel in 1:n)
  {
    # get MCMC output
    if(verbose) cat("getting files from",dir[imodel],"\n")
    post <- read.table(paste(dir[imodel],postname,sep="/"),header=TRUE)
    names(post)[names(post)=="SR_LN.R0."] <- "SR_LN(R0)"

    derpost <- read.table(paste(dir[imodel],derpostname,sep="/"),header=TRUE)
    # remove redundant values
    derpost <- derpost[,!(names(derpost) %in% c("Iter","Objective_function"))]

    # combine two dataframes
    allpost <- cbind(post,derpost)

    #apply burnin and thinning
    allpost <- allpost[seq((burnin+1),nrow(allpost),thin),]

    # make list of all dataframes
    postlist[[imodel]] <- allpost

    keylabels <- NULL
    nuisancelabels <- NULL
    for(istring in 1:length(keystrings))
      keylabels <- c(keylabels,names(allpost)[grep(keystrings[istring],names(allpost))])
    for(istring in 1:length(nuisancestrings))
      nuisancelabels <- c(nuisancelabels,names(allpost)[grep(nuisancestrings[istring],names(allpost))])
    keypost <- allpost[,names(allpost) %in% keylabels]
    nuisancepost <- allpost[,names(allpost) %in% nuisancelabels]

    if(writecsv){
      file1 <- paste(dir[imodel],csv1,sep="/")
      file2 <- paste(dir[imodel],csv2,sep="/")
      if(verbose) cat("writing subset of posteriors to files:\n  ",
                      file1,"\n  ",file2,"\n")
      write.csv(keypost,file1,row.names=FALSE)
      write.csv(nuisancepost,file2,row.names=FALSE)
    }
  }
  if(modelnames[1]=="default") names(postlist) <- paste("model",1:n,sep="")
  else names(postlist) <- modelnames
  
  return(invisible(postlist))
}
