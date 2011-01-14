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
         modelnames="default"
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
    derpost <- read.table(paste(dir[imodel],derpostname,sep="/"),header=TRUE)
    # remove redundant values
    derpost <- derpost[,-(names(derpost) %in% c("Iter","Objective_function"))]

    # combine two dataframes
    allpost <- cbind(post,derpost)
    # make list of all dataframes
    postlist[[imodel]] <- allpost

    keylabels <- NULL
    nuisancelabels <- NULL
    for(istring in 1:length(keystrings))
      keylabels <- c(keylabels,names(allpost)[grep(keystrings[istring],names(allpost))])
      nuisancelabels <- c(nuisancelabels,names(allpost)[grep(nuisancestrings[istring],names(allpost))])
    keypost <- allpost[names(allpost) %in% keylabels,]
    nuisancepost <- allpost[names(allpost) %in% nuisancelabels,]

    if(writecsv){
      write.csv(keypost,paste(dir[imodel],csv1,sep="/"),row.names=FALSE)
      write.csv(nuisancepost,paste(dir[imodel],csv2,sep="/"),row.names=FALSE)
    }
  }
  if(modelnames[1]=="default") names(postlist) <- paste("model",1:n,sep="")
  else names(postlist) <- modelnames
  
  return(invisible(postlist))
}
