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
