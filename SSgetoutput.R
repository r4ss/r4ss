SSgetoutput <-
function(keyvec=NULL,dirvec=NULL,getcovar=TRUE,getcomp=TRUE,forecast=FALSE,
         verbose=TRUE,ncols=210,listlists=TRUE,underscore=FALSE)
{
  # a function to run the function SS_output to create a list in the R workspace
  # for a Stock Synthesis model with output filenames ending with the same "key"

  if(!is.null(keyvec)) cat('length(keyvec) as input to SSgetoutput:',length(keyvec),'\n')
  if(!is.null(dirvec)) cat('length(dirvec) as input to SSgetoutput:',length(dirvec),'\n')
 
  # change inputs so that keyvec and dirvec have matching lengths or keyvec=NULL
  if(listlists) biglist <- list()
  n1 <- length(keyvec)
  n2 <- length(dirvec)
  if(n1>1 & n2>1 & n1!=n2){
    cat("inputs 'keyvec' and 'dirvec' have unmatched lengths > 1\n")
  }else{
    n <- max(1, n1, n2) # n=1 or n=length of either optional input vector
  }
  if(n1==1) keyvec <- rep(keyvec,n)
  objectnames <- paste("replist",keyvec,sep="")
  if(n1==0) objectnames <- paste("replist",1:n,sep="")

  if(n2==0) dirvec <- getwd()
  if(length(dirvec)==1) dirvec <- rep(dirvec,n)
  dirvec <- paste(dirvec,"/",sep="")

  # loop over directories or key strings
  for(i in 1:n)
  {
    key <- keyvec[i]
    mydir <- dirvec[i]
    if(is.null(key)){
      key2 <- NULL
    }else{
      key2 <- ifelse(underscore,paste("_",key,sep=""),key)
    }
    newobject <- objectnames[i]

    if(verbose & !is.null(key)) cat("getting files with key =",key,"\n")

    repfilename <- paste("Report",key2,".sso",sep="")
    covarname <- paste("covar",key2,".sso",sep="")
    if(getcomp){
      compfilename <- paste("CompReport",key2,".sso",sep="")
      NoCompOK <- FALSE
    }else{
      compfilename <- "nothing"
      NoCompOK <- TRUE
    }

    if(file.exists(paste(mydir,covarname,sep="")) & getcovar) mycovar=TRUE else mycovar=FALSE
    fullfile <- paste(mydir,repfilename,sep="")
    if(verbose) cat("reading output from",fullfile,"\n")
    repfilesize <- file.info(fullfile)$size

    if(!is.na(repfilesize) && repfilesize>0){ # if there's a non-empty file
      output <- SS_output(dir=mydir, repfile=repfilename, covarfile=covarname,
                            compfile=compfilename, NoCompOK=NoCompOK, printstats=FALSE,
                            covar=mycovar, forecast=forecast, verbose=FALSE, ncols=ncols)
      if(is.null(output)){
        # for some reason covarfile exists, but is old so SS_output rejects
        cat("output==NULL so trying again with covar=FALSE\n")
        output <- SS_output(dir=mydir, repfile=repfilename, covarfile=covarname,
                              compfile=compfilename, NoCompOK=NoCompOK, printstats=FALSE,
                              covar=FALSE, forecast=forecast, verbose=FALSE, ncols=ncols)
      }
      output$key <- as.character(key)
    }else{
      cat("!repfile doesn't exists or is empty\n")
    }
    cat("added element '", newobject, "' to list\n",sep="")
    if(listlists) biglist[[newobject]] <- output
    ## if(global)
    ## {
    ##   if(exists(newobject) && !is.null(get(newobject)) & !replace)
    ##   {
    ##     cat("exists and not replacing:",newobject,"\n")
    ##   }else{
    ##     assign(newobject,output,pos=1)
    ##     cat("created new object:",newobject,"\n")
    ##   }
    ## }
  }
  return(invisible(biglist))
}
