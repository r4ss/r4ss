SSgetoutput <-
function(keyvec=NULL,dirvec=NULL,getcovar=TRUE,getcomp=TRUE,forecast=FALSE,
         verbose=TRUE,ncols=210,global=FALSE,replace=FALSE,listlists=TRUE)
{
  # a function to run the function SSoutput to create a list in the R workspace
  # for a Stock Synthesis model with output filenames ending with the same "key"

  ## if(!is.environment(env) && is.character(env)) assign(env,new.env(parent = baseenv()),env=.GlobalEnv)
  ## else print("input 'env' should be either an environment or a string",quote=FALSE)
  ## env <- ifelse(exists(env),env,get(env))
  ## if(is.environment(env)) print("assigning objects to environment:",env)

  # change inputs so that keyvec and dirvec have matching lengths or keyvec=NULL
  if(listlists) biglist <- list()
  n1 <- length(keyvec)
  n2 <- length(dirvec)
  if(n1>1 & n2>1 & n1!=n2){
    print("inputs 'keyvec' and 'dirvec' have unmatched lengths > 1",quote=FALSE)
  }else{
    n <- max(1, n1, n2) # n=1 or n=length of either optional input vector
  }
  if(n1==1) keyvec <- rep(keyvec,n)
  objectnames <- paste("replist_",keyvec,sep="")
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
      key2 <- key
    }else{
      key2 <- paste("_",key,sep="")
    }
    newobject <- objectnames[i]

    if(verbose) print(paste("getting files with key =",key),quote=FALSE)

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
    if(verbose) print(paste("reading output from ",fullfile,sep=""),quote=FALSE)
    repfilesize <- file.info(fullfile)$size

    if(!is.na(repfilesize) && repfilesize>0){ # if there's a non-empty file
      output <- SSoutput(dir=mydir, repfile=repfilename, covarfile=covarname,
                            compfile=compfilename, NoCompOK=NoCompOK, printstats=FALSE,
                            covar=mycovar, forecast=forecast, verbose=FALSE, ncols=ncols)
      if(is.null(output)){
        # for some reason covarfile exists, but is old so SSoutput rejects
        print("output==NULL so trying again with covar=FALSE",quote=FALSE)
        output <- SSoutput(dir=mydir, repfile=repfilename, covarfile=covarname,
                              compfile=compfilename, NoCompOK=NoCompOK, printstats=FALSE,
                              covar=FALSE, forecast=forecast, verbose=FALSE, ncols=ncols)
      }
      output$key <- as.character(key)
    }else{
      print("!repfile doesn't exists or is empty")
      return()
    }
    print(paste("added element '", newobject, "' to list",sep=""),quote=FALSE)
    if(listlists) biglist[[newobject]] <- output
    if(global)
    {
      if(exists(newobject) && !is.null(get(newobject)) & !replace)
      {
        print(paste("exists and not replacing:",newobject),quote=FALSE)
      }else{
        assign(newobject,output,pos=1)
        print(paste("created new object:",newobject),quote=FALSE)
      }
    }
  }
  return(invisible(biglist))
}
