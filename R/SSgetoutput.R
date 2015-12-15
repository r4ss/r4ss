#' Get output from multiple Stock Synthesis models.
#' 
#' Apply the function \code{\link{SS_output}} multiple times and save output as
#' individual objects or a list of lists.
#' 
#' 
#' @param keyvec A vector of strings that are appended to the output files from
#' each model if models are all in one directory. Default=NULL.
#' @param dirvec A vector of directories (full path or relative to working
#' directory) in which model output is located. Default=NULL.
#' @param getcovar Choice to read or not read covar.sso output (saves time and
#' memory). Default=TRUE.
#' @param getcomp Choice to read or not read CompReport.sso output (saves time
#' and memory). Default=TRUE.
#' @param forecast Choice to read or not read forecast quantities.
#' Default=FALSE.
#' @param verbose Print various messages to the command line as the function
#' runs? Default=TRUE.
#' @param ncols Maximum number of columns in Report.sso (same input as for
#' \code{\link{SS_output}}).  Default=210.
#' @param listlists Save output from each model as a element of a list (i.e.
#' make a list of lists). Default = TRUE.
#' @param underscore Add an underscore '_' between any file names and any keys
#' in keyvec. Default=FALSE.
#' @param save.lists Save each list of parsed output as a .Rdata file (with default
#' filenaming convention based on iteration and date stamp.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_output}} \code{\link{SSsummarize}}
#' @keywords data manip list
SSgetoutput <-
function(keyvec=NULL,dirvec=NULL,getcovar=TRUE,getcomp=TRUE,forecast=FALSE,
         verbose=TRUE,ncols=210,listlists=TRUE,underscore=FALSE,
         save.lists=FALSE)
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

    # mycovar = TRUE/FALSE based on presence of file and user input
    mycovar <- file.exists(file.path(mydir,covarname)) & getcovar
    
    fullfile <- paste(mydir,repfilename,sep="")
    if(verbose) cat("reading output from",fullfile,"\n")
    repfilesize <- file.info(fullfile)$size

    output <- NA
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

    if(save.lists){
      biglist.file <- paste("biglist",i,"_",format(Sys.time(),'%d-%b-%Y_%H.%M' ),".Rdata",sep="")
      save(biglist, file=biglist.file)
    }
  }
  return(invisible(biglist))
}
