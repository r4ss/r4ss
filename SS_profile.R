SS_profile <-
function(
         dir="C:/myfiles/mymodels/myrun/",
         masterctlfile="control.ss_new",
         newctlfile="control_modified.ss", # must match entry in starter file
         linenum=NULL, string=NULL, profilevec=NULL, usepar=TRUE,
         dircopy=TRUE, exe.delete=FALSE,
         command="SS3 -nox",model='ss3',systemcmd=FALSE,saveoutput=TRUE,
         overwrite=FALSE,
         verbose=TRUE)
{
  ################################################################################
  #
  # SS_profile
  # July 5, 2011.
  # This function comes with no warranty or guarantee of accuracy
  #
  # Purpose: run a likelihood profile by iteratively modifying
  #          a Stock Synthesis control file
  # Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
  # Returns: Plots of prior distributions used in Stock Synthesis model
  # Notes:   requires SS_parlines and SS_changepars
  #          hosted at http://code.google.com/p/r4ss/
  # Required packages: none
  #
  ################################################################################

  OS <- "Mac" # don't know the version$os info for Mac
  if(length(grep("linux",version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw",version$os)) > 0) OS <- "Windows"

  if(length(linenum)+length(string)!=1)
    stop("one value should be input for either 'linenum' or 'string', but not both")
  n <- length(profilevec)
  converged <- rep(NA,n)
  totallike <- rep(NA,n)
  liketable <- NULL

  setwd(dir) # change working directory
  stdfile <- paste(model,'.std',sep='')

  # read starter file to get input file names and check for prior in likelihood
  starter.file <- dir()[tolower(dir())=='starter.ss']
  if(length(starter.file)==0) stop("starter.ss not found in",dir)
  starter <- SS_readstarter(starter.file)
  if(starter$prior_like==0){
    stop("for likelihood profile, you should change\n",
         " 'Include prior likelihood for non-estimated parameters'\n",
         " from 0 to 1 and re-run the estimation.\n")
  }
  

  # run loop over profile values
  for(i in 1:n){
    SS_changepars(dir=dir,ctlfile=masterctlfile,newctlfile=newctlfile,
                  linenums=linenum,strings=string,
                  newvals=profilevec[i], estimate=FALSE,
                  verbose=TRUE)
    if(file.exists(stdfile)) file.remove(stdfile)
    if(file.exists('Report.sso')) file.remove('Report.sso')

    # run model
    cat("Running model in directory:",getwd(),"\n")
    cat("Using the command:",command,"\n")
    if(OS=="Windows" & !systemcmd){
      shell(cmd=command)
    }else{
      system(command)
    }

    converged[i] <- file.exists(stdfile)
    onegood <- FALSE
    if(file.exists('Report.sso') & file.info('Report.sso')$size>0){
      onegood <- TRUE
      Rep <- readLines('Report.sso',n=120)
      like <- read.table('Report.sso',skip=grep('LIKELIHOOD',Rep)[2]+0,nrows=11,header=TRUE,fill=TRUE)
      liketable <- rbind(liketable,as.numeric(like$logL.Lambda))
    }else{
      liketable <- rbind(liketable,rep(NA,10))
    }

    if(saveoutput){
      file.copy('Report.sso',paste('Report',i,".sso",sep=""),overwrite=overwrite)
      file.copy('CompReport.sso',paste('CompReport',i,".sso",sep=""),overwrite=overwrite)
      file.copy('covar.sso',paste('covar',i,".sso",sep=""),overwrite=overwrite)
    }
  } # end loop
  if(onegood){
    liketable <- as.data.frame(liketable)
    names(liketable) <- like$Component
    bigtable <- cbind(profilevec,converged,liketable)
    names(bigtable)[1] <- 'Value'
    return(bigtable)
  }else{
    stop('Error: no good Report.sso files created in profile')
  }
} # end function

