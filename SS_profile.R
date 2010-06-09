SS_profile <-
function(
         dir="C:/myfiles/mymodels/myrun/",
         masterctlfile="control.ss_new",
         newctlfile="control_modified.ss", # must match entry in starter file
         linenum=NULL, string=NULL, profilevec=NULL,
         command="SS3 -nox",model='ss3',systemcmd=F,saveoutput=T,
         verbose=T)
{
  ################################################################################
  #
  # SS_profile
  # October 5, 2009.
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

  if(.Platform$OS.type=="windows") win <- T  else  win <- F

  if(length(linenum)+length(string)!=1){
    print("one value should be input for either 'linenum' or 'string', but not both",quote=F)
    return()
  }
  n <- length(profilevec)
  converged <- rep(NA,n)
  totallike <- rep(NA,n)
  liketable <- NULL

  setwd(dir) # change working directory
  stdfile <- paste(model,'.std',sep='')

  # run loop over profile values
  for(i in 1:n){
    SS_changepars(dir=dir,ctlfile=masterctlfile,newctlfile=newctlfile,
                  linenums=linenum,string=string,
                  newvals=profilevec[i], estimate=F,
                  verbose=T)
    if(file.exists(stdfile)) file.remove(stdfile)
    if(file.exists('Report.sso')) file.remove('Report.sso')

    # run model
    if(win & !systemcmd){
      shell(cmd=command)
    }else{
      system(command)
    }

    converged[i] <- file.exists(stdfile)
    onegood <- F
    if(file.exists('Report.sso') & file.info('Report.sso')$size>0){
      onegood <- T
      Rep <- readLines('Report.sso',n=120)
      like <- read.table('Report.sso',skip=grep('LIKELIHOOD',Rep)[2]+0,nrows=10,head=T,fill=T)
      liketable <- rbind(liketable,as.numeric(like$logL.Lambda))
    }else{
      liketable <- rbind(liketable,rep(NA,10))
    }

    if(saveoutput){
      file.copy('Report.sso',paste('Report',i,".sso",sep=""))
      file.copy('CompReport.sso',paste('CompReport',i,".sso",sep=""))
      file.copy('covar.sso',paste('covar',i,".sso",sep=""))
    }
  } # end loop
  if(onegood){
    liketable <- as.data.frame(liketable)
    names(liketable) <- like$Component
    bigtable <- cbind(profilevec,converged,liketable)
    names(bigtable)[1] <- 'Value'
    return(bigtable)
  }else{
    print('Error: no good Report.sso files created in profile',quote=F)
    return()
  }
} # end function

