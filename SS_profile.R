SS_profile <-
function(
         dir="C:/myfiles/mymodels/myrun/",
         masterctlfile="control.ss_new",
         newctlfile="control_modified.ss", # must match entry in starter file
         linenum=NULL, string=NULL, profilevec=NULL,
         usepar=FALSE, globalpar=FALSE, parfile=NULL,
         parlinenum=NULL, parstring=NULL,
         dircopy=TRUE, exe.delete=FALSE,
         model='ss3',extras="-nox",systemcmd=FALSE,saveoutput=TRUE,
         overwrite=TRUE,
         verbose=TRUE)
{
  ################################################################################
  #
  # SS_profile
  #
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

  # figure out name of executable based on 'model' input which may contain .exe
  if(length(grep(".exe",tolower(model)))){
    exe <- model
  }else{
    exe <- tolower(paste(model,ifelse(OS=="Windows",".exe",""),sep=""))
  }
  # check whether exe is in directory
  if(!exe %in% tolower(dir(dir))) stop("Executable ",exe," not found in ",dir)

  if(length(linenum)+length(string)!=1)
    stop("one value should be input for either 'linenum' or 'string', but not both")
  if(usepar & length(parlinenum)+length(parstring)!=1)
    stop("one value should be input for either 'parlinenum' or 'parstring', but not both")
  n <- length(profilevec)
  if(n==0) stop("Missing input 'profilevec'")
  converged <- rep(NA,n)
  totallike <- rep(NA,n)
  liketable <- NULL

  setwd(dir) # change working directory
  stdfile <- paste(model,'.std',sep='')

  # read starter file to get input file names and check various things
  starter.file <- dir()[tolower(dir())=='starter.ss']
  if(length(starter.file)==0) stop("starter.ss not found in",dir)
  starter <- SS_readstarter(starter.file)
  # check for new control file
  if(starter$ctlfile!=newctlfile){
    stop("starter file should be changed to change\n",
         "'",starter$ctlfile,"' to '",newctlfile,"'")
  }
  # check for prior in likelihood
  if(starter$prior_like==0){
    stop("for likelihood profile, you should change the starter file value of\n",
         " 'Include prior likelihood for non-estimated parameters'\n",
         " from 0 to 1 and re-run the estimation.\n")
  }
  # check for consistency in use of par file
  if(usepar & starter$init_values_src==0){
    stop("with setting 'usepar=TRUE', you need to change the starter file value\n",
         " for initial value source from 0 (ctl file) to 1 (par file).\n")
  }

  if(is.null(parfile)) parfile <- paste(model,'.par',sep='')
  if(usepar) file.copy(parfile, "parfile_original_backup.sso")

  # run loop over profile values
  for(i in 1:n){
    # change initial values in the control file
    # this also sets phase negative which is needed even when par file is used
    SS_changepars(dir=dir,ctlfile=masterctlfile,newctlfile=newctlfile,
                  linenums=linenum,strings=string,
                  newvals=profilevec[i], estimate=FALSE,
                  verbose=TRUE, repeat_vals=TRUE)
    if(usepar){
      # alternatively change initial values in the par file
      # read file
      if(globalpar){
        par <- readLines("parfile_original_backup.sso")
      }else{
        par <- readLines(parfile)
      }
      # find value
      if(!is.null(parstring)) parlinenum <- grep(parstring,par,fixed=TRUE)+1
      if(length(parlinenum)!=1) stop("Problem with input parstring = '",parstring,"'",sep="")
      parline <- par[parlinenum]
      parval <- as.numeric(parline)
      if(is.na(parval))
        stop("Problem with parlinenum or parstring for par file.\n",
             "line as read: ", parline)
      # replace value
      par[parlinenum] <- profilevec[i]
      # add new header
      note <- c(paste("# New par file created by SS_profile with the value on line number",linenum),
               paste("# changed from",parval,"to",profilevec[i]))
      par <- c(par,"#",note)
      print(note)
      # write new file
      writeLines(par, paste("ss3.par_input_",i,".ss",sep=""))
      writeLines(par, "ss3.par")
    }
    if(file.exists(stdfile)) file.remove(stdfile)
    if(file.exists('Report.sso')) file.remove('Report.sso')

    # run model
    command <- paste(model, extras)
    cat("Running model in directory:",getwd(),"\n")
    cat("Using the command: '",command,"'\n",sep="")
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
      file.copy(parfile,paste(model,'.par_',i,'.sso',sep=""),overwrite=overwrite)
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

