#' Run a likelihood profile in Stock Synthesis.
#' 
#' Iteratively changes the control file using SS_changepars.
#' 
#' 
#' @param dir Directory where input files and executable are located.
#' @param masterctlfile Source control file. Default = "control.ss_new"
#' @param newctlfile Destination for new control files (must match entry in
#' starter file). Default = "control_modified.ss".
#' @param linenum Line number of parameter to be changed.  Can be used instead
#' of \code{string} or left as NULL.
#' @param string String partially matching name of parameter to be changed. Can
#' be used instead of \code{linenum} or left as NULL.
#' @param usepar Use PAR file from previous profile step for starting values?
#' @param globalpar Use global par file for all runs instead of the par file
#' from each successive run
#' @param parfile Name of par file to use (Ian says "I don't remember how this
#' interacts with the globalpar input")
#' @param parlinenum Line number in par file to change.
#' @param parstring String in par file preceding line number to change.
#' @param dircopy Copy directories for each run? NOT IMPLEMENTED YET.
#' @param exe.delete Delete exe files in each directory?  NOT IMPLEMENTED YET.
#' @param profilevec Vector of values to profile over.  Default = NULL.
#' @param model Name of executable. Default = "ss3".
#' @param extras Additional commands to use when running SS. Default = "-nox"
#' will reduce the amound of command-line output.
#' @param systemcmd Should R call SS using "system" function intead of "shell".
#' This may be required when running R in Emacs. Default = FALSE.
#' @param saveoutput Copy output .SSO files to unique names.  Default = TRUE.
#' @param overwrite Overwrite any existing .SSO files. Default = TRUE. If FALSE,
#' then some runs may be skipped.
#' @param whichruns Optional vector of run indices to do. This can be used to
#' re-run a subset of the cases in situations where the function was
#' interupted or some runs fail to converge. Must be a subset of 1:n, where n
#' is the length of profilevec.
#' @param verbose Controls amount of info output to command line.  Default =
#' TRUE.
#' @note The starting values used in this profile are not ideal and some models
#' may not converge. Care should be taken in using an automated tool like this,
#' and some models are likely to require rerunning with alternate starting
#' values.
#' 
#' Also, someday this function will be improved to work directly with the
#' plotting function \code{\link{SSplotProfile}}, but they don't yet work well
#' together. Thus, even if \code{\link{SS_profile}} is used, the output should
#' be read using \code{\link{SSgetoutput}} or by multiple calls to
#' \code{\link{SS_output}} before sending to \code{\link{SSplotProfile}}.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SSplotProfile}}, \code{\link{SSgetoutput}},
#' \code{\link{SS_changepars}}, \code{\link{SS_parlines}}
#' @keywords data manip
#' @examples
#' 
#'   \dontrun{
#' # note: don't run this in your main directory
#' # make a copy in case something goes wrong
#' mydir <- "C:/ss/Simple - Copy"
#' 
#' # the following commands related to starter.ss could be done by hand
#' # read starter file
#' starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
#' # change control file name in the starter file
#' starter$ctlfile <- "control_modified.ss"
#' # make sure the prior likelihood is calculated
#' # for non-estimated quantities
#' starter$prior_like <- 1
#' # write modified starter file
#' SS_writestarter(starter, dir=mydir, overwrite=TRUE)
#' 
#' # vector of values to profile over
#' h.vec <- seq(0.3,0.9,.1)
#' Nprofile <- length(h.vec)
#' 
#' # run SS_profile command
#' profile <- SS_profile(dir=mydir, # directory
#'                       # "NatM" is a subset of one of the
#'                       # parameter labels in control.ss_new
#'                       model="ss3_safe",
#'                       masterctlfile="control.ss_new",
#'                       newctlfile="control_modified.ss",
#'                       string="steep",
#'                       profilevec=h.vec)
#' 
#' 
#' # read the output files (with names like Report1.sso, Report2.sso, etc.)
#' profilemodels <- SSgetoutput(dirvec=mydir, keyvec=1:Nprofile)
#' # summarize output
#' profilesummary <- SSsummarize(profilemodels)
#' 
#' # OPTIONAL COMMANDS TO ADD MODEL WITH PROFILE PARAMETER ESTIMATED
#' MLEmodel <- SS_output("C:/ss/SSv3.24l_Dec5/Simple")
#' profilemodels$MLE <- MLEmodel
#' profilesummary <- SSsummarize(profilemodels)
#' # END OPTIONAL COMMANDS
#' 
#' # plot profile using summary created above
#' SSplotProfile(profilesummary,           # summary object
#'               profile.string = "steep", # substring of profile parameter
#'               profile.label="Stock-recruit steepness (h)") # axis label
#' 
#' # make timeseries plots comparing models in profile
#' SSplotComparisons(profilesummary,legendlabels=paste("h =",h.vec))
#' 
#' }
#' 
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
         overwrite=TRUE,whichruns=NULL,
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
    exe <- paste(model,ifelse(OS=="Windows",".exe",""),sep="")
  }
  # check whether exe is in directory
  if(OS=="Windows"){
    if(!tolower(exe) %in% tolower(dir(dir))) stop("Executable ",exe," not found in ",dir)
  }else{
    if(!exe %in% dir(dir)) stop("Executable ",exe," not found in ",dir)
  }

  # figure out which line to change in control file
  if(is.null(linenum) & is.null(string))
    stop("You should input either 'linenum' or 'string' (but not both)")
  if(!is.null(linenum) & !is.null(string))
    stop("You should input either 'linenum' or 'string', but not both")
  if(usepar){
    if(is.null(parlinenum) & is.null(parstring))
      stop("Using par file. You should input either 'parlinenum' or 'parstring', but not both")
    if(!is.null(parlinenum) & !is.null(parstring))
      stop("Using par file. You should input either 'parlinenum' or 'parstring' (but not both)")
  }
  
  # figure out length of profile vec and sort out which runs to do
  n <- length(profilevec)
  if(n==0) stop("Missing input 'profilevec'")
  if(is.null(whichruns)){
    whichruns <- 1:n
  }else{
    if(!all(whichruns %in% 1:n)){
      stop("input whichruns should be NULL or a subset of 1:",n,"\n",sep="")
    }
  }
  cat("doing runs: ",paste(whichruns,collapse=","),",\n  out of n=",n,"\n",sep="")
  
  converged <- rep(NA,n)
  totallike <- rep(NA,n)
  liketable <- NULL

  cat("changing working directory to",dir,"\n")
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
  for(i in whichruns){
    # check for presence of ReportN.sso files. If present and overwrite=FALSE,
    # then don't bother running anything
    newrepfile <- paste('Report',i,".sso",sep="")
    if(!overwrite & file.exists(newrepfile)){
      cat("skipping profile i=",i,"/",n," because overwrite=FALSE\n",
          "  and file exists: ", newrepfile, "\n", sep="")
    }else{
      cat("running profile i=",i,"/",n,"\n", sep="")
      
      # change initial values in the control file
      # this also sets phase negative which is needed even when par file is used
      SS_changepars(dir=dir,ctlfile=masterctlfile,newctlfile=newctlfile,
                    linenums=linenum,strings=string,
                    newvals=profilevec[i], estimate=FALSE,
                    verbose=TRUE, repeat.vals=TRUE)
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
        if(length(parlinenum)==0) stop("Problem with input parstring = '",parstring,"'",sep="")
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
      if(OS!="Windows") command <- paste("./",command,sep="")
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
    } # end running stuff
  } # end loop of whichruns
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

