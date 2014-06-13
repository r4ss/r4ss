################## 
# SS_RunJitter
##################
##' Iteratively apply the jitter option in SS
##'
##' Iteratively runs SS model with different jittered starting parameter values
##' (jitter value must be mannually set in starter.ss). Output files are renamed
##' in the format Report1.sso, Report2.sso, etc.
##' 
##' @param mydir Directory where model files are located
##' @param model Executable name
##' @param extras Additional command line arguments passed to executable
##' @param Njitter Number of jitters
##' @param Intern Show command line info in R console or keep hidden (Internal=TRUE)
##' @param systemcmd Option to switch between 'shell' and 'system'
##' @param printlikes Print likelihood values to console
##' @author Jim Thorson
SS_RunJitter <- function(mydir, model="ss3",
                         extras="-nohess -cbs 500000000 -gbs 500000000",
                         Njitter, Intern=TRUE, systemcmd=FALSE,
                         printlikes=TRUE){

  # determine operating system in a relatively brute force way
  OS <- "Mac" # don't know the version$os info for Mac
  if(length(grep("linux",version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw",version$os)) > 0) OS <- "Windows"

  setwd(mydir)
  # read starter file to test for non-zero jitter value
  starter <- SS_readstarter("starter.ss")
  if(starter$jitter_fraction == 0){
    stop("Change starter file to have jitter value > 0")
  }
  file.copy(from="CompReport.sso", to="CompReport0.sso", overwrite=TRUE)
  file.copy(from="covar.sso", to="covar0.sso", overwrite=TRUE)
  file.copy(from="Report.sso", to="Report0.sso", overwrite=TRUE)
  file.copy(from=paste(model,".par",sep=""), to=paste(model,".par_0.sso",sep=""), overwrite=TRUE)
  for(i in 1:Njitter){
    print(paste("Jitter=",i,date()))
    file.copy(from=paste(model,".par_0.sso",sep=""), to=paste(model,".par",sep=""), overwrite=TRUE)
    # run model
    command <- paste(model,extras,sep=" ")
    if(i==1){
      cat("Running model in directory:",getwd(),"\n")
      cat("Using the command: '",command,"'\n",sep="")
    }
    if(OS=="Windows" & !systemcmd){
      shell(cmd=command, intern=Intern)
    }else{
      system(command, intern=Intern)
    }
    if(printlikes){
      Rep.head <- readLines("Report.sso",n=100)
      likeline <- Rep.head[which(Rep.head=="Component logL*Lambda Lambda")-1]
      like <- as.numeric(substring(likeline,11))
      cat("Likelihood = ",like,"\n")
    }
    # rename output files
    file.copy(from=paste("CompReport.sso"), to=paste("CompReport",i,".sso",sep=""), overwrite=TRUE)
    file.copy(from=paste("covar.sso"), to=paste("covar",i,".sso",sep=""), overwrite=TRUE)
    file.copy(from=paste("Report.sso"), to=paste("Report",i,".sso",sep=""), overwrite=TRUE)
    file.copy(from=paste(model,".par",sep=""), to=paste(model,".par_",i,".sso",sep=""), overwrite=TRUE)
  }
  # Move original files back
  file.copy(from="CompReport0.sso", to="CompReport.sso", overwrite=TRUE)
  file.copy(from="covar0.sso", to="covar.sso", overwrite=TRUE)
  file.copy(from="Report0.sso", to="Report.sso", overwrite=TRUE)
  file.copy(from=paste(model,".par_0.sso",sep=""), to=paste(model,".par",sep=""), overwrite=TRUE)
}


## ##################
## # Example for help page
## ##################

## #### Change starter file appropriately
## starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
## # CHANGE THIS FOR GLOBAL_PAR START
## starter$init_values_src = 1
## # Change jitter
## starter$jitter_fraction = 0.1
## # write modified starter file
## SS_writestarter(starter, dir=mydir, overwrite=TRUE)

## # Run jitter
## mydir <- RunFile
## extras = "-nohess -cbs 500000000 -gbs 500000000"
## model = "ss3"
## Njitter = 25

## SS_RunJitter(mydir=mydir, model=model, extras=extras, Njitter=Njitter, Intern=TRUE)

## # Read in results
## profilemodels <- SSgetoutput(dirvec=mydir, keyvec=1:Njitter, getcovar=FALSE)
## # summarize output
## profilesummary <- SSsummarize(profilemodels)
## # Likelihoods
## profilesummary$likelihoods[1,]
## # Parameters
## profilesummary$pars
