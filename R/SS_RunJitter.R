##################
# SS_RunJitter
##################
##' Iteratively apply the jitter option in SS
##'
##' Iteratively runs SS model with different jittered starting parameter values
##' (jitter value must be manually set in starter.ss). Output files are renamed
##' in the format Report1.sso, Report2.sso, etc.
##'
##' @param mydir Directory where model files are located
##' @param model Executable name
##' @param extras Additional command line arguments passed to executable
##' @param Njitter Number of jitters, or a vector of jitter iterations.
##'   If \code{length(Njitter) > 1} only the iterations specified will be ran,
##'   else \code{1:Njitter} will be executed.
##' @param Intern Show command line info in R console or keep hidden (Internal=TRUE)
##' @param systemcmd Option to switch between 'shell' and 'system'
##' @param printlikes Print likelihood values to console
##' @author James T. Thorson, Kelli F. Johnson, Ian G. Taylor
##' @return A vector of likelihoods for each jitter iteration.
##' @export
##' @examples
##'   \dontrun{
##'     #### Change starter file appropriately (can also edit file directly)
##'     starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
##'     # CHANGE THIS FOR GLOBAL_PAR START
##'     starter$init_values_src = 1
##'     # Change jitter (0.1 is an arbitrary, but common choice for jitter amount)
##'     starter$jitter_fraction = 0.1
##'     # write modified starter file
##'     SS_writestarter(starter, dir=mydir, overwrite=TRUE)
##'
##'     #### Run jitter using this function
##'     mydir <- RunFile
##'     extras <- "-nohess"
##'     model <- "ss3"
##'     Njitter <- 25
##'
##'     SS_RunJitter(mydir=mydir, model=model, extras=extras,
##'                  Njitter=Njitter, Intern=TRUE)
##'
##'     #### Read in results using other r4ss functions
##'     # (note that un-jittered model can be read using keyvec=0:Njitter)
##'     profilemodels <- SSgetoutput(dirvec=mydir, keyvec=1:Njitter, getcovar=FALSE)
##'     # summarize output
##'     profilesummary <- SSsummarize(profilemodels)
##'     # Likelihoods
##'     profilesummary$likelihoods[1,]
##'     # Parameters
##'     profilesummary$pars
##'   }

SS_RunJitter <- function(mydir, model="ss",
                         extras="-nohess",
                         Njitter, Intern=TRUE, systemcmd=FALSE,
                         printlikes=TRUE){
  # Determine working directory on start and return upon exit
  startdir <- getwd()
  on.exit(setwd(startdir))

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
  file.copy(from="ParmTrace.sso", to="ParmTrace0.sso", overwrite=TRUE)
  file.copy(from="warning.sso", to="warning0.sso", overwrite=TRUE)
  file.copy(from=paste0(model,".par"), to=paste0(model,".par_0.sso"), overwrite=TRUE)
  file.remove( c("CompReport.sso", "covar.sso", "Report.sso",
                 "ParmTrace.sso", "warning.sso", paste0(model,".par")) )

  if (length(Njitter) == 1) Njitter <- 1:Njitter
  likesaved <- rep(NA, length(Njitter))
  for(i in Njitter){
    print(paste("Jitter=",i,date()))
    file.copy(from=paste0(model,".par_0.sso"), to=paste0(model,".par"), overwrite=TRUE)
    # run model
    command <- paste(model,extras,sep=" ")
    if(OS!="Windows") command <- paste("./",command,sep="")

    if(i==1){
      cat("Running model in directory:",getwd(),"\n")
      cat("Using the command: '",command,"'\n",sep="")
    }
    if(OS=="Windows" & !systemcmd){
      shell(cmd=command, intern=Intern)
    }else{
      system(command, intern=Intern)
    }
    # Only save stuff if it converged
    if( "Report.sso" %in% list.files() ){
      if(printlikes){
        Rep.head <- readLines("Report.sso",n=100)
        likeline <- Rep.head[which(Rep.head=="Component logL*Lambda Lambda")-1]
        like <- as.numeric(substring(likeline,11))
        likesaved[i] <- like
        cat("Likelihood = ",like,"\n")
      }
      # rename output files
      file.copy(from="CompReport.sso", to=paste0("CompReport",i,".sso"), overwrite=TRUE)
      file.copy(from="covar.sso", to=paste0("covar",i,".sso"), overwrite=TRUE)
      file.copy(from="Report.sso", to=paste0("Report",i,".sso"), overwrite=TRUE)
      file.copy(from="ParmTrace.sso", to=paste0("ParmTrace",i,".sso"), overwrite=TRUE)
      file.copy(from="warning.sso", to=paste0("warning",i,".sso"), overwrite=TRUE)
      file.copy(from=paste0(model,".par"), to=paste0(model,".par_",i,".sso"), overwrite=TRUE)
    }else{
      cat("Run = ",i," didn't converge \n")
    }
  }
  # Move original files back
  file.copy(from="CompReport0.sso", to="CompReport.sso", overwrite=TRUE)
  file.copy(from="covar0.sso", to="covar.sso", overwrite=TRUE)
  file.copy(from="Report0.sso", to="Report.sso", overwrite=TRUE)
  file.copy(from="ParmTrace0.sso", to="ParmTrace.sso", overwrite=TRUE)
  file.copy(from="warning0.sso", to="warning.sso", overwrite=TRUE)
  file.copy(from=paste(model,".par_0.sso",sep=""), to=paste(model,".par",sep=""),
            overwrite=TRUE)

  # Return (invisibly), the vector of likelihoods
  return(invisible(likesaved))
}
