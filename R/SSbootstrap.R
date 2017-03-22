#' Fit models to parametric bootstraps
#' 
#' Run a series of models fit to parametric bootstrap data taken from
#' data.ss_new.  This is not yet a generalized function, just some example code
#' for how to do a parametric bootstrap such as was done for the Pacific hake
#' model in 2006.
#' 
#' 
#' @note Thanks to Nancie Cummings for inspiration.
#' @author Ian Taylor
#' @export
#' @references
#' \url{http://www.pcouncil.org/wp-content/uploads/2006_hake_assessment_FINAL_ENTIRE.pdf}
#' (A description is on page 41 and Figures 55-56 (pg 139-140) show some
#' results.)
SSbootstrap <- function(){
  # this is not yet a generalized function, just some example code for how to do
  # a parametric bootstrap such as was done for the Pacific hake model in 2006
  # See http://www.pcouncil.org/wp-content/uploads/2006_hake_assessment_FINAL_ENTIRE.pdf
  # A description is on page 41 and Figures 55-56 (pg 139-140) show some results.

  # Written by Ian Taylor on 10/11/2012 after disucussion with Nancie Cummings
  
  # first set "Number of datafiles to produce" in starter.ss = 100 or some large number
  # re-run model to get data.ss_new file concatenating all bootstrap data files

  # Directory where bootstrap will be run.
  # You probably want to use a copy of the directory where you ran it,
  # so as not to overwrite the true results.
  inpath <- 'c:/Simple'

  #setwd(inpath) # change working directory (commented out to avoid violating CRAN policy)

  # split apart data.ss_new into multiple data files with names like "BootData1.ss"
  SS_splitdat(inpath=inpath, outpath=inpath, number=TRUE, MLE=FALSE)

  N <- 10 # number of bootstrap models to run (less than or equal to setting in starter)

  starter <- SS_readstarter(file="starter.ss") # read starter file
  file.copy("starter.ss","starter_backup.ss") # make backup

  # loop over bootstrap files
  for(iboot in 1:N){
    # note what's happening
    cat("\n##### Running bootstrap model number",iboot," #########\n") 
    
    # change data file name in starter file
    starter$datfile <- paste("BootData",iboot,".ss",sep="")
    # replace starter file with modified version
    SS_writestarter(starter, overwrite=TRUE)

    # delete any old output files
    file.remove("Report.sso")
    file.remove("CompReport.sso")
    file.remove("covar.sso")

    # run model
    shell("SS3")
    # for some computers or versions of R, "shell" works better than "system"
    #system("ss3")

    # copy output files (might be good to use "file.exists" command first to check if they exist
    file.copy("Report.sso",paste("Report_",iboot,".sso",sep=""))
    file.copy("CompReport.sso",paste("CompReport_",iboot,".sso",sep=""))
    file.copy("covar.sso",paste("covar_",iboot,".sso",sep=""))
    # other .sso files could be copied as well
    
  }

  # read and summarize all models
  # (setting getcomp=FALSE will produce warning
  #  about missing comp file, but use less memory)
  bootmodels <- SSgetoutput(keyvec=paste("_",1:N,sep=""), dirvec=inpath, getcomp=FALSE)
  bootsummary <- SSsummarize(bootmodels)

  # a bunch of plots that won't work well if there are lots of models
  SSplotComparisons(bootsummary,png=TRUE)
  # histogram of a single quantity
  hist(as.numeric(bootsummary$quants[bootsummary$quants$Label=="SPB_Virgin",1:N]))
  hist(as.numeric(bootsummary$pars[grep("R0",bootsummary$pars$Label),1:N]))


}
