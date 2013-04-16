SS_doRetro <- function(masterdir, oldsubdir, newsubdir='retrospectives',
                       subdirstart='retro',years=0:-5,overwrite=TRUE,
                       extras="-nox",intern=FALSE,CallType="system"){

  # save working directory
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  
  olddir <- file.path(masterdir,oldsubdir)
  newdir <- file.path(masterdir,newsubdir)
  
  # make directories, modify starter file, and start retrospective analyses
  
  # get model file names from olddir
  exefile <- dir(olddir)[grep(".exe",dir(olddir))]
  startfile <- dir(olddir)[tolower(dir(olddir))=="starter.ss"]
  forefile <- dir(olddir)[tolower(dir(olddir))=="forecast.ss"]
  wtatagefile <- dir(olddir)[tolower(dir(olddir))=="wtatage.ss"]
  testfile <- dir(olddir)[tolower(dir(olddir))=="test.ss"]

  if(length(startfile)==0) stop("No starter.ss file found in ",olddir)

## print(getwd())
## print(startfile)
  startfile <- file.path(olddir,startfile)
  
  cat("Get input file names from starter file:",startfile,"\n")
  starter <- SS_readstarter(startfile,verbose=FALSE)
  ctlfile <- starter$ctlfile
  datfile <- starter$datfile

  filenames <- c(exefile,forefile,ctlfile,datfile,wtatagefile,testfile)
  cat('copying model files from\n',olddir,'\nto\n',newdir,'\n')
  cat('model files to copy:',filenames,sep='\n ')
      
    
  if(!file.exists(newdir)) dir.create(newdir)
  
  subdirnames <- paste(subdirstart, years, sep='')

  for(iyr in 1:length(years)){
    # create directory
    if(!file.exists(file.path(newdir,subdirnames[iyr])))
      dir.create(file.path(newdir,subdirnames[iyr]))
    # copy files
    file.copy(file.path(olddir,filenames),
              file.path(newdir,subdirnames[iyr],filenames),
              overwrite=TRUE)
    # change starter file to do retrospectives
    starter$retro_yr <- years[iyr]
    starter$init_values_src = 0
    setwd(file.path(newdir,subdirnames[iyr]))
    SS_writestarter(starter, dir=getwd(), verbose=FALSE, overwrite=TRUE)

    ## # someday the code could be expanded to fix data file if it has blocks
    ## ctl <- SS_parlines(ctlfile) # doesn't currently read columns with block info
    ctl <- readLines(ctlfile)
    ctl[grep('block designs',ctl)] <- "0 # Number of block designs for time varying parameters"
    ctl[grep('blocks per design',ctl)+0:2] <- "# blocks deleted"
    file.remove(ctlfile)
    writeLines(ctl, ctlfile)
    
    # run model
    cat("Running model in ",getwd(),"\n",sep="")
    if(file.exists("covar.sso")) file.remove("covar.sso")
    if(intern) cat("ADMB output generated during model run will be written to:\n   ",
                   getwd(),"/ADMBoutput.txt. \n   To change this, set intern=FALSE\n",
                   "Note: ignore message about 'Error trying to open data input file ss3.dat'\n",
                   sep="")
    if(CallType=="system") ADMBoutput <- system(paste(exefile,extras),intern=intern)
    if(CallType=="shell") ADMBoutput <- shell(paste(exefile,extras),intern=intern)
    if(intern) writeLines(c("###","ADMB output",as.character(Sys.time()),
                            "###"," ",ADMBoutput), con = 'ADMBoutput.txt')
    setwd('..')
    
  }
  setwd(oldwd)
}


## if(FALSE){
##   #### example use
##   # source this file
##   source('c:/SS/hake/Hake_2012/retro/retro_script.R')

##   # move to directory one level above existing model run
##   setwd('C:/ss/hake/Hake_2013/runs/')

##   # run the function above
##   SS_doRetro(olddir='2013hake_12',years=0:-10)
##   # read in output
##   retroModels <- SSgetoutput(dirvec=paste('retrospectives/retro',-10:0,sep=''))
##   # summarize output
##   retroSummary <- SSsummarize(retroModels)

##   # set the ending year of each model in the set
##   endyrvec <- retroModels[[1]]$endyr-10:0
##   # make comparison plot
##   pdf('retrospectives/retrospective_comparison_plots.pdf')
##   SSplotComparisons(retroSummary,endyrvec=endyrvec,new=FALSE)
##   dev.off()

##   # make Ianelli-style plot of recdev retrospectives using a different function
##   pdf('retrospectives/retrospective_dev_plots.pdf',width=7,height=10)
##   par(mfrow=c(2,1))
##   # first scaled relative to most recent estimate
##   SSplotRetroDevs(retroSummary, endyrvec=endyrvec, cohorts=1999:2012, relative=TRUE, legend=FALSE)
##   # second without scaling
##   SSplotRetroDevs(retroSummary, endyrvec=endyrvec, cohorts=1999:2012, relative=FALSE, legend=FALSE)
##   dev.off()

## }

