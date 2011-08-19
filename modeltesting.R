# a collection of functions for testing a new SS executable

copyinputs <-
  function(olddir="c:/SS/Version_A",
           newdir="c:/SS/Version_B",
           use_ss_new=FALSE)
{
  # source the SS_readstarter function
  source("http://r4ss.googlecode.com/svn/trunk/SS_readstarter.R")
  if(!file.exists(newdir)) dir.create(newdir)

  if(is.na(file.info(olddir)$isdir)) stop("not a directory:",olddir)
  folderlist <- NULL
  stuff <- dir(olddir) # get list of subfolders within olddir
  for(i in 1:length(stuff)){ # loop over things within this directory
    # get full path of file
    foldername <- stuff[i]
    oldsubfolder <- paste(olddir,foldername,sep="/")
    # check if it's a directory
    info <- file.info(oldsubfolder) 

    if(info$isdir){ # if it's a directory, then do stuff
      cat("checking",oldsubfolder,"\n")
      # get starter and forecast, allowing for differences in capitalization
      starterfile <- dir(oldsubfolder)[grep("^starter.ss$",tolower(dir(oldsubfolder)))]
      forecastfile <- dir(oldsubfolder)[grep("^forecast.ss$",tolower(dir(oldsubfolder)))]
      wtatagefile <- dir(oldsubfolder)[grep("^wtatage.ss$",tolower(dir(oldsubfolder)))]

      if(length(starterfile)==1){
        folderlist <- c(folderlist,foldername) # keep a record of folders with model files inside
        
        if(use_ss_new){ # if requested, use the .ss_new files as sources
          starterfile <- dir(oldsubfolder)[grep("^starter.ss_new$",tolower(dir(oldsubfolder)))]
          forecastfile <- dir(oldsubfolder)[grep("^forecast.ss_new$",tolower(dir(oldsubfolder)))]
          wtatagefile <- dir(oldsubfolder)[grep("^wtatage.ss_new$",tolower(dir(oldsubfolder)))]
        }
        # new file names (forcing lowercase for consistency and linux compatibility)
        newstarterfile <- "starter.ss"
        newforecastfile <- "forecast.ss"
        newwtatagefile <- "wtatage.ss"
          
        # read starter file using SS_readstarter function
        cat("reading starter file:",starterfile,"\n")
        startercontents <- SS_readstarter(paste(oldsubfolder,starterfile,sep="/"))
        # get ctl and data file names from contents of starter file
        newdatfile <- datfile <- startercontents$datfile
        newctlfile <- ctlfile <- startercontents$ctlfile

        # if requested, have the source file names be based on the .ss_new files
        if(use_ss_new){
          SS_splitdat(inpath=oldsubfolder,
                      outpath=oldsubfolder,
                      inputs=T,MLE=F)
          datfile <- "BootData.ss"
          ctlfile <- "control.ss_new"
        }
        
        # create new directory within newdir
        newsubfolder <- paste(newdir,foldername,sep="/")
        cat("creating new directory:",newsubfolder,"\n")
        dir.create(newsubfolder)

        # copy files to new directories
        copyfile <- function(filename1,filename2=NULL){
          # simple function to make repeated task more efficient
          if(is.null(filename2)) filename2 <- filename1
          old <- paste(oldsubfolder,filename1,sep="/")
          new <- paste(newsubfolder,filename2,sep="/")
          x <- file.copy(old,new)
          if(x) cat("success copying") else cat("failure copying")
          cat("\n old:", old, "\n new:", new, "\n")
        }
        oldfilelist <- c(starterfile,forecastfile,datfile,ctlfile,wtatagefile)
        newfilelist <- c(newstarterfile,newforecastfile,newdatfile,newctlfile,newwtatagefile)
        # loop over input files
        for(ifile in 1:length(oldfilelist)){
          copyfile(filename1=oldfilelist[ifile],
                   filename2=newfilelist[ifile])
        }
      }else{ # if check for starter file fails
        cat("  !no starter.ss file in this directory\n")
      }# end check for starter file
    } # end check if it's a directory
  } # end loop over stuff within olddir
  return(list(olddir=olddir, newdir=newdir, folderlist=folderlist))
}

copyexe <-
  function(sourcedir="c:/dir_with_exe/",
           newdir="c:/newmodel/",
           folderlist=c("Example_1","Example_2"),
           exe="SS3_safe.exe", overwrite=FALSE)
{
  fullexe <- paste(sourcedir,exe,sep="/")
  if(!file.exists(fullexe)) stop("file missing:",fullexe) 
  for(i in 1:length(folderlist)){
    new <- paste(newdir,folderlist[i],exe,sep="/")
    x <- file.copy(fullexe, new, overwrite=overwrite)
    if(x) cat("success copying") else cat("failure copying")
    cat(" exe to:", new, "\n")
  } # end loop over folders
}

runmodels <-
  function(newdir="c:/newmodel/",
           folderlist=c("Example_1","Example_2"),
           exe="SS3_safe.exe",
           extras="-nox -gbs 100000000 -cbs 100000000",
           intern=FALSE)
{
  for(i in 1:length(folderlist)){
    new <- paste(newdir,folderlist[i],sep="/")
    cat("running model in",new,"\n")
    setwd(new)
    ADMBoutput <- system(paste(exe,extras),intern=intern)
    if(intern){
      writeLines(c("###","ADMB output",paste("key =",key),as.character(Sys.time()),
                            "###"," ",ADMBoutput), con = 'ADMBoutput.txt')
      cat("commandline stuff written to ADMBoutput.txt\n")
    }
  } # end loop over folders
}

addtotable <- function(dir="\\\\nwcfs2\\assessment\\FramPublic\\StockSynthesisStuff\\modeltesting\\",
                       SSversions=c("Version_X","Version_Y"),
                       oldtable="summarytable.csv",
                       newtable="newsummarytable.csv")
{
  cat("reading",paste(dir,oldtable,sep="\\"),"\n")
  summarytable <- read.csv(paste(dir,oldtable,sep="\\"))

  # stuff from existing table
  Model <- summarytable$Model
  Quant <- summarytable$Quantity
  nquants <- length(unique(Quant))

  alloutputs <- list()
  
  for(iversion in 1:length(SSversions)){ # loop over versions of SS
    newcolumn <- rep(NA,nrow(summarytable))

    cat("\nGetting info from folder:",SSversions[iversion],"\n")
    versionfolder <- paste(dir,SSversions[iversion],sep="\\")
    if(is.na(file.info(versionfolder)$isdir)) stop(versionfolder,"\n  is not a directory")

    # check for which are directories
    folderlist <- NULL
    stuff <- dir(versionfolder) # get list of subfolders within olddir
    for(i in 1:length(stuff)){ # loop over things within this directory
      # get full path of file
      foldername <- stuff[i]
      subfolder <- paste(versionfolder,foldername,sep="/")
      # check if it's a directory
      info <- file.info(subfolder) 

      if(info$isdir){ # if it's a directory, then do stuff
        cat("checking",subfolder,"\n")
        # get starter and forecast, allowing for differences in capitalization
        starterfile <- dir(subfolder)[grep("^starter.ss$",tolower(dir(subfolder)))]

        if(length(starterfile)==1) folderlist <- c(folderlist,foldername)
      }
    }
    cat("\nGood folders are:",paste(folderlist,collapse="\n                 "),"\n")
    cat("Getting output from those folders...\n\n")
    
    nmodels <- length(folderlist)    
    longfolderlist <- paste(versionfolder,folderlist,sep="\\")

    newoutput <- SSgetoutput(keyvec = NULL, dirvec = longfolderlist,
                              getcovar = TRUE, getcomp = TRUE,
                              forecast = FALSE, verbose = TRUE)
    names(newoutput) <- folderlist

    alloutputs[[SSversions[iversion]]] <- newoutput
    
    for(imodel in 1:nmodels){    # loop over test models within that version

      newreplist <- newoutput[[imodel]]
  
      # make new column
      newcolumn[Model==names(newoutput)[imodel] &
                Quant=="TotalNLL"] <- newreplist$likelihoods_used$values[1]
      newcolumn[Model==names(newoutput)[imodel] &
                Quant=="EndingDepl"] <- newreplist$current_depletion
      newcolumn[Model==names(newoutput)[imodel] &
                Quant=="LogR0"] <- newreplist$parameters$Value[newreplist$parameters$Label %in% c("SR_LN(R0)","SR_R0")]
      newcolumn[Model==names(newoutput)[imodel] &
                Quant=="LogR0_SD"] <- newreplist$parameters$Parm_StDev[newreplist$parameters$Label %in% c("SR_LN(R0)","SR_R0")]
      newcolumn[Model==names(newoutput)[imodel] &
                Quant=="B0_SD"] <- newreplist$derived_quants$StdDev[newreplist$derived_quants$LABEL=="SPB_Virgin"]
      newcolumn[Model==names(newoutput)[imodel] &
                Quant=="ForeCatch_last"] <- tail(newreplist$derived_quants$Value[grep("ForeCatch_",newreplist$derived_quants$LABEL)],1)
      newcolumn[Model==names(newoutput)[imodel] &
                Quant=="ForeCatch_last_SD"] <- tail(newreplist$derived_quants$StdDev[grep("ForeCatch_",newreplist$derived_quants$LABEL)],1)
      newcolumn[Model==names(newoutput)[imodel] &
                Quant=="Nwarnings"] <- newreplist$Nwarnings
      newcolumn[Model==names(newoutput)[imodel] &
                Quant=="MaxGradient"] <- newreplist$maximum_gradient_component
      
    }
    summarytable$newcolumn <- newcolumn
    names(summarytable)[names(summarytable)=="newcolumn"] <- SSversions[iversion]
  }

  
  cat("writing",paste(dir,newtable,sep="\\"),"\n")
  write.csv(summarytable,paste(dir,newtable,sep="\\"),row.names=FALSE)

  return(invisible(alloutputs))
}

extrastuff <- function(){

  # a collection of additional commands that have not been generalized to work as well
  # might be improved upon in the future
  oldfolders <- paste(folderinfo$olddir,folderinfo$folderlist,sep="/")
  newfolders <- paste(folderinfo$newdir,folderinfo$folderlist,sep="/")

  # get output from old model
  oldoutput <- SSgetoutput(keyvec = NULL, dirvec = oldfolders,
                           getcovar = TRUE, getcomp = TRUE,
                           forecast = FALSE, verbose = TRUE)

  # get output from new model
  newoutput <- SSgetoutput(keyvec = NULL, dirvec = newfolders,
                           getcovar = TRUE, getcomp = TRUE,
                           forecast = FALSE, verbose = TRUE)

  names(oldoutput) <- folderinfo$folderlist
  names(newoutput) <- folderinfo$folderlist


  # compare all parameters
  for(i in 1:length(oldoutput)){
    oldreplist <- oldoutput[[i]]
    newreplist <- newoutput[[i]]
    labels <- newreplist$parameters$Label
    oldpars <- oldreplist$parameters$Value
    newpars <- newreplist$parameters$Value
    diff <- (newpars - oldpars)
    reldiff <- (newpars - oldpars)/oldpars
    partable <- data.frame(labels, newpars, oldpars, diff, reldiff)
    abstable <- partable[abs(partable$diff)>1e-3,]
    reltable <- partable[!is.nan(partable$reldiff) & abs(partable$reldiff)>1e-3,]
    cat("\nabsolute differences greater than 1e-3  for model",i,":", folderinfo$folderlist[i],"\n" )
    if(nrow(abstable)>1) print(abstable) else print("none!")
    cat("relative differences greater than 1e-3 for model",i,":", folderinfo$folderlist[i],"\n" )
    if(nrow(reltable)>1) print(reltable) else print("none!")      
  }

  quantities <- c("TotalNLL", "EndingDepl", "LogR0", "LogR0_SD",
                  "B0_SD", "Nwarnings", "MaxGradient")
  temptable <- expand.grid(quantities,folderinfo$folderlist)
  summarytable <- data.frame(Model=temptable$Var2,Quantity=temptable$Var1)
}

checkforreport <- function(dir="default", folderlist="default"){
  if(dir=="default") dir <- getwd()
  if(folderlist=="default"){
    alldir <- dir(dir)
    folderlist <- NULL
    for(folder in alldir){
      if(file.info(paste(dir,folder,sep='/'))$isdir)
        folderlist <- c(folderlist,folder)
    }
  }
  cat("info on Report.sso files in",dir,"\n")
  n <- length(folderlist)
  sizes <- rep(-999,n)
  for(i in 1:n){
    file <- paste(dir,folderlist[i],"Report.sso",sep="/")
    sizes[i] <- file.info(file)$size
  }
  return(data.frame(model=folderlist,filesize=sizes))
}

if(FALSE){
  ## this stuff should be pasted directly into R instead of run as a function
  
  # make directories and copy input files from one folder to the next
  folderinfo <- copyinputs(olddir="c:/SS/modeltesting/Version_3_22b_Aug3",
                           newdir="c:/SS/modeltesting/Version_3_22c_Aug12")

  source('c:/SS/R/r4ss/trunk/modeltesting.R')
  setwd("c:/SS/modeltesting/Version_3_22c_Aug12")
  folderinfo <- list(newdir=getwd(),
                     folderlist=dir())
  # on sysiphus
  folderinfo <- copyinputs(olddir="c:/SS/modeltesting/Version_3_20_Jan3",
                           newdir="y:/h_itaylor/SS/modeltesting/Version_3_20e_Mar15")
  
  # make copy using .ss_new files as sources to use as example files
  folderinfo <- copyinputs(olddir="c:/SS/modeltesting/Version_3_21e_June9_examples",
                           newdir="c:/SS/modeltesting/Version_3_21e_June9_examples_clean",
                           use_ss_new=TRUE)
  folderinfo <- copyinputs(olddir="c:/SS/modeltesting/Version_3_21e_June9_examples_clean",
                           newdir="c:/SS/modeltesting/Version_3_21e_June9_examples_test")

  # copy executables into subfolders where each new model will be run
  copyexe(sourcedir="c:/SS/SSv3.22c_Aug12/",
          newdir=folderinfo$newdir,
          folderlist=folderinfo$folderlist,
          exe="ss3.exe")

  copyexe(sourcedir="y:/h_itaylor/SS/SSv3.20_Jan3",
          newdir=folderinfo$newdir,
          folderlist=folderinfo$folderlist,
          exe="SS3")

  ## # convert to SSv3.20
  ## setwd(folderinfo$newdir)
  ## for(i in 1:length(folderinfo$folderlist)){
  ##   model <- folderinfo$folderlist[i]
  ##   convert_to_v3.20(model,replace=T)
  ## }
  ## for(i in 1:length(folderinfo$folderlist)){
  ##   model <- folderinfo$folderlist[i]
  ##   (file.copy(paste(model,"forecast.ss",sep="/"),paste(model,"old_forecast.ss",sep="/")))
  ##   (file.copy("generic_forecast.ss",paste(model,"forecast.ss",sep="/"),overwrite=TRUE))
  ## }

  # run new SS executable for each example model without estimating anything
  runmodels(newdir=folderinfo$newdir,
            folderlist=folderinfo$folderlist,exe="ss3.exe",extras="-noest -nohess -nox")

  # run new SS executable for each example model 
  runmodels(newdir=folderinfo$newdir,
            folderlist=folderinfo$folderlist,exe="ss3.exe",extras="-nox")

  # alternatively, run models in all subfolders
  #   if the folderinfo object is not available
  source("c:/SS/R/r4ss/trunk/modeltesting.R")
  mydir <- "c:/SS/modeltesting/Version_3_22c_Aug12"
  runmodels(newdir=mydir, folderlist=dir(mydir),exe="SS3.exe",extras="-nox")
  runmodels(newdir=mydir, folderlist=dir(mydir),exe="SS3.exe",extras="-noest -nohess -nox")

  # on sysiphus
  source("http://r4ss.googlecode.com/svn/trunk/modeltesting.R")
  mydir <- "~/h_itaylor/SS/modeltesting/Version_3_20_Jan3"
  runmodels(newdir=mydir, folderlist=dir(mydir),exe="./SS3",extras="-nox")
  
  # get updated package files, including the SSgetoutput function
  library(r4ss)
  update_r4ss_files()

  #for(folder in dir(mydir)) file.remove(paste(folder,"SS3_safe.log",sep="/"))
  
  # read the output from the new runs and add it to the summary table
  alloutput <-
    addtotable(dir = "c:/SS/modeltesting/", 
               #dir = "\\\\nwcfs2\\assessment\\FramPublic\\StockSynthesisStuff\\modeltesting\\", 
               oldtable = "summarytable.csv", 
               newtable = "newsummarytable.csv",
               SSversions=c("Version_3_22c_Aug12"))

  # example on sysiphus
  alloutput <-
    addtotable(dir = "y:/h_itaylor/SS/modeltesting/", 
               oldtable = "summarytable.csv", 
               newtable = "newsummarytable.csv",
               SSversions=c("Version_3_20_Jan3"))
  
  # making plots
  for(i in length(alloutput):1){
    models <- alloutput[[i]]
    testvec <- rep(NA, length(models))
    for(j in 1:length(models)){
      test <- SS_plots(models[[j]],pdf=T,verbose=F,forecast=F,datplot=T)
      if(test==999){
        cat("!!!! plot code succeeded on model",j,"\n")
      }else{
        cat("!!!! plot code failed on model",j,"\n")
      }
      testvec[j] <- test
    }
    print(testvec)
  }

  
  # running on linux
  newdir <- "~/h_itaylor/SS/modeltesting/Version_3_11c_Oct30/"
  copyexe(sourcedir="~/h_itaylor/SS/SSv3.11c_Oct30",
          newdir=newdir,
          folderlist=dir(newdir),
          exe="SS3admb10")
  runmodels(newdir=newdir, folderlist=dir(newdir),exe="./SS3admb10")
}
