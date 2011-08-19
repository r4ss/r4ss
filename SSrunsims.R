SSrunsims <-
function(sims=1,newrun=TRUE,sim=FALSE,fit=FALSE,
         simfolder="sims",
         fitfolder="fits",
         masterfolder="fits",
         MLEdata=FALSE,skipfiles=TRUE,
         simchoices=1,fitchoices=1,samedatafile=FALSE,
         CAAL=TRUE,
         homepath="c:/myfiles/",
         recdevmatrix=NULL,
         rescale=TRUE,
         fitbiasramp=FALSE,
         exe="ss3_opt",
         simextras="-nox -nohess",
         fitextras="-nox -gbs 1000000000 -cbs 1000000000",
         fyr=NULL, lyr=NULL,
         printfile=TRUE,
         intern=FALSE,
         verbose=TRUE)
{
  # function for running simulation and estimation models

  cat("running SSrunsims\n")

  if(0 %in% sims) cat("runs with isim 0 will have no recdevs included\n")
  
  # create file to save list of completed model runs
  starttime <- Sys.time()
  simnotesfile <- paste(homepath,"/simnotes_",format(starttime,"%d-%b-%Y_%H.%M" ),".csv",sep="")
  fitnotesfile <- paste(homepath,"/fitnotes_",format(starttime,"%d-%b-%Y_%H.%M" ),".csv",sep="")

  notes <- list() # list to store notes on model runs

  # do some setup stuff (no reason not to repeat each time, but could be skipped)
  if(newrun){
    # setup home directory based on computer being used
    if(.Platform$OS.type=="windows") win <- T  else  win <- F
    if(substring(homepath,nchar(homepath)) %in% c("/","\\")) homepath <- substring(homepath,1,nchar(homepath)-1)
    if(is.na(file.info(homepath)$isdir)){
        cat("need to create directory:",homepath,"\n")
        cat("  containing master control and data files\n")
        return()
    }
    if(verbose) cat("setting working directory to:",homepath,"\n")
    setwd(homepath)

    # read large matrix of recruitment deviations if not provided
    if(is.null(recdevmatrix)){
      recmatfile <- paste(homepath,"recdevmatrix.csv",sep="/")
      if(file.exists(recmatfile)){
        recmat = read.csv(recmatfile)
      }else{ # or create a matrix if it doesn't exist already (add random seed in future?)
        recmat = NULL
        for(i in 1:1000) # up to 1000 simulations
        {
          recdevs = rnorm(200,0,1) # up to 200 years per simulation
          recmat = cbind(recmat,recdevs)
        }
        write.csv(recmat,recmatfile,row.names=FALSE)
      }
    }else{
      recmat <- recdevmatrix
    }

    simpath <- paste(homepath,simfolder,sep="/") # path for simulations
    fitpath <- paste(homepath,fitfolder,sep="/") # path for estimation

    if(is.na(file.info(simpath)$isdir) || !file.info(simpath)$isdir){
      # create folders to store simulations and estimations
      dir.create(path=simpath)
      dir.create(path=fitpath)
      cat("put starter.ss, forecast.ss, and executable or batch file into the directories:\n")
      return()
    }

    # path to collect results if distributing over multiple simultaneous runs
    masterpath <- paste(homepath,masterfolder,sep="/")
  }
  #################################################

  # run simulation model
  if(sim){
    setwd(simpath)
    if(verbose) cat("running simulations in",simpath,"\n")
    for(isim in sims){
      if(isim==0) norecdevs <- TRUE # turn off recdevs for sim numbered 0
      for(isimchoice in simchoices){
        if(verbose) cat("  in isimchoice=",LETTERS[isimchoice],"\n",sep="")
        filekey <- paste("sim",LETTERS[isimchoice],sep="") # description of this sim
        key <- paste(filekey,isim,sep="") # key has index of simulation added
        if(verbose) cat("  key = ",key,"\n",sep="")

        bootdatfile <- paste("bootdat_",key,".ss",sep="")
        bootdatfilesize <- file.info(bootdatfile)$size
        if(!is.na(bootdatfilesize) & bootdatfilesize>0 & skipfiles==TRUE){
            # skip this run if a file with non-zero file size exists already
            cat("skipping ",bootdatfile," with size=",bootdatfilesize,"\n",sep="")
        }else{
          ## run simulation model
          # copy control file
          ctl_sim <- paste(homepath,"/ctl_",filekey,".ss",sep="")
          temp <- file.copy(ctl_sim,"ctl_isim_nodevs.ss",overwrite=TRUE)
          if(temp!=TRUE){
              cat("Error copying",ctl_sim,"\n")
              break()}
          # copy data file
          if(samedatafile){
              dat_sim <- paste(homepath,"/dat_master.ss",sep="")
          }else{
              dat_sim <- paste(homepath,"/dat_",filekey,".ss",sep="")
          }
          temp <- file.copy(dat_sim,"dat_isim.ss",overwrite=TRUE)
          if(temp!=TRUE){
              cat("Error copying",dat_sim,"\n")
              break()}

          if(norecdevs){
            # if no recdevs are to be modeled
            file.copy("ctl_isim_nodevs.ss","ctl_isim.ss")
          }else{
            # add recdevs to control files for simulations
            if(is.null(fyr) | is.null(lyr)){
              myctl <- SS_readctl("ctl_isim_nodevs.ss")
              fyr <- myctl$fyr_main_recdevs
              lyr <- myctl$lyr_main_recdevs
            }
            recdevs <- recmat[1:(lyr-fyr+1),isim]

            SS_recdevs(fyr=fyr, lyr=lyr, recdevs=recdevs,
                       rescale=rescale, scaleyrs=NULL,
                       ctlfile="ctl_isim_nodevs.ss",
                       newctlfile="ctl_isim.ss",
                       verbose=TRUE, writectl=TRUE, returnctl=FALSE)
          }
          # run simulation
          if(file.exists("covar.sso")) file.remove("covar.sso")
          if(intern) cat("Running model. ADMB output generated during model run will be written to:\n   ",
                          getwd(),"/ADMBoutput.txt. \n   To change this, set intern=FALSE\n",sep="")
          ADMBoutput <- system(paste(exe,simextras),intern=intern)
          if(intern) writeLines(c("###","ADMB output",paste("key =",key),as.character(Sys.time()),
                                  "###"," ",ADMBoutput), con = 'ADMBoutput.txt')
          # rename files
          file.copy("Report.sso",paste("Report_",key,".sso",sep=""),overwrite=TRUE)
          file.copy("CompReport.sso",paste("CompReport_",key,".sso",sep=""),overwrite=TRUE)

          # split apart simulation results
          if(CAAL){
            bootstrap_CAAL(master=F,
                           infile=paste(simpath,"/data.ss_new",sep=''),
                           outfile=paste(simpath,"/bootdat_",key,".ss",sep=''),
                           MLE=MLEdata
                           )
            cat("doing conditional age at length sampling\n")
          }else{
            SS_splitdat(inpath     = simpath,
                        outpath    = simpath,
                        inname     = "data.ss_new",
                        outpattern = paste("bootdat_",key,sep=""),
                        MLE        = MLEdata
                        )
          }
          # fill in or create a data frame to store notes on model runs
          if(exists("simnotes")) simnotes[nrow(simnotes)+1,] <- data.frame(isim, isimchoice, key, Sys.time(), stringsAsFactors=FALSE)
          else simnotes <- data.frame(sim=isim, simchoice=isimchoice, key=key, time=Sys.time(), stringsAsFactors=FALSE)

          # save runs completed so far
          if(printfile) write.csv(simnotes,simnotesfile)
        } # end if file doesn't already exist
      } # end simchoices loop
    } # end isim loop
    setwd(homepath) # reset working directory
    if(exists("simnotes")) notes$simnotes <- simnotes else notes$simnotes <- NA
  } # end if sim
  #################################################

  # run estimation model
  if(fit)
  { # if fits are requested
    setwd(fitpath) # change path
    for(ifit in sims){ # loop over number of simulations in each scenario
      if(ifit==0) norecdevs <- TRUE
      for(isimchoice in simchoices){ # loop over simulation scenarios
        for(ifitchoice in fitchoices){ # loop over estimation scenarios
          for(ibiasadj in unique(c(FALSE,fitbiasramp))){ # loop over whether to apply the bias adjustment function

            if(verbose) cat("running estimation models")

            if(verbose & exists("runtime")){
              cat("Duration of previous model run:",Sys.time() - runtime,"\n")
              runtime <- Sys.time()
            }
            # text description of particular case
            filekey <- paste("fit",LETTERS[ifitchoice],sep="")
            key <- paste("sim",LETTERS[isimchoice],"_fit",LETTERS[ifitchoice],ifit,sep="")

            # if fitbiasramp will be applied on some model runs
            # but NOT this one
            if(fitbiasramp & !ibiasadj) key <- paste(key,"_nobiasadj",sep="")
            if(verbose) cat("key = ",key,", max(sims) = ",max(sims),"\n",sep="")

            # new names for output files
            repfilename <- paste("Report_",key,".sso",sep="")
            compfilename <- paste("CompReport_",key,".sso",sep="")
            covarname <- paste("covar_",key,".sso",sep="")
            repmaster <- paste(masterpath,repfilename,sep="/")
            repmastersize <- file.info(repmaster)$size

            ## run estimation model
            if(!is.na(repmastersize) & repmastersize>0 & skipfiles==TRUE){
              # skip this run if a file with non-zero file size exists already
              cat("skipping",repfilename,"with size =",repmastersize,"\n")
            }else{
              if(is.na(repmastersize)){
                cat("no rep file matching:",repfilename,"\n")
              }else{
                if(repmastersize==0) cat("empty rep file:",repfilename)
              }
              if(skipfiles) cat("running model to replace\n")
              if(!skipfiles) cat("running model to create\n")
              # write temporary rep file to show that this run is active
              writeLines(c("Temporary report file to show this model is currently active",
                           paste("running in",getwd()),
                           as.character(Sys.time())),
                         repmaster)
              if(verbose) cat("writing temporary file showing activity in",repmaster,"\n")

              # copy data file
              if(samedatafile){
                dat_fit <- "../dat_master.ss"
              }else{
                dat_fit <- paste("../dat_",filekey,".ss",sep="")
              }
              temp <- file.copy(dat_fit,"dat_ifit.ss",overwrite=TRUE)
              if(temp!=TRUE){
                cat("Error copying",dat_fit,"\n")
                break()}

              # copy control file
              ctl_fit <- paste("../ctl_",filekey,".ss",sep="")
              temp <- file.copy(ctl_fit,"ctl_ifit.ss",overwrite=TRUE)
              if(temp!=TRUE){
                cat("Error copying",ctl_fit,"\n")
                break()}

              # if NO recdevs will be estimated
              if(norecdevs){
                ctl <- readLines("ctl_ifit.ss")
                ctl[grep("do_recdev", ctl)] <- "0 #do_recdev:  0=none; 1=devvector; 2=simple deviations"
                writeLines(ctl,"ctl_ifit.ss")
              }
              dorun <- TRUE # switch for whether run will be conducted or not
              # if fitbiasramp will be applied on some model runs
              # INCLUDING this one
              if(!norecdevs & fitbiasramp & ibiasadj){
                dorun <- FALSE # switch for whether run will be conducted or not
                # get output from previous run
                tempkey <- paste(key,"_nobiasadj",sep="")

                # new names for output files
                oldrepfilename <- paste("Report_",tempkey,".sso",sep="")
                oldcompfilename <- paste("CompReport_",tempkey,".sso",sep="")
                oldcovarname <- paste("covar_",tempkey,".sso",sep="")
                newctl <- paste("ctl_",key,".ss",sep="")

                if(verbose) cat("applying bias adjustment based on previous model run\n")
                # check for covar file and a real report file
                # (not just the temporary file written by this function)
                if(file.exists(paste(masterpath,oldcovarname,sep='/'))
                   & file.info(paste(masterpath,oldrepfilename,sep='/'))$size > 200)
                {
                  dorun <- TRUE
                  # if the covar file exists, then apply fitbiasramp function
                  replist <- SS_output(dir=masterpath,model=exe,repfile=oldrepfilename,
                                       compfile=oldcompfilename,covarfile=oldcovarname,
                                       forecast=FALSE,printstats=FALSE,verbose=FALSE)
                  SS_fitbiasramp(replist,pdf=paste("fitbiasramp_",key,".pdf",sep=""),
                                 oldctl="ctl_ifit.ss",newctl=newctl)
                  file.copy(newctl,"ctl_ifit.ss",overwrite=TRUE)
                }else{
                  cat("run failed to converge (or is being run by another R process).","\n")
                  cat("key:",tempkey,"\n")
                }
              } # end fit of bias adjustment on the fly

              if(!dorun){
                # don't do this run
                cat("skipping run with key =",key,
                            "\n  because previous no previous run exists on which to base bias adjustment")
              }else{
                # run this run
                repmastersize <- file.info(repmaster)$size
                if(is.na(repmastersize)){
                  cat("no rep file, running to create ",repmaster," with size=",repmastersize,"\n",sep="")
                }else{
                  if(repmastersize==0){
                    cat("empty rep file, running to create ",repmaster," with size=",repmastersize,"\n",sep="")
                  }
                  if(repmastersize>0 & repmastersize<200){
                    cat("temporary rep file exists, running to replace\n")
                  }
                  if(repmastersize>200){
                    cat("full rep file exists, replacing\n")
                  }
                }

                # get bootstrap data from simulations folder
                boot1 <- paste("../",simfolder,"/bootdat_sim",LETTERS[isimchoice],ifit,".ss",sep="")
                boot2 <- "dat_ifit.ss"

                ## cat("info on bootstrap data file:\n")
                ## cat(file.info(boot1))
                ## cat(file.info(boot2))
                temp <- file.copy(boot1,boot2,overwrite=TRUE)
                if(temp!=TRUE){
                  cat("!error copying bootstrap data files: make sure simulations were run to create them\n")
                  break()
                }

                if(file.exists("covar.sso")) file.remove("covar.sso")
                if(intern) cat("Running model. ADMB output generated during model run will be written to:\n   ",
                               getwd(),"/ADMBoutput.txt. \n To change this, set intern=FALSE\n",sep="")
                ADMBoutput <- system(paste(exe,fitextras),intern=intern)
                if(intern) writeLines(c("###","ADMB output",paste("key =",key),as.character(Sys.time()),
                                        "###"," ",ADMBoutput), con = 'ADMBoutput.txt')

                # test again
                # rerun if hessian doesn't invert
                if(!file.exists("covar.sso")){
                #if(!is.na(repfilesize) & repfilesize>0){
                  cat("run was good, non-empty report file created: ",repfilename,"\n",sep="")
                  file.copy("covar.sso",paste(masterpath,covarname,sep="/"),overwrite=TRUE)
                }else{
                  if(file.exists("covar.sso")) file.remove("covar.sso")
                  cat("empty report file (due to bad hessian):",repfilename,"\n  rerunning with -nohess")
                  ADMBoutput <- system(paste(exe,fitextras,"-nohess"),intern=intern)
                  if(intern) writeLines(c("###","ADMB output from run with -nohess",paste("key =",key),as.character(Sys.time()),
                                          "###"," ",ADMBoutput), con = 'ADMBoutput.txt')
                }
                # rename files for current fit
                file.copy("Report.sso",repmaster,overwrite=TRUE)
                file.copy("CompReport.sso",paste(masterpath,compfilename,sep="/" ),overwrite=TRUE)
              }
            } # end if files exist but skipfiles
            repmastersize <- file.info(repmaster)$size
            if(!is.na(repmastersize) & repmastersize>0){
              if(length(grep("Temporary report file",readLines(repmaster,n=1)))>0){
                if(verbose) cat("end of run, but temporary file still exists. Deleting",repmaster,"\n")
                file.remove(repmaster)
              }
            }

            # fill in or create a data frame to store notes on model runs
            if(exists("fitnotes")) fitnotes[nrow(fitnotes)+1,] <- data.frame(ifit, isimchoice, ifitchoice, key, ibiasadj, Sys.time(), stringsAsFactors=FALSE)
            else fitnotes <- data.frame(fit=ifit, simchoice=isimchoice, fitchoice=ifitchoice, key=key, fitbiasramp=ibiasadj, time=Sys.time(), stringsAsFactors=FALSE)

            # save runs completed so far
            if(printfile) write.csv(fitnotes,fitnotesfile)
          } # end loop over fitbiasadj options
        } # end ifitchoice
      } # end isimchoice
    } # end ifit loop
    setwd(homepath) # reset working directory
    if(exists("fitnotes")) notes$fitnotes <- fitnotes else notes$fitnotes <- NA
  } # end if fit==TRUE

  #################################################
  return(invisible(notes))
} # end of function
