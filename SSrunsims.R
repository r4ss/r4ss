SSrunsims <-
function(sims=1,newrun=TRUE,sim=FALSE,fit=FALSE,
         simfolder="sims",
         fitfolder="fits",
         masterfolder="fits",
         MLEdata=FALSE,skipfiles=TRUE,
         simchoices=1,fitchoices=1,samedatafile=FALSE,
         homepath="c:/myfiles/",
         recdevmatrix=NULL,
         fitbiasramp=FALSE,
         exe="ss3_opt",
         simextras="-nox -nohess",
         fitextras="-nox -gbs 1000000000 -cbs 1000000000",
         fyr=NULL, lyr=NULL,
         verbose=TRUE)
{
  # function for running simulation and estimation models

  print("running SSrunsims",quote=FALSE)
  starttime <- Sys.time()
  simnotesfile <- paste(homepath,"/simnotes_",format(starttime,'%d-%b-%Y_%H.%M' ),".csv",sep="")
  fitnotesfile <- paste(homepath,"/fitnotes_",format(starttime,'%d-%b-%Y_%H.%M' ),".csv",sep="")

  notes <- list() # list to store notes on model runs

  # do some setup stuff (no reason not to repeat each time, but could be skipped)
  if(newrun){
    # setup home directory based on computer being used
    if(.Platform$OS.type=="windows") win <- T  else  win <- F
    if(substring(homepath,nchar(homepath)) %in% c("/","\\")) homepath <- substring(homepath,1,nchar(homepath)-1)
    if(is.na(file.info(homepath)$isdir)){
        print(paste("need to create directory:",homepath),quote=FALSE)
        print("  containing master control and data files",quote=FALSE)
        return()
    }
    if(verbose) print(paste("setting working directory to:",homepath),quote=FALSE)
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
    }

    simpath <- paste(homepath,simfolder,sep="/") # path for simulations
    fitpath <- paste(homepath,fitfolder,sep="/") # path for estimation

    if(is.na(file.info(simpath)$isdir) || !file.info(simpath)$isdir){
      # create folders to store simulations and estimations
      dir.create(path=simpath)
      dir.create(path=fitpath)
      print("put starter.ss, forecast.ss, and executable or batch file into the directories:",quote=FALSE)
      return()
    }

    # path to collect results if distributing over multiple simultaneous runs
    masterpath <- paste(homepath,masterfolder,sep="/")
  }
  #################################################

  # run simulation model
  if(sim){
    setwd(simpath)
    if(verbose) print(paste("running simulations in",simpath),quote=FALSE)
    for(isim in sims){
      for(isimchoice in simchoices){
        if(verbose) print(paste("in isimchoice=",LETTERS[isimchoice],sep=""),quote=FALSE)
        filekey <- paste("sim",LETTERS[isimchoice],sep="") # description of this sim
        key <- paste(filekey,isim,sep="") # key has index of simulation added
        if(verbose) print(paste("key = ",key,sep=""),quote=FALSE)

        bootdatfile <- paste("bootdat_",key,".ss",sep="")
        bootdatfilesize <- file.info(bootdatfile)$size
        if(!is.na(bootdatfilesize) & bootdatfilesize>0 & skipfiles==TRUE){
            # skip this run if a file with non-zero file size exists already
            print(paste("skipping ",bootdatfile," with size=",bootdatfilesize,sep=""),quote=FALSE)
        }else{
          ## run simulation model
          # copy control file
          ctl_sim <- paste("../ctl_",filekey,".ss",sep="")
          temp <- file.copy(ctl_sim,"ctl_isim_nodevs.ss",overwrite=TRUE)
          if(temp!=TRUE){
              print(paste("Error copying",ctl_sim),quote=FALSE)
              break()}
          # copy data file
          if(samedatafile){
              dat_sim <- "../dat_master.ss"
          }else{
              dat_sim <- paste("../dat_",filekey,".ss",sep="")
          }
          temp <- file.copy(dat_sim,"dat_isim.ss",overwrite=TRUE)
          if(temp!=TRUE){
              print(paste("Error copying",dat_sim),quote=FALSE)
              break()}

          # add recdevs to control files for simulations
          if(is.null(fyr) | is.null(lyr)){
            myctl <- SSreadctl("ctl_isim_nodevs.ss")
            fyr <- myctl$fyr_main_recdevs
            lyr <- myctl$lyr_main_recdevs
          }
          recdevs <- recmat[1:(lyr-fyr+1),isim]

          SS_recdevs(fyr=fyr, lyr=lyr, recdevs=recdevs,
                     rescale=TRUE, scaleyrs=NULL,
                     ctlfile="ctl_isim_nodevs.ss",
                     newctlfile="ctl_isim.ss",
                     verbose=TRUE, writectl=TRUE, returnctl=FALSE)

          # run simulation
          file.remove('covar.sso')
          system(paste(exe,simextras))

          # rename files
          file.copy("Report.sso",paste("Report_",key,".sso",sep=""),overwrite=TRUE)
          file.copy("CompReport.sso",paste("CompReport_",key,".sso",sep=""),overwrite=TRUE)

          # split apart simulation results
          SS_splitdat(inpath     = simpath,
                      outpath    = simpath,
                      inname     = "data.ss_new",
                      outpattern = paste("bootdat_",key,sep=""),
                      MLE        = MLEdata
                      )

          # fill in or create a data frame to store notes on model runs
          if(exists("simnotes")) simnotes[nrow(simnotes)+1,] <- data.frame(isim, isimchoice, key, Sys.time(), stringsAsFactors=FALSE)
          else simnotes <- data.frame(sim=isim, simchoice=isimchoice, key=key, time=Sys.time(), stringsAsFactors=FALSE)

          write.csv(simnotes,simnotesfile)
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
      for(isimchoice in simchoices){ # loop over simulation scenarios
        for(ifitchoice in fitchoices){ # loop over estimation scenarios
          for(ibiasadj in unique(c(FALSE,fitbiasramp))){ # loop over whether to apply the bias adjustment function

            # text description of particular case
            filekey <- paste("fit",LETTERS[ifitchoice],sep="")
            key <- paste("sim",LETTERS[isimchoice],"_fit",LETTERS[ifitchoice],ifit,sep="")

            # if fitbiasramp will be applied on some model runs
            # but NOT this one
            if(fitbiasramp & !ibiasadj) key <- paste(key,"_nobiasadj",sep="")
            if(verbose) print(paste("key = ",key,", max(sims) = ",max(sims),sep=""),quote=FALSE)

            # new names for output files
            repfilename <- paste("Report_",key,".sso",sep="")
            compfilename <- paste("CompReport_",key,".sso",sep="")
            covarname <- paste("covar_",key,".sso",sep="")
            repfilesize <- file.info(repfilename)$size

            ## run estimation model
            if(!is.na(repfilesize) & repfilesize>0 & skipfiles==TRUE){
              # skip this run if a file with non-zero file size exists already
              print(paste("skipping ",repfilename," with size=",repfilesize,sep=""),quote=FALSE)
            }else{
              # copy data file
              if(samedatafile){
                dat_fit <- "../dat_master.ss"
              }else{
                dat_fit <- paste("../dat_",filekey,".ss",sep="")
              }
              temp <- file.copy(dat_fit,"dat_ifit.ss",overwrite=TRUE)
              if(temp!=TRUE){
                print(paste("Error copying",dat_fit),quote=FALSE)
                break()}

              # copy control file
              ctl_fit <- paste("../ctl_",filekey,".ss",sep="")
              temp <- file.copy(ctl_fit,"ctl_ifit.ss",overwrite=TRUE)
              if(temp!=TRUE){
                print(paste("Error copying",ctl_fit),quote=FALSE)
                break()}

              # if fitbiasramp will be applied on some model runs
              # INCLUDING this one
              if(fitbiasramp & ibiasadj){
                # get output from previous run
                tempkey <- paste(key,"_nobiasadj",sep="")

                # new names for output files
                oldrepfilename <- paste("Report_",tempkey,".sso",sep="")
                oldcompfilename <- paste("CompReport_",tempkey,".sso",sep="")
                oldcovarname <- paste("covar_",tempkey,".sso",sep="")
                newctl <- paste("ctl_",key,".ss",sep="")

                if(verbose) print("applying bias adjustment based on previous model run",quote=FALSE)
                if(file.exists(oldcovarname)){
                  # if the covar file exists, then apply fitbiasramp function
                  replist <- SSoutput(dir=getwd(),model=exe,repfile=oldrepfilename,
                                      compfile=oldcompfilename,covarfile=oldcovarname,
                                      forecast=FALSE,printstats=FALSE,verbose=FALSE)
                  SS_fitbiasramp(replist,png=paste("fitbiasramp_",key,".png",sep=""),
                                 oldctl="ctl_ifit.ss",newctl=newctl)
                  file.copy(newctl,"ctl_ifit.ss",overwrite=TRUE)
                }else{
                  print(paste('run failed to converge: key:',tempkey),quote=FALSE)
                }
              } # end fit of bias adjustment on the fly

              # run this run
              if(is.na(repfilesize)){
                print(paste("no rep file, running to create ",repfilename," with size=",repfilesize,sep=""),quote=FALSE)
              }else{
                if(repfilesize==0){
                  print(paste("empty rep file, running to create ",repfilename," with size=",repfilesize,sep=""),quote=FALSE)
                }else{
                  print("rep file exists, deleting and replacing",quote=FALSE)
                  file.remove(repfilename)
                }
              }

              # get bootstrap data from simulations folder
              boot1 <- paste("../",simfolder,"/bootdat_sim",LETTERS[isimchoice],ifit,".ss",sep="")
              boot2 <- "dat_ifit.ss"

              print("info on bootstrap data file:",quote=FALSE)
              print(file.info(boot1))
              print(file.info(boot2))
              temp <- file.copy(boot1,boot2,overwrite=TRUE)
              if(temp!=TRUE){
                print("!error copying bootstrap data files: make sure simulations were run to create them",quote=FALSE)
                break()
              }

              file.remove('covar.sso')
              system(paste(exe,fitextras))

              # test again
              repfilesize <- file.info("Report.sso")$size

              # rerun if hessian doesn"t invert
              if(!is.na(repfilesize) & repfilesize>0){
                print(paste("run was good, non-empty report file exists: ",repfilename,sep=""),quote=FALSE)
                file.copy("covar.sso",paste(masterpath,covarname,sep="/"),overwrite=TRUE)
              }else{
                file.remove("covar.sso")
                print(paste("empty report file (due to bad hessian): ",repfilename,sep=""),quote=F)
                print("rerunning with -nohess",quote=F)
                system(paste(exe,fitextras,"-nohess"))
              }
              # rename files for current fit
              file.copy("Report.sso",paste(masterpath,repfilename,sep="/"),overwrite=TRUE)
              file.copy("CompReport.sso",paste(masterpath,compfilename,sep="/" ),overwrite=TRUE)
            } # end if files exist but skipfiles

            # fill in or create a data frame to store notes on model runs
            if(exists("fitnotes")) fitnotes[nrow(fitnotes)+1,] <- data.frame(ifit, isimchoice, ifitchoice, key, ibiasadj, Sys.time(), stringsAsFactors=FALSE)
            else fitnotes <- data.frame(fit=ifit, simchoice=isimchoice, fitchoice=ifitchoice, key=key, fitbiasramp=ibiasadj, time=Sys.time(), stringsAsFactors=FALSE)
            write.csv(fitnotes,fitnotesfile)
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
