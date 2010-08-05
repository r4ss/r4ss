SS_plots <-
  function(
    replist=ReportObject, plot=1:24, print=0, pdf=FALSE, printfolder="", dir="default", fleets="all", areas="all",
    fleetnames="default", fleetcols="default", fleetlty=1, fleetpch=1, lwd=1, areacols="default", areanames="default",
    verbose=TRUE, uncertainty=TRUE, forecastplot=FALSE, datplot=FALSE, Natageplot=TRUE, samplesizeplots=TRUE, compresidplots=TRUE,
    sprtarg=0.4, btarg=0.4, minbthresh=0.25, pntscalar=2.6, minnbubble=8, aalyear=-1, aalbin=-1, aalmaxbinrange=0,
    aalresids=FALSE, maxneff=5000, cohortlines=c(), smooth=TRUE, showsampsize=TRUE, showeffN=TRUE, showlegend=TRUE,
    pwidth=7, pheight=7, punits="in", ptsize=12, res=300, cex.main=1,selexlines=1:5,
    rows=1, cols=1, maxrows=6, maxcols=6, maxrows2=2, maxcols2=4, tagrows=3, tagcols=3, fixdims=TRUE, new=TRUE,
    catchasnumbers=FALSE,legendloc="topleft",...)

{
  ################################################################################
  #
  # SS_plots
  # This function comes with no warranty or guarantee of accuracy
  #
  # Purpose: A wrapper to call many plot functions which collectively
  #          sumarize the results of a Stock Synthesis model run.
  # Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
  #          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
  #          and other contributors to http://code.google.com/p/r4ss/
  # Returns: Plots with plot history in R GUI and/or .png files.
  # General: Updated for Stock Synthesis version 3.10; R version 2.8.1
  # Notes:   See users guide for documentation.
  # Required SS3v_output function and plotrix package
  # Credit:  Based loosely on an early version of "Scape" (A. Magnusson) and "Output viewer" (R. Methot)
  #
  ################################################################################

  codedate <- "June 14, 2010"

  if(verbose){
    print(paste("R function updated:",codedate),quote=FALSE)
    print("Check for new code and report problems at http://code.google.com/p/r4ss/",quote=FALSE)
  }

  flush.console()

  # label table is a step toward internationalization of the code
  # in the future, this could be read from a file, or we could have multiple columns
  # in the table to choose from


  # get quantities from the big list
  if(replist[[1]]=="ReportObject"){
    return("The input 'replist' should refer to an R object created by the function 'SSoutput'.")
  }

  nfleets     <- replist$nfleets
  nareas      <- replist$nareas
  timeseries  <- replist$timeseries
  inputs      <- replist$inputs
  endyr       <- replist$endyr
  SS_version  <- replist$SS_version
  Run_time    <- replist$Run_time
  Files_used    <- replist$Files_used

  # check for internal consistency
  if(uncertainty==TRUE & inputs$covar==FALSE){
    print("To use uncertainty=T, you need to have covar=T in the input to the SSoutput function",quote=FALSE)
    return()
  }
  if(forecastplot==TRUE & inputs$forecast==FALSE){
    print("To use forecastplot=T, you need to have forecast=T in the input to the SSoutput function",quote=FALSE)
    return()
  }
  if(forecastplot==TRUE & max(timeseries$Yr > endyr+1)==0){
    print("Changeing 'forecastplot' input to FALSE because all years up to endyr+1 are included by default",quote=FALSE)
    forecastplot <- FALSE
  }

  # derived quantities
  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{ if(length(intersect(fleets,1:nfleets))!=length(fleets)){
      return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
  }}
  if(areas[1]=="all"){
    areas <- 1:nareas
  }else{ if(length(intersect(areas,1:nareas))!=length(areas)){
      return("Input 'areas' should be 'all' or a vector of values between 1 and nareas.")
  }}

  if(verbose) print("Finished defining objects",quote=FALSE)
  if(nareas>1){
    print(paste("! Warning: some plots are not configured for mult-area models (nareas=",nareas,")",sep=""),quote=FALSE)
    if(areanames[1]=="default") areanames <- paste("area",1:nareas)
  }

  #### prepare for plotting
  # make plot window (operating system specific)
  nplots <- length(intersect(1:24,plot))
  nprints <- length(intersect(1:24,print))

  if(length(grep("linux",version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw",version$os)) > 0) OS <- "Windows"
  # need appropriate line to support Mac operating systems

  if(nprints>0 & pdf){
    print("can't have pdf=T and print!=0: use print only or pdf & plot inputs",quote=FALSE)
    return()
  }
  if(nplots>0 & !pdf & new){
    if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    if(OS=="Windows") windows(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
    if(OS=="Linux") X11(width=pwidth,height=pheight,pointsize=ptsize)
    if(OS=="Mac") quartz(width=pwidth,height=pheight,pointsize=ptsize)
    plotdir <- "No directory"
  }
  if(nplots>0 & !new){
    if(verbose) print("Adding plots to existing plot window. Plot history not erased.",quote=FALSE)
  }
  if(nprints>0){
    if(dir=="default") dir <- inputs$dir
    dir.create(dir,showWarnings=FALSE)
    plotdir <- paste(dir,printfolder,"/",sep="")
    dir.create(plotdir,showWarnings=FALSE)
    if(nprints>0 & verbose) print(paste("Plots specified by 'print' will be written to",plotdir),quote=FALSE)
  }
  if(pdf){
    if(dir=="default") dir <- inputs$dir
    dir.create(dir,showWarnings=FALSE)
    plotdir <- paste(dir,printfolder,"/",sep="")
    pdffile <- paste(inputs$dir,"/SS_plots_",format(Sys.time(),'%d-%b-%Y_%H.%M' ),".pdf",sep="")
    pdf(file=pdffile,width=pwidth,height=pheight)
    if(verbose) print(paste("PDF file with plots will be: ",pdffile,sep=""),quote=FALSE)
  }
  if(new) par(mfcol=c(rows,cols)) # make multi-panel plot if requested

  if(pdf){
    mar0 <- par()$mar # current margins
    par(mar=rep(0,4))
    plot(0,type="n",xlab="",ylab="",axes=FALSE,xlim=c(0,1),ylim=c(0,1))
    y <- 0.9
    ystep <- -.05
    text(0,y,"Plots created using the 'r4ss' package in R",pos=4)
    y <- y+ystep
    text(0,y,paste("Stock Synthesis version:",substr(SS_version,1,9)),pos=4)
    y <- y+ystep
    text(0,y,Run_time,pos=4)
    y <- y+ystep
    Files2 <- strsplit(Files_used," ")[[1]]
    text(0,y,paste(Files2[[1]],Files2[2]),pos=4)
    y <- y+ystep
    text(0,y,paste(Files2[[3]],Files2[4]),pos=4)
    par(mar=mar0) # replace margins
  }

  #### plots 1 (now contains 2 as well)
  # Static growth (mean weight, maturity, fecundity, spawning output)
  # and Time-varying growth
  if(1 %in% c(plot, print) | length(cohortlines)>0)
  {
    SSplotBiology(replist=replist,
                  plot=(1 %in% plot),print=(1 %in% print),
                  cex.main=cex.main)
    if(verbose) print("Finished plot 1: Biology (weight, maturity, spawning output, growth)",quote=FALSE)
  }

  ### plots 3 and 4 selectivity and retention
  # Length selex and retention
  if(length(intersect(c(3,4), c(plot,print)))>0)
  {
    SSplotSelex(replist=replist, selexlines=selexlines, fleets=fleets,
                plot=(3 %in% plot), print=(3 %in% print),
                cex.main=cex.main)
    if(verbose) print("Finished plots 3 and 4: selectivity plots",quote=FALSE)
  }


  ### plot 5: Basic time series (contains what used to be plot 7)
  # stats and dimensions
  if(5 %in% c(plot, print))
  {
    for(isubplot in 1:12){ # which of 12 subplots to make
      for(doforecast in unique(c(FALSE,forecastplot))){ # add forecast or not
        if(isubplot %in% c(7,9,11)){
          for(douncertainty in unique(c(FALSE,uncertainty))){ # add uncertainty or not
            SSplotTimeseries(replist=replist,
                             subplot=isubplot,
                             forecast=doforecast,
                             uncertainty=douncertainty,
                             plot=(5 %in% plot),
                             print=(5 %in% print),
                             verbose=verbose,
                             btarg=btarg, 
                             minbthresh=minbthresh)
          } # end loop over uncertainty or not
        }else{ # these plots don't have the option for uncertainty
            SSplotTimeseries(replist=replist,
                             subplot=isubplot,
                             forecast=doforecast,
                             uncertainty=FALSE,
                             plot=(5 %in% plot),
                             print=(5 %in% print),
                             verbose=verbose,
                             btarg=btarg, 
                             minbthresh=minbthresh)
        }
      }
    }
  } # end if 5 in plot or print

  if(6 %in% c(plot, print))
  {
    # time series of catch
    SSplotCatch(replist=replist,
                plot=(6 %in% plot),
                print=(6 %in% print),
                fleetnames=fleetnames)

    if(verbose) print("Finished plot 5: Basic time series",quote=FALSE)
  } # end if 6 in plot or print

  ### Plot 8: discard fractions (if present) ###
  if(8 %in% c(plot, print)){
    SSplotDiscard(replist=replist,
                  plot=(8 %in% plot),
                  print=(8 %in% print),
                  fleets=fleets,
                  fleetnames=fleetnames)
  } # end if 8 in plot or print

  ### Plot 9: mean body weight (if present) ###
  if(9 %in% c(plot, print)){
    SSplotMnwt(replist=replist,
               plot=(9 %in% plot),
               print=(9 %in% print),
               fleets=fleets,
               fleetnames=fleetnames)
  } # end if 9 in plot or print

  ### Plot 11: SPR and fishing intensity plots ###
  if(11 %in% c(plot, print)){
    SSplotSPR(replist=replist,
              plot=(11 %in% plot),
              print=(11 %in% print),
              uncertainty=uncertainty,
              sprtarg=0.4, btarg=0.4)
  } # end if 11 in plot or print

  ### Plot 7: recruitment (moved to near S-R curve, but needs renumbering) ###
  if(7 %in% c(plot, print)){
    SSplotRecdevs(replist=replist,
                  plot=(7 %in% plot),
                  print=(7 %in% print),
                  forecastplot=forecastplot,
                  uncertainty=uncertainty)
  } # end if 7 in plot or print

  ### Plot 25: estimating recruitment bias adjustment (probably needs renumbering) ###
  if(25 %in% c(plot, print)){
    SS_fitbiasramp(replist=replist,
                   png=(25 %in% print),
                   twoplots=FALSE)
  } # end if 25 in plot or print                   

  ### Plot 12: spawner-recruit curve ###
  if(12 %in% c(plot, print)){
    SSplotSpawnrecruit(replist=replist,
                       plot=(12 %in% plot),
                       print=(12 %in% print),
                       virg=TRUE,  # add point on curve at equilibrium values (B0,R0)
                       init=FALSE) # add point on curve at initial values (B1,R1)
  } # end if 12 in plot or print

  ### Plot 13: CPUE plots ###
  if(13 %in% c(plot, print))
  {
    SSplotIndices(replist=replist,
                  fleets=fleets,
                  fleetnames=fleetnames,
                  plot=(13 %in% plot),
                  print=(13 %in% print),
                  datplot=datplot)
  } # end if 13 in plot or print

  ### Plot 14: numbers at age ###
  if(14 %in% c(plot, print)){
    SSplotNumbers(replist=replist,
                  areas=areas,
                  areanames=areanames,
                  areacols=areacols,
                  pntscalar=pntscalar,
                  cex.main=cex.main,
                  plot=(14 %in% plot),
                  print=(14 %in% print))
  } # close if 14 in plot or print

  ### Plot 15: Composition data plots ###
  # use of SSplotcomps function to make composition plots
  if(!datplot)
  {
    if(length(intersect(15:17,c(plot,print)))>0)
      print("skipped data-only plots 15-17 (comp data without fit) because input 'datplot=F'",quote=FALSE)
  }else{
    if(15 %in% c(plot,print))  # data only aspects
    {
      # length comp bar plot
      SSplotComps(replist=replist,datonly=TRUE,kind="LEN",bub=FALSE,verbose=verbose,fleets=fleets,
                  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(15%in%print),plot=(15%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      # length comp bubble plot
      SSplotComps(replist=replist,datonly=TRUE,kind="LEN",bub=TRUE,verbose=verbose,fleets=fleets,
                  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(15%in%print),plot=(15%in%plot),plotdir=plotdir,cex.main=cex.main,cohortlines=cohortlines,...)
      # size comp bar plot
      SSplotComps(replist=replist,datonly=TRUE,kind="SIZE",bub=FALSE,verbose=verbose,fleets=fleets,
                  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(15%in%print),plot=(15%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      # size comp bubble plot
      SSplotComps(replist=replist,datonly=TRUE,kind="SIZE",bub=TRUE,verbose=verbose,fleets=fleets,
                  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(15%in%print),plot=(15%in%plot),plotdir=plotdir,cex.main=cex.main,cohortlines=cohortlines,...)
      if(verbose) print("Finished plot 15: length and size comp data",quote=FALSE)
      flush.console()
    }
    if(16 %in% c(plot,print))
    {
      # age comp bar plot
      SSplotComps(replist=replist,datonly=TRUE,kind="AGE",bub=FALSE,verbose=verbose,fleets=fleets,
                  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(16%in%print),plot=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      # age comp bubble plot
      SSplotComps(replist=replist,datonly=TRUE,kind="AGE",bub=TRUE,verbose=verbose,fleets=fleets,
                  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(16%in%print),plot=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      # ghost age comp bar plot
      SSplotComps(replist=replist,datonly=TRUE,kind="GSTAGE",bub=FALSE,verbose=verbose,fleets=fleets,
                  samplesizeplots=samplesizeplots,showsampsize=FALSE,showeffN=FALSE,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(16%in%print),plot=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      # ghost age comp bubble plot
      SSplotComps(replist=replist,datonly=TRUE,kind="GSTAGE",bub=TRUE,verbose=verbose,fleets=fleets,
                  samplesizeplots=samplesizeplots,showsampsize=FALSE,showeffN=FALSE,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(16%in%print),plot=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      if(verbose) print("Finished plot 16: age comp data",quote=FALSE)
      flush.console()
    }
    if(17 %in% c(plot,print))
    {
      # conditional age plot
      SSplotComps(replist=replist,datonly=TRUE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                      maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,
                      fixdims=fixdims,rows=rows,cols=cols,
                      print=(17%in%print),plot=(17%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      if(verbose) print("Finished plot 17: conditional age at length data",quote=FALSE)
      flush.console()
    }
  } # end if data plot

  # plot 18: length comp data with fits, sample size, etc.
  if(18 %in% c(plot,print)){
    SSplotComps(replist=replist,datonly=FALSE,kind="LEN",bub=TRUE,verbose=verbose,fleets=fleets,
                samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                print=(18%in%print),plot=(18%in%plot),smooth=smooth,plotdir=plotdir,
                maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,...)
    SSplotComps(replist=replist,datonly=FALSE,kind="SIZE",bub=TRUE,verbose=verbose,fleets=fleets,
                samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                print=(18%in%print),plot=(18%in%plot),smooth=smooth,plotdir=plotdir,
                maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,...)
    if(verbose) print("Finished plot 18: length and size comps with fits",quote=FALSE)
    flush.console()
  }

  # plot 19: age comp data with fits, sample size, etc.
  if(19 %in% c(plot,print)){
    SSplotComps(replist=replist,datonly=FALSE,kind="AGE",bub=TRUE,verbose=verbose,fleets=fleets,
                samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                print=(19%in%print),plot=(19%in%plot),smooth=smooth,plotdir=plotdir,
                maxneff=maxneff,cex.main=cex.main,...)
    SSplotComps(replist=replist,datonly=FALSE,kind="GSTAGE",bub=TRUE,verbose=verbose,fleets=fleets,
                samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                print=(19%in%print),plot=(19%in%plot),smooth=smooth,plotdir=plotdir,
                maxneff=maxneff,cex.main=cex.main,...)
    if(verbose) print("Finished plot 19: age comps with fits",quote=FALSE)
    flush.console()
  } # end if 19 in plot or print

  # plot 20: conditional age at length plot with fits, sample size, etc.
  if(20 %in% c(plot,print)){
    if(aalresids==TRUE){
      SSplotComps(replist=replist,subplot=3,datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                  aalbin=aalbin,aalyear=aalyear,
                  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                  maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,rows=rows,cols=cols,
                  print=(20%in%print),plot=(20%in%plot),smooth=smooth,plotdir=plotdir,
                  maxneff=maxneff,cex.main=cex.main,...)
      if(verbose) print("Finished plot 20a: conditional age at length with fits",quote=FALSE)
    }
    # more plot 20: Andre's new conditional age-at-length plots
    SSplotComps(replist=replist,subplot=8,datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                aalbin=aalbin,aalyear=aalyear,
                samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,rows=rows,cols=cols,
                print=(20%in%print),plot=(20%in%plot),smooth=smooth,plotdir=plotdir,
                maxneff=maxneff,cex.main=cex.main,...)
    if(verbose){
      print("Finished plot 20b: mean age and std. dev. in conditional AAL",quote=FALSE)
      print("  This is a new plot, currently in beta mode.",quote=FALSE)
      print("  Left plots are mean AAL by size-class (obs. and pred.)",quote=FALSE)
      print("  with 90% CIs based on adding 1.64 SE of mean to the data",quote=FALSE)
      print("  Right plots in each pair are SE of mean AAL (obs. and pred.)",quote=FALSE)
      print("  with 90% CIs based on the chi-square distribution.",quote=FALSE)
    }
  } # end if 20 in plot or print

  # plot 21: mean length at age and mean weight at age
  if(21 %in% c(plot,print)){
    SSplotComps(replist=replist,datonly=FALSE,kind="L@A",bub=TRUE,verbose=verbose,fleets=fleets,
                samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                print=(21%in%print),plot=(21%in%plot),smooth=smooth,plotdir=plotdir,
                maxneff=maxneff,cex.main=cex.main,...)
    SSplotComps(replist=replist,datonly=FALSE,kind="W@A",bub=TRUE,verbose=verbose,fleets=fleets,
                samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                print=(21%in%print),plot=(21%in%plot),smooth=smooth,plotdir=plotdir,
                maxneff=maxneff,cex.main=cex.main,...)
    if(verbose) print("Finished plot 21: mean length at age and mean weight at age",quote=FALSE)
    flush.console()
  } # end if 21 in plot or print


  # restore default single panel settings if needed
  # conditional because if adding to existing plot may mess up layout
  if(FALSE %in% (par()$mfcol == c(rows,cols))) par(mfcol=c(rows,cols))
  if(FALSE %in% (par()$mar == c(5,4,4,2)+.1)) par(mar=c(5,4,4,2)+.1, oma=rep(0,4))

  # Yield curve
  if(22 %in% c(plot, print)){
    SSplotYield(replist=replist,
                cex.main=cex.main,
                plot=(22 %in% plot),
                print=(22 %in% print))
  } # close plot section 22

  ### Plot 24: Tag plots ###
  if(24 %in% c(plot, print)){
    SSplotTags(replist=replist,
               rows=rows,cols=cols,
               tagrows=tagrows,tagcols=tagcols,
               cex.main=cex.main,
               pntscalar=pntscalar,minnbubble=minnbubble,
               plot=(24 %in% plot),
               print=(24 %in% print))
  } # end if 24 in plot or print

  if(pdf) dev.off() # close PDF file if it was open
  if(verbose) print("Finished all requested plots in SS_plots function",quote=FALSE)
  ### end of SS_plots function
}
