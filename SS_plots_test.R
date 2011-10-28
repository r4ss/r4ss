SS_plots_test <-
  function(
    replist=NULL, plot=1:27, print=0, pdf=FALSE, printfolder="plots", dir="default", fleets="all", areas="all",
    fleetnames="default", fleetcols="default", fleetlty=1, fleetpch=1, lwd=1, areacols="default", areanames="default",
    verbose=TRUE, uncertainty=TRUE, forecastplot=FALSE, datplot=FALSE, Natageplot=TRUE, samplesizeplots=TRUE, compresidplots=TRUE,
    sprtarg="default", btarg="default", minbthresh="default", pntscalar=2.6, minnbubble=8, aalyear=-1, aalbin=-1,
    aalresids=FALSE, maxneff=5000, cohortlines=c(), smooth=TRUE, showsampsize=TRUE, showeffN=TRUE, showlegend=TRUE,
    pwidth=7, pheight=7, punits="in", ptsize=12, res=300, cex.main=1,selexlines=1:5,
    rows=1, cols=1, maxrows=6, maxcols=6, maxrows2=2, maxcols2=4, tagrows=3, tagcols=3, fixdims=TRUE, new=TRUE,
    SSplotDatMargin=8,
    catchasnumbers=FALSE,legendloc="topleft", minyr=NULL, maxyr=NULL, scalebins=FALSE, ...)
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
  # Required other functions in r4ss package
  # Credit:  Based loosely on an early version of "Scape" (A. Magnusson) and "Output viewer" (R. Methot)
  #
  ################################################################################

  codedate <- "Oct 26, 2011"

  if(verbose) cat("R function updated:",codedate,
    "\nCheck for new code and report problems at http://code.google.com/p/r4ss/\n")

  flush.console()

  # label table is a step toward internationalization of the code
  # in the future, this could be read from a file, or we could have multiple columns
  # in the table to choose from

  if(is.null(replist)) stop("The input 'replist' should refer to an R object created by the function 'SS_output'.")

  # get quantities from the big list
  nfleets     <- replist$nfleets
  nfishfleets <- replist$nfishfleets
  nareas      <- replist$nareas
  nseasons    <- replist$nseasons
  timeseries  <- replist$timeseries
  lbins       <- replist$lbins
  inputs      <- replist$inputs
  endyr       <- replist$endyr
  SS_version  <- replist$SS_version
  Run_time    <- replist$Run_time
  Files_used  <- replist$Files_used
  FleetNames  <- replist$FleetNames
  rmse_table <- replist$rmse_table
  comp_data_exists <- replist$comp_data_exists

  # check for internal consistency
  if(uncertainty==TRUE & inputs$covar==FALSE){
    stop("To use uncertainty=T, you need to have covar=T in the input to the SS_output function")
  }
  if(forecastplot==TRUE & inputs$forecast==FALSE){
    stop("To use forecastplot=T, you need to have forecast=T in the input to the SSoutput function")
  }
  if(forecastplot==TRUE & max(timeseries$Yr > endyr+1)==0){
    cat("Changeing 'forecastplot' input to FALSE because all years up to endyr+1 are included by default\n")
    forecastplot <- FALSE
  }

  # derived quantities
  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{
    if(length(intersect(fleets,1:nfleets))!=length(fleets)){
      return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
  }}
  if(areas[1]=="all"){
    areas <- 1:nareas
  }else{ if(length(intersect(areas,1:nareas))!=length(areas)){
      return("Input 'areas' should be 'all' or a vector of values between 1 and nareas.")
  }}

  if(verbose) cat("Finished defining objects\n")

  ## if(nareas>1){
  ##   cat("! Warning: some plots are not configured for mult-area models (nareas=",nareas,")\n",sep="")
  ##   if(areanames[1]=="default") areanames <- paste("area",1:nareas)
  ## }

  if(fleetnames[1]=="default") fleetnames <- FleetNames
  if(fleetcols[1]=="default"){
    fleetcols <- rich.colors.short(nfishfleets)
    if(nfishfleets > 2) fleetcols <- rich.colors.short(nfishfleets+1)[-1]
  }
  if(length(fleetlty)<nfishfleets) fleetlty <- rep(fleetlty,nfishfleets)
  if(length(fleetpch)<nfishfleets) fleetpch <- rep(fleetpch,nfishfleets)
  if(areacols[1]=="default"){
    areacols  <- rich.colors.short(nareas)
    if(nareas > 2) areacols <- rich.colors.short(nareas+1)[-1]
  }


  #### prepare for plotting
  # make plot window (operating system specific)
  nplots <- length(intersect(1:50,plot))
  nprints <- length(intersect(1:50,print))

  OS <- "Mac" # don't know the version$os info for Mac
  if(length(grep("linux",version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw",version$os)) > 0) OS <- "Windows"

  pdf <- FALSE
  if(nprints>0 & pdf){
    stop("can't have pdf=T and print!=0: use print only or pdf & plot inputs")
  }
  if(nplots>0 & !pdf & new){
    if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    if(OS=="Windows") windows(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
    if(OS=="Linux") X11(width=pwidth,height=pheight,pointsize=ptsize)
    if(OS=="Mac") quartz(width=pwidth,height=pheight,pointsize=ptsize)
  }
  if(nplots>0 & !new){
    if(verbose) cat("Adding plots to existing plot window. Plot history not erased.\n")
  }
  if(dir=="default") dir <- inputs$dir
  plotdir <- paste(dir,"/",printfolder,"/",sep="")
  if(nprints>0){
    dir.create(dir,showWarnings=FALSE)
    dir.create(plotdir,showWarnings=FALSE)
    if(nprints>0 & verbose) cat("Plots specified by 'print' will be written to",plotdir,"\n")
  }

  plotInfoTable <- NULL
  if(pdf){
    if(dir=="default") dir <- inputs$dir
    dir.create(dir,showWarnings=FALSE)
    pdffile <- paste(dir,"/SS_plots_",format(Sys.time(),'%d-%b-%Y_%H.%M' ),".pdf",sep="")
    pdf(file=pdffile,width=pwidth,height=pheight)
    if(verbose) cat("PDF file with plots will be:",pdffile,'\n')
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

  ##########################################
  # Biology plots (mean weight, maturity, fecundity, spawning output)
  # and Time-varying growth
  #
  igroup <- 1
  if(igroup %in% c(plot, print) | length(cohortlines)>0)
  {
    if(verbose) cat("Starting biology plots (group ",igroup,")\n",sep="")
    plotinfo <- SSplotBiology(replist=replist,
                              plot=(igroup %in% plot),print=(igroup %in% print),
                              pwidth=pwidth, pheight=pheight, punits=punits,
                              ptsize=ptsize, res=res, cex.main=cex.main,
                              plotdir=plotdir)
    if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
    #if(verbose) cat("Finished biology plots\n")
  }

  ##########################################
  # Selectivity and retention plots
  #
  igroup <- 2
  if(igroup %in% c(plot, print)){
    if(verbose) cat("Starting selectivity and retention plots (group ",igroup,")\n",sep="")
    selexinfo <-
      SSplotSelex(replist=replist, selexlines=selexlines,
                  fleets=fleets, fleetnames=fleetnames,
                  plot=(igroup %in% plot), print=(igroup %in% print),
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res, cex.main=cex.main,
                  plotdir=plotdir)
    plotinfo <- selexinfo$plotinfo
    if(!is.null(plotinfo))
      plotInfoTable <- rbind(plotInfoTable,plotinfo)
  } # end if igroup in plot or print

  ##########################################
  # Basic time series
  #
  igroup <- 3
  if(igroup %in% c(plot, print))
  {
    if(verbose) cat("Starting timeseries plots (group ",igroup,")\n",sep="")
    for(isubplot in 1:15){ # which of 12 subplots to make
      for(doforecast in unique(c(FALSE,forecastplot))){ # add forecast or not
        if(isubplot %in% c(7,9,11)){
          for(douncertainty in unique(c(FALSE,uncertainty))){ # add uncertainty or not
            plotinfo <-
              SSplotTimeseries(replist=replist,
                               subplot=isubplot,
                               areas=areas,
                               areacols=areacols,
                               areanames=areanames,
                               forecast=doforecast,
                               uncertainty=douncertainty,
                               plot=(igroup %in% plot),
                               print=(igroup %in% print),
                               verbose=verbose,
                               btarg=btarg,
                               minbthresh=minbthresh,
                               minyr=minyr,maxyr=maxyr,
                               pwidth=pwidth, pheight=pheight, punits=punits,
                               ptsize=ptsize, res=res, cex.main=cex.main,
                               plotdir=plotdir)
            if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
          } # end loop over uncertainty or not
        }else{ # these plots don't have the option for uncertainty
          plotinfo <-
            SSplotTimeseries(replist=replist,
                             subplot=isubplot,
                             areas=areas,
                             areacols=areacols,
                             areanames=areanames,
                             forecast=doforecast,
                             uncertainty=FALSE,
                             plot=(igroup %in% plot),
                             print=(igroup %in% print),
                             verbose=verbose,
                             btarg=btarg,
                             minbthresh=minbthresh,
                             minyr=minyr,maxyr=maxyr,
                             pwidth=pwidth, pheight=pheight, punits=punits,
                             ptsize=ptsize, res=res, cex.main=cex.main,
                             plotdir=plotdir)
          if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
        }
      }
    } # end loop over timeseries subplots
  } # end if igroup in plot or print

  ##########################################
  # Recruitment deviation plots
  #
  igroup <- 4
  if(igroup %in% c(plot, print)){
    if(verbose) cat("Starting recruitment deviation plots (group ",igroup,")\n",sep="")
    plotinfo <-
      SSplotRecdevs(replist=replist,
                    plot=(igroup %in% plot),
                    print=(igroup %in% print),
                    forecastplot=forecastplot,
                    uncertainty=uncertainty,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,cex.main=cex.main,
                    plotdir=plotdir)
    if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)

    if(nareas>1 & nseasons>1){
      plotinfo <-
        SSplotRecdist(replist=replist,
                      plot=(igroup %in% plot),
                      print=(igroup %in% print),
                      verbose=verbose,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,cex.main=cex.main,
                      plotdir=plotdir)
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
    }
  } # end if igroup in plot or print

  ##########################################
  # Estimating recruitment bias adjustment plots
  #
  igroup <- 5
  if(igroup %in% c(plot, print)){
    if(uncertainty){
      if(verbose) cat("Starting estimation of recruitment bias adjustment and associated plots (group ",igroup,")\n",sep="")
      if(max(rmse_table$RMSE)>0){
        temp <-
          SS_fitbiasramp(replist=replist,
                         plot=(igroup %in% plot),
                         print=(igroup %in% print),
                         twoplots=FALSE,
                         pwidth=pwidth, pheight=pheight, punits=punits,
                         ptsize=ptsize, res=res,cex.main=cex.main,
                         plotdir=plotdir)
        plotinfo <- temp$plotinfo
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
      }else{
        cat("Skipping bias adjustment fit because root mean squared error of recruit devs is 0.\n")
      }
    }else{
      if(verbose) cat("Skipping estimation of recruitment bias adjustment (group ",igroup,") because uncertainty=FALSE\n",sep="")
    }
  } # end if igroup in plot or print

  ##########################################
  # spawner-recruit curve
  #
  igroup <- 6
  if(igroup %in% c(plot, print)){
    if(verbose) cat("Starting spawner-recruit curve plot (group ",igroup,")\n",sep="")
    plotinfo <-
      SSplotSpawnrecruit(replist=replist,
                         plot=(igroup %in% plot),
                         print=(igroup %in% print),
                         virg=TRUE,  # add point on curve at equilibrium values (B0,R0)
                         init=FALSE, # add point on curve at initial values (B1,R1)
                         pwidth=pwidth, pheight=pheight, punits=punits,
                         ptsize=ptsize, res=res,cex.main=cex.main,
                         plotdir=plotdir)
    if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
  } # end if igroup in plot or print
  
  ##########################################
  # time series of catch
  #
  igroup <- 7
  if(igroup %in% c(plot, print))
  {
    if(verbose) cat("Starting catch plots (group ",igroup,")\n",sep="")
    temp <-
      SSplotCatch(replist=replist,
                  plot=(igroup %in% plot),print=(igroup %in% print),
                  fleetnames=fleetnames,
                  fleetlty=fleetlty,
                  fleetpch=fleetpch,
                  fleetcols=fleetcols,
                  minyr=minyr,maxyr=maxyr,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,cex.main=cex.main,
                  plotdir=plotdir)
    plotinfo <- temp$plotinfo
    if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
  } # end if igroup in plot or print

  ##########################################
  # SPR and fishing intensity plots
  #
  igroup <- 8
  if(igroup %in% c(plot, print)){
    if(verbose) cat("Starting SPR plots (group ",igroup,")\n",sep="")
    plotinfo <-
      SSplotSPR(replist=replist,
                plot=(igroup %in% plot),
                print=(igroup %in% print),
                uncertainty=uncertainty,
                sprtarg=sprtarg, btarg=btarg,
                pwidth=pwidth, pheight=pheight, punits=punits,
                ptsize=ptsize, res=res,cex.main=cex.main,
                plotdir=plotdir)
    if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
  } # end if igroup in plot or print

  ##########################################
  # discard fit plots (if present)
  #
  igroup <- 9
  if(igroup %in% c(plot, print)){
    if(!is.na(replist$discard) && nrow(replist$discard)>0){
      if(verbose) cat("Starting discard plot (group ",igroup,")\n",sep="")
      plotinfo <-
        SSplotDiscard(replist=replist,
                      plot=(igroup %in% plot),
                      print=(igroup %in% print),
                      fleets=fleets,
                      fleetnames=fleetnames,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,cex.main=cex.main,
                      plotdir=plotdir)
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
    }else{
      if(verbose) cat("Skipping discard plot (group ",igroup,") because no discard data\n",sep="")
    }
  } # end if igroup in plot or print

  ##########################################
  # mean body weight (if present)
  #
  igroup <- 10
  if(igroup %in% c(plot, print)){
    if(!is.na(replist$mnwgt) && nrow(replist$mnwgt)>0){
      if(verbose) cat("Starting mean body weight plot (group ",igroup,")\n",sep="")
      plotinfo <-
        SSplotMnwt(replist=replist,
                   plot=(igroup %in% plot),
                   print=(igroup %in% print),
                   fleets=fleets,
                   fleetnames=fleetnames,
                   pwidth=pwidth, pheight=pheight, punits=punits,
                   ptsize=ptsize, res=res,cex.main=cex.main,
                   plotdir=plotdir)
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
    }else{
      if(verbose) cat("Skipping mean weight plot (group ",igroup,") because no mean weight data\n",sep="")
    }
  } # end if igroup in plot or print


  ##########################################
  # Index plots
  #
  igroup <- 11
  if(igroup %in% c(plot, print)){
    if(!is.null(dim(replist$cpue))){
      if(verbose) cat("Starting index plots (group ",igroup,")\n",sep="")
      plotinfo <- SSplotIndices(replist=replist,
                                fleets=fleets,
                                fleetnames=fleetnames,
                                plot=(igroup %in% plot),
                                print=(igroup %in% print),
                                datplot=datplot,
                                pwidth=pwidth, pheight=pheight, punits=punits,
                                ptsize=ptsize, res=res,cex.main=cex.main,
                                plotdir=plotdir)
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
    }else{
      if(verbose) cat("Skipping index plots (group ",igroup,") because no indices in model\n",sep="")
    }
  } # end if igroup in plot or print

  ##########################################
  # Numbers at age plots
  #
  igroup <- 12
  if(igroup %in% c(plot, print)){
    if(verbose) cat("Starting numbers at age plots (group ",igroup,")\n",sep="")
    plotinfo <- 
      SSplotNumbers(replist=replist,
                    areas=areas,
                    areanames=areanames,
                    areacols=areacols,
                    pntscalar=pntscalar,
                    plot=(igroup %in% plot),
                    print=(igroup %in% print),
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,cex.main=cex.main,
                    plotdir=plotdir)
    if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
  } # end if igroup in plot or print

  ##########################################
  # Composition data plots
  #
  # use of SSplotcomps function to make composition plots
  if(is.null(comp_data_exists) || !comp_data_exists){
    cat("No composition data, skipping all composition plots\n")
  }else{
    lenCompDatGroup <- 13
    ageCompDatGroup <- 14
    condCompDatGroup <- 15
    if(!datplot)
    {
      if(length(intersect(c(lenCompDatGroup, ageCompDatGroup, condCompDatGroup),
                          c(plot,print)))>0)
        cat("Skipping plot groups ",lenCompDatGroup,"-",condCompDatGroup," (comp data without fit) because input 'datplot=F'\n",sep="")
    }else{
      if(lenCompDatGroup %in% c(plot,print))  # data only aspects
      {
        if(verbose) cat("Starting length comp data plots (group ",lenCompDatGroup,")\n",sep="")
        # length comp bar plot
        plotinfo <-
          SSplotComps(replist=replist,datonly=TRUE,kind="LEN",bub=TRUE,verbose=verbose,fleets=fleets,
                      fleetnames=fleetnames,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                      print=(lenCompDatGroup%in%print),
                      plot=(lenCompDatGroup%in%plot),plotdir=plotdir,cex.main=cex.main,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
        # size comp bubble plot
        for(sizemethod in sort(unique(replist$sizedbase$method))){
          plotinfo <-
            SSplotComps(replist=replist,datonly=TRUE,kind="SIZE",sizemethod=sizemethod,
                        bub=TRUE,verbose=verbose,fleets=fleets,fleetnames=fleetnames,
                        samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                        minnbubble=minnbubble, pntscalar=pntscalar,
                        maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                        print=(lenCompDatGroup%in%print),
                        plot=(lenCompDatGroup%in%plot),plotdir=plotdir,cex.main=cex.main,
                        scalebins=scalebins,
                        pwidth=pwidth, pheight=pheight, punits=punits,
                        ptsize=ptsize, res=res,
                        ...)
          if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
        }
      }
      if(ageCompDatGroup %in% c(plot,print)){
        if(verbose) cat("Starting age comp data plots (group ",ageCompDatGroup,")\n",sep="")
        # age comp bar plot
        plotinfo <-
          SSplotComps(replist=replist,datonly=TRUE,kind="AGE",bub=FALSE,verbose=verbose,fleets=fleets,
                      fleetnames=fleetnames,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                      print=(ageCompDatGroup%in%print),
                      plot=(ageCompDatGroup%in%plot),plotdir=plotdir,cex.main=cex.main,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
        # age comp bubble plot
        plotinfo <-
          SSplotComps(replist=replist,datonly=TRUE,kind="AGE",bub=TRUE,verbose=verbose,fleets=fleets,
                      fleetnames=fleetnames,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                      print=(ageCompDatGroup%in%print),
                      plot=(ageCompDatGroup%in%plot),plotdir=plotdir,cex.main=cex.main,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
        # ghost age comp bar plot
        plotinfo <-
          SSplotComps(replist=replist,datonly=TRUE,kind="GSTAGE",bub=FALSE,verbose=verbose,fleets=fleets,
                      fleetnames=fleetnames,
                      samplesizeplots=samplesizeplots,showsampsize=FALSE,showeffN=FALSE,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                      print=(ageCompDatGroup%in%print),
                      plot=(ageCompDatGroup%in%plot),plotdir=plotdir,cex.main=cex.main,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
        # ghost age comp bubble plot
        plotinfo <-
          SSplotComps(replist=replist,datonly=TRUE,kind="GSTAGE",bub=TRUE,verbose=verbose,fleets=fleets,
                      fleetnames=fleetnames,
                      samplesizeplots=samplesizeplots,showsampsize=FALSE,showeffN=FALSE,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                      print=(ageCompDatGroup%in%print),
                      plot=(ageCompDatGroup%in%plot),plotdir=plotdir,cex.main=cex.main,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
        flush.console()
      }
      if(condCompDatGroup %in% c(plot,print)){
        if(verbose) cat("Starting conditional comp data plots (group ",condCompDatGroup,")\n",sep="")
        # conditional age plot
        plotinfo <-
          SSplotComps(replist=replist,datonly=TRUE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                      fleetnames=fleetnames,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,
                      fixdims=fixdims,rows=rows,cols=cols,
                      print=(condCompDatGroup%in%print),plot=(condCompDatGroup%in%plot),plotdir=plotdir,cex.main=cex.main,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
        if(!is.null(plotInfoTable))
          plotInfoTable$category[plotInfoTable$category=="Comp"] <- "CompDat"
        flush.console()
      } # end conditional data plots
    } # end if data plot

    ##########################################
    # Length comp fits
    #
    igroup <- 16
    if(igroup %in% c(plot, print)){
      if(verbose) cat("Starting fit to length comp plots (group ",igroup,")\n",sep="")
      plotinfo <- 
        SSplotComps(replist=replist,datonly=FALSE,kind="LEN",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)

      plotinfo <-
        SSplotComps(replist=replist,datonly=FALSE,kind="GSTLEN",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)

      # loop over size methods for generalized size comp data
      for(sizemethod in sort(unique(replist$sizedbase$method))){
        plotinfo <-
          SSplotComps(replist=replist,datonly=FALSE,kind="SIZE",sizemethod=sizemethod,
                      bub=TRUE,verbose=verbose,fleets=fleets, fleetnames=fleetnames,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                      print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                      maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
      }
      if(!is.null(plotInfoTable))
        plotInfoTable$category[plotInfoTable$category=="Comp"] <- "LenComp"
    }

    ##########################################
    # Age comp fits
    #
    igroup <- 17
    if(igroup %in% c(plot, print)){
      if(verbose) cat("Starting fit to age comp plots (group ",igroup,")\n",sep="")
      plotinfo <- 
        SSplotComps(replist=replist,datonly=FALSE,kind="AGE",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
      plotinfo <- 
        SSplotComps(replist=replist,datonly=FALSE,kind="GSTAGE",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
      if(!is.null(plotInfoTable))
        plotInfoTable$category[plotInfoTable$category=="Comp"] <- "AgeComp"
    } # end if igroup in plot or print

    ##########################################
    # Conditional age-at-length comp fits
    #
    igroup <- 18
    if(igroup %in% c(plot, print)){
      if(verbose) cat("Starting fit to conditional age-at-length comp plots (group ",igroup,")\n",sep="")
      if(aalresids==TRUE){
        plotinfo <- 
          SSplotComps(replist=replist,subplot=3,datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                      fleetnames=fleetnames,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,rows=rows,cols=cols,
                      print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                      maxneff=maxneff,cex.main=cex.main,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
      }
      # conditional age at length for a given year
      if(length(intersect(aalyear, unique(timeseries$Yr)))>0){
        plotinfo <- 
          SSplotComps(replist=replist,subplot=4:5,datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                      fleetnames=fleetnames,
                      aalbin=aalbin,aalyear=aalyear,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,rows=rows,cols=cols,
                      print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                      maxneff=maxneff,cex.main=cex.main,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
      }
      # conditional age at length for a given length bin
      if(length(intersect(aalbin, unique(lbins)))>0){
        plotinfo <- 
          SSplotComps(replist=replist,subplot=6,datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                      fleetnames=fleetnames,
                      aalbin=aalbin,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,rows=rows,cols=cols,
                      print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                      maxneff=maxneff,cex.main=cex.main,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
      }
      if(!is.null(plotInfoTable))
        plotInfoTable$category[plotInfoTable$category=="Comp"] <- "A@LComp"
    } #end if igroup in plot or print

    ##########################################
    # Andre's conditional age-at-length comp fits
    #
    igroup <- 19
    if(igroup %in% c(plot, print)){
      if(nrow(replist$condbase)>0 & verbose){
        if(verbose){
          cat("Starting Andre's new conditional age-at-length plots (group ",igroup,")\n",
              "  This plot shows mean age and std. dev. in conditional A@L.\n",
              "    Left plots are mean A@L by size-class (obs. and pred.)\n",
              "    with 90% CIs based on adding 1.64 SE of mean to the data.\n",
              "    Right plots in each pair are SE of mean A@L (obs. and pred.)\n",
              "    with 90% CIs based on the chi-square distribution.\n")
        }
        plotinfo <-
          SSplotComps(replist=replist,subplot=8,datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                      fleetnames=fleetnames,
                      aalbin=aalbin,aalyear=aalyear,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,rows=rows,cols=cols,
                      print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                      maxneff=maxneff,cex.main=cex.main,
                      scalebins=FALSE,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
        if(!is.null(plotInfoTable))
          plotInfoTable$category[plotInfoTable$category=="Comp"] <- "A@LComp"
      }else{
        if(verbose) cat("Skipping Andre's conditioanal A@L plots (group ",igroup,") because no such data in model\n",sep="")
      }
    } # end if igroup in plot or print
    
    ##########################################
    # Mean length-at-age and mean weight-at-age plots
    #
    igroup <- 20
    if(igroup %in% c(plot, print)){
      if(verbose) cat("Starting mean length-at-age and mean weight-at-age plots (group ",igroup,")\n",sep="")
      plotinfo <- 
        SSplotComps(replist=replist,datonly=FALSE,kind="L@A",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
      plotinfo <- 
        SSplotComps(replist=replist,datonly=FALSE,kind="W@A",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(igroup%in%print),plot=(igroup%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
    } # end if length-at-age and weight-at-age comps in plot or print
    if(!is.null(plotInfoTable))
      plotInfoTable$category[plotInfoTable$category=="Comp"] <- "Mean@A"

    # restore default single panel settings if needed
    # conditional because if adding to existing plot may mess up layout
    if(any(par()$mfcol != c(rows,cols))) par(mfcol=c(rows,cols))
    if(any(par()$mar != c(5,4,4,2)+.1)) par(mar=c(5,4,4,2)+.1, oma=rep(0,4))

    ##########################################
    # Tag plots
    #
    igroup <- 21
    if(igroup %in% c(plot, print)){
      if(is.null(replist$tagdbase2) || nrow(replist$tagdbase2)==0){
        if(verbose) cat("Skipping tag plots (group ",igroup,") because no tag data in model\n",sep="")
      }else{
        if(verbose) cat("Starting tag plots (group ",igroup,")\n",sep="")
        plotinfo <- 
          SSplotTags(replist=replist,
                     rows=rows,cols=cols,
                     tagrows=tagrows,tagcols=tagcols,
                     pntscalar=pntscalar,minnbubble=minnbubble,
                     plot=(igroup %in% plot),
                     print=(igroup %in% print),
                     pwidth=pwidth, pheight=pheight, punits=punits,
                     ptsize=ptsize, res=res, cex.main=cex.main,
                     plotdir=plotdir)
        if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
      } # end if data present
    } # end if igroup in plot or print
  } # end if comp data
  
  ##########################################
  # Yield plots
  #
  igroup <- 22
  if(igroup %in% c(plot, print)){
    if(verbose) cat("Starting yield plots (group ",igroup,")\n",sep="")
    plotinfo <- 
      SSplotYield(replist=replist,
                  plot=(igroup %in% plot),
                  print=(igroup %in% print),
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res, cex.main=cex.main,
                  plotdir=plotdir)
    if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
  } # end if igroup in plot or print


  ##########################################
  # Movement rate plots
  #
  igroup <- 23
  if(igroup %in% c(plot, print)){
    if(nrow(replist$movement)>0){
      if(verbose) cat("Starting movement rate plots (group ",igroup,")\n",sep="")
      plotinfo <- NULL
      temp <- 
        SSplotMovementRates(replist=replist,
                            plot=(igroup %in% plot),
                            print=(igroup %in% print),
                            pwidth=pwidth, pheight=pheight, punits=punits,
                            ptsize=ptsize, res=res, cex.main=cex.main,
                            plotdir=plotdir)
      plotinfo <- temp$plotinfo
      if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
    }else{
      if(verbose) cat("Skipping movement plots (group ",igroup,") because no movement in model\n",sep="")
    } # end if movement included in model
  } # end if igroup in plot or print

  ##########################################
  # Data range plots
  #
  igroup <- 24
  if(igroup %in% c(plot, print)){
    if(verbose) cat("Starting data range plots (group ",igroup,")\n",sep="")
    temp <- 
      SSplotData(replist=replist,
                 plot=(igroup %in% plot),
                 print=(igroup %in% print),
                 pwidth=pwidth, pheight=pheight, punits=punits,
                 ptsize=ptsize, res=res, cex.main=cex.main,
                 plotdir=plotdir, margins=c(5.1,2.1,4.1,SSplotDatMargin),
                 fleetnames=fleetnames)
    if(!is.null(temp) & length(temp)>0) plotinfo <- temp$plotinfo
    if(!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable,plotinfo)
  } # end if igroup in plot or print

  if(pdf) dev.off() # close PDF file if it was open
  if(verbose) cat("Finished all requested plots in SS_plots function\n")
  ### end of SS_plots function
  if(!is.null(plotInfoTable)){
    plotInfoTable$file <- as.character(plotInfoTable$file)
    plotInfoTable$caption <- as.character(plotInfoTable$caption)
    write.csv(plotInfoTable, paste(plotdir,"/plotInfoTable.csv",sep=""),row.names=FALSE)
    return(invisible(plotInfoTable))
  }else{
    return(invisible(999))
  }
}


