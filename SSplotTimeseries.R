SSplotTimeseries <-
  function(replist,subplot,add=FALSE,areas="all",areacols=1:10,areanames="default",
           forecastplot=TRUE,uncertainty=TRUE,bioscale="default",
           plot=TRUE,print=FALSE,plotdir="default",verbose=FALSE,
           btarg=0.4,minbthresh=0.25,xlab="Year",
           labels=c("Total biomass (mt)", #1
             "Total biomass at beginning of season 1 (mt)", #2
             "Summary biomass (mt)",      #3
             "Summary biomass at beginning of season 1 (mt)", #4
             "Spawning biomass (mt)",     #5
             "Spawning depletion",        #6
             "Spawning output (eggs)",    #7
             "Age-0 recruits (1,000s)"),  #8
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1)
{

  # individual function for plotting time series of total or summary biomass
  # subplot1 = total biomass total all areas
  # subplot2 = total biomass by area
  # subplot3 = total biomass in all areas in season 1
  # subplot4 = summary biomass total all areas
  # subplot5 = summary biomass by area
  # subplot6 = summary biomass in all areas in season 1
  # subplot7 = spawning biomass total (with or without uncertainty)
  # subplot8 = spawning biomass by area
  # subplot9 = spawning depletion total (with or without uncertainty)
  # subplot10 = spawning depletion by area
  # subplot11 = recruitment total (with or without uncertainty)
  # subplot12 = recruitment by area

  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  # get values from replist
  timeseries     <- replist$timeseries
  nseasons       <- replist$nseasons
  startyr        <- replist$startyr
  endyr          <- replist$endyr
  nsexes         <- replist$nsexes
  nareas         <- replist$nareas
  derived_quants <- replist$derived_quants
  FecPar2        <- replist$FecPar2

  # check if spawning output rather than spawning biomass is plotted
  if(plotdir=="default") plotdir <- replist$inputs$dir
  if(FecPar2!=0) labels[5] <- labels[7]

  # check area subsets
  if(areas[1]=="all"){
    areas <- 1:nareas
  }else{
    if(length(intersect(areas,1:nareas))!=length(areas))
      return("Input 'areas' should be 'all' or a vector of values between 1 and nareas.")
  }
  if(nareas>1 & areanames[1]=="default"){
    areanames <- paste("area",1:nareas)
  }

  #scaling factor for single sex models
  if(bioscale=="default"){
    bioscale <- 1
    if(nsexes==1) bioscale <- 0.5
  }
  # modifying data to subset for a single season
  ts <- timeseries
  ts$Yr <- ts$Yr + (ts$Seas-1)/nseasons

  # get spawning season
  spawnseas <- unique(ts$Seas[!is.na(ts$SpawnBio)])
  if(length(spawnseas) > 1) stop("more than one season has spawning biomass in TIME_SERIES: seasons", spawnseas)
  if(spawnseas != 1){
    cat("Note: spawning seems to be in season ",spawnseas,". Some plots will show only this season.\n",sep="") 
  }

  # define which years are forecast or not
  ts$period <- "time"
  ts$period[ts$Yr > endyr+1] <- "fore"
  if(!forecastplot) ts$period[ts$Yr > endyr + 1] <- "exclude"
  if(forecastplot) xlim=range(tsyears) else xlim=range(tsyears[ts$period=="time"])

  # a function to make the plot
  biofunc <- function(subplot){
    # make the logical vector of which time-series entries to use
    plot1 <- ts$Area==1 & ts$Era=="VIRG" # T/F for in area & is virgin value
    plot2 <- ts$Area==1 & ts$period=="time" & ts$Era!="VIRG" # T/F for in area & not virgin value
    plot3 <- ts$Area==1 & ts$period=="fore" & ts$Era!="VIRG" # T/F for in area & not virgin value
    if(subplot %in% c(3,6,7,9)){
      plot1 <- ts$Area==1 & ts$Era=="VIRG" & ts$Seas == spawnseas # T/F for in area & is virgin value
      plot2 <- ts$Area==1 & ts$period=="time" & ts$Era!="VIRG" & ts$Seas == spawnseas # T/F for in area & not virgin value
      plot3 <- ts$Area==1 & ts$period=="fore" & ts$Era!="VIRG" & ts$Seas == spawnseas # T/F for in area & not virgin value
    }

    # switch for which column of the TIME_SERIES table is being plotted
    # subplot1,2,3 = total biomass
    if(subplot %in% 1:3){
      yvals <- ts$Bio_all
      ylab <- labels[1]
      if(subplot==3){ylab <- labels[2]}
    }
    # subplot4,5,6 = summary biomass
    if(subplot %in% 4:6){
      yvals <- ts$Bio_smry
      ylab <- labels[3]
      if(subplot==6){ylab <- labels[4]}
    }
    # subplot7&8 = spawning biomass
    if(subplot %in% 7:8){
      yvals <- bioscale*ts$SpawnBio
      ylab <- labels[5]
    }
    # subplot9&10 = spawning depletion
    if(subplot %in% 9:10){
      # yvals for spatial models are corrected later within loop over areas
      yvals <- ts$SpawnBio/ts$SpawnBio[1]
      ylab <- labels[6]
    }

    # subplot11&12 = recruitment
    if(subplot %in% 11:12){
      yvals <- ts$Recruit_0
      ylab <- labels[8]
    }
    main=ylab

    # sum up total across areas if needed
    if(nareas>1){
      if(subplot%in%c(2,3,6,8,10,12)){
        # these plots have separate lines for each area
        main=paste(main,"by area")
      }
      if(subplot %in% c(1,4,11)){
        # these plots have sum across areas
        yvals2 <- rep(NA,length(ts$Yr))
        for(iyr in 1:length(yvals)){
          y <- ts$Yr[iyr]
          yvals2[iyr] <- sum(yvals[ts$Yr==y])
        }
        yvals <- yvals2
      }
      if(subplot==9){
        # sum up total across areas differently for spawning depletion
        yvals2 <- rep(NA,length(ts$Yr))
        for(iyr in 1:length(yvals)){
          y <- ts$Yr[iyr]
          yvals[iyr] <- sum(ts$SpawnBio[ts$Yr==y])
        }
        yvals <- yvals/yvals[!is.na(yvals)][1] # total depletion
      }
      ymax <- max(yvals,1,na.rm=TRUE)

      # correct ymax value for plot 10 (other plots may need it too)
      if(subplot==10){
        for(iarea in 1:nareas){
          yvals <- ts$SpawnBio[ts$Area==iarea]/(ts$SpawnBio[ts$Area==iarea & ts$Seas==spawnseas][1])
          ymax <- max(ymax,yvals,na.rm=TRUE)
        }
      }
    }
    
    if(forecastplot) main <- paste(main,"with forecast")
    # calculating intervals around spawning biomass, depletion, or recruitment
    # area specific confidence intervals?
    if(uncertainty & subplot %in% c(7,9,11)){
      main <- paste(main,"with ~95% asymptotic intervals")
      if(!"SPB_Virgin" %in% derived_quants$LABEL){
        print("Skipping spawning biomass with uncertainty plot because 'SPB_Virgin' not in derived quantites.",quote=FALSE)
        print("  Try changing 'min yr for Spbio_sdreport' in starter file to -1.",quote=FALSE)
      }else{
        # get subset of DERIVED_QUANTITIES
        if(subplot==7){ # spawning biomass
          stdtable <- derived_quants[grep("SPB_Virgin",derived_quants[,1]):(grep("Recr_Virgin",derived_quants[,1])-1),1:3]
          # year as part of the LABEL string starting with 5th character
          stdtable$Yr <- substring(stdtable$LABEL,5)
          # filling in Virgin and Initial years as 2 and 1 years prior to following years
          stdtable$Yr[1:2] <- as.numeric(stdtable$Yr[3])-(2:1)
          stdtable$Yr <- as.numeric(stdtable$Yr)
        }
        if(subplot==9){ # spawning depletion
          stdtable <- derived_quants[substring(derived_quants$LABEL,1,6)=="Bratio",]
          stdtable$Yr <- as.numeric(substring(stdtable$LABEL,8))
          bioscale <- 1
          if(abs(stdtable$Value[1] - 4)<.1) bioscale <- .25 # temporary fix
        }
        if(subplot==11){ # recruitment
          stdtable <- derived_quants[substring(derived_quants$LABEL,1,5)=="Recr_",]
          stdtable <- stdtable[stdtable$LABEL!="Recr_Unfished",]
          # year as the part of the LABEL string starting with 6th character
          stdtable$Yr <- substring(stdtable$LABEL,6)
          # filling in Virgin and Initial years as 2 and 1 years prior to following years
          stdtable$Yr[1:2] <- as.numeric(stdtable$Yr[3])-(2:1)
          stdtable$Yr <- as.numeric(stdtable$Yr)
          bioscale <- 1
        }
        v <- stdtable$Value * bioscale
        std <- stdtable$StdDev * bioscale
        stdtable$upper <- v + 1.96*std
        stdtable$lower <- pmax(v - 1.96*std, 0) # max of value or 0

        if(max(stdtable$Yr) < max(ts$Yr)){
          print("  !warning:",quote=FALSE)
          print(paste("   ",max(stdtable$Yr),"is last year with uncertainty in Report file, but",max(ts$Yr),"is last year of time series."),quote=FALSE)
          print("    Consider changing starter file input for 'max yr for sdreport outputs' to -2",quote=FALSE)
        }
      }
    }

    # improved y-range for plot (possibly excluding time periods that aren't plotted)
    #   only works on single area models
    if(nareas==1){
      ymax <- max(yvals[plot1 | plot2 | plot3], na.rm=TRUE)
    }
    

    if(uncertainty & subplot %in% c(7,9,11)) ymax <- max(ymax,stdtable$upper, na.rm=TRUE)

    if(print){ # if printing to a file
      # adjust file names
      filename <- main
      filename <- gsub(",","",filename,fixed=TRUE)
      filename <- gsub("~","",filename,fixed=TRUE)
      filename <- gsub("%","",filename,fixed=TRUE)
      if(forecastplot) filename <- paste(filename,"forecast")
      if(uncertainty & subplot %in% c(5,7,9)) filename <- paste(filename,"intervals")
      filename <- paste(filename,".png",sep="")
      filename <- paste(plotdir,filename,sep="")
      if(verbose) print(paste("printing plot to file:",filename))
      pngfun(file=filename)
    }

    # create an empty plot (if not adding to existing plot)
    if(!add) plot(ts$Yr[plot1 | plot2 | plot3],yvals[plot1 | plot2 | plot3],
                  type='n', xlab=xlab, ylim=c(0,ymax), ylab=ylab, main=main, cex.main=cex.main)

    # add stuff to plot
    if(subplot %in% c(9,10))
    {
      addtarg <- function(){
        if(btarg>0){
          abline(h=btarg,col="red")
          text(startyr+4,btarg+0.03,"Management target",adj=0)
        }
        if(minbthresh>0){
          abline(h=minbthresh,col="red")
          text(startyr+4,minbthresh+0.03,"Minimum stock size threshold",adj=0)
        }
      }
      addtarg()
    }

    # not sure why the following line was used: too avoid a bug?
    if(subplot %in% c(1,4,7,9,11)) myareas <- 1 else myareas <- areas 
        
    for(iarea in myareas){ # loop over chosen areas
      ###
      # subset for time period to allow different colors in plot
      #   plot1 = subset for equilibrium (virgin) values
      #   plot2 = subset for main timeseries
      #   plot3 = subset for forecast
      ###
      if(subplot==10){
        yvals <- ts$SpawnBio/(ts$SpawnBio[ts$Area==iarea & ts$Seas==spawnseas][1])
      }
      if(subplot %in% c(3,6,7,8,9,10)){
        plot1 <- ts$Area==iarea & ts$Era=="VIRG" & ts$Seas == spawnseas # T/F for in area & is virgin value
        plot2 <- ts$Area==iarea & ts$period=="time" & ts$Era!="VIRG" & ts$Seas == spawnseas # T/F for in area & not virgin value
        plot3 <- ts$Area==iarea & ts$period=="fore" & ts$Era!="VIRG" & ts$Seas == spawnseas # T/F for in area & not virgin value
      }else{
        plot1 <- ts$Area==iarea & ts$Era=="VIRG" # T/F for in area & is virgin value
        plot2 <- ts$Area==iarea & ts$period=="time" & ts$Era!="VIRG" # T/F for in area & not virgin value
        plot3 <- ts$Area==iarea & ts$period=="fore" & ts$Era!="VIRG" # T/F for in area & not virgin value
      }
      if(length(myareas)>1) mycol <- areacols[iarea] else mycol <- "blue"
      mytype <- "o" # overplotting points on lines for most time series
      if(subplot==11 & uncertainty) mytype <- "p" # just points without connecting lines if plotting recruitment with confidence intervals
      lines(ts$Yr[plot2],yvals[plot2],type=mytype,col=mycol) # open points and lines in middle
      points(ts$Yr[plot3],yvals[plot3],pch=19,  col=mycol) # filled points for forecast

      # add lines for confidence intervals areas if requested
      if(uncertainty){
        if(subplot %in% c(7,9,11)){
          # subset years for confidence intervals
          plot1 <- stdtable$Yr %in% ts$Yr[plot1]
          plot2 <- stdtable$Yr %in% ts$Yr[plot2]
          plot3 <- stdtable$Yr %in% ts$Yr[plot3]
          plotall <- plot1 | plot2 | plot3 # all years that are within ts$Yr
        }
        if(subplot %in% c(7,9)){
          # add lines for main period
          lines(stdtable$Yr[plot2], stdtable$upper[plot2], lty=2, col=mycol)
          lines(stdtable$Yr[plot2], stdtable$lower[plot2], lty=2, col=mycol)

          # add dashes for early period
          points(stdtable$Yr[plot1], stdtable$upper[plot1], pch="-", col=mycol)
          points(stdtable$Yr[plot1], stdtable$lower[plot1], pch="-", col=mycol)

          # add dashes for forecast period
          points(stdtable$Yr[plot3], stdtable$upper[plot3], pch="-", col=mycol)
          points(stdtable$Yr[plot3], stdtable$lower[plot3], pch="-", col=mycol)
        }
        if(subplot==11){ # confidence intervals as error bars because recruitment is more variable
          arrows(x0=stdtable$Yr[plotall], y0=stdtable$lower[plotall], y1=stdtable$upper[plotall],
                 length=0.01, angle=90, code=3, col=mycol)
        }
      } # end if uncertainty
    } # end loop over areas
    if(nareas>1 & subplot%in%c(2,3,5,6,8,10,12)) legend("topright",legend=areanames[areas],lty=1,pch=1,col=areacols[areas],bty="n")
    abline(h=0,col="grey")
    if(verbose) cat("  finished time series subplot ",subplot,": ",main,"\n",sep="")
    if(print) dev.off()

  } # end biofunc

  # make plots
  for(iplot in subplot){
    # plots 2 and 5 are redundant for 1-area models
    # plots 3 and 6 are redundant for 1-season models
    if(!(nareas==1 & iplot %in% c(2,5))|(nseasons==1 & iplot %in% c(3,6))){
      biofunc(subplot=iplot)
    }
  }
}
