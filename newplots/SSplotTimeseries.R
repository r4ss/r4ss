SSplotTimeseries <-
  function(replist,subplots=1:5,add=FALSE,areas="all",areacols=1:10,areanames="default",
           forecastplot=TRUE,uncertainty=TRUE,bioscale="default",
           plot=TRUE,print=FALSE,plotdir="default",verbose=FALSE,
           xlab="Year",
           labels=c("Total biomass (mt)", #1
             "Summary biomass (mt)",      #2
             "Spawning biomass (mt)",     #3
             "Spawning depletion",        #4
             "Spawning output (eggs)",    #5
             "Age-0 recruits (1,000s)"),  #6
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12)
{
  # individual function for plotting time series of total or summary biomass
  # subplot1 = total biomass total
  # subplot2 = total biomass
  # subplot3 = summary biomass total
  # subplot4 = summary biomass
  # subplot5 = spawning biomass total (with or without uncertainty)
  # subplot6 = spawning biomass by area
  # subplot7 = spawning depletion total (with or without uncertainty)
  # subplot8 = spawning depletion by area
  # subplot9 = recruitment total (with or without uncertainty)
  # subplot10 = recruitment by area

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

  if(plotdir=="default") plotdir <- replist$inputs$dir
  if(FecPar2!=0) labels[3] <- labels[5]

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
  # modifying data
  ts <- timeseries
  ts$Yr <- ts$Yr + (ts$Seas-1)/nseasons
  tsyears <- ts$Yr[ts$Seas==1]
  tsarea <- ts$Area[ts$Seas==1]

  # get values
  tsspaw_bio <- bioscale*ts$SpawnBio[ts$Seas==1]
  dep <- tsspaw_bio/tsspaw_bio[1] # depletion
  if(nareas > 1){
    for(a in areas){
      asb <- tsspaw_bio[tsarea==a]
      dep[tsarea==a] <- asb/asb[1]
    }
  }

  # define which years are forecast or not
  ts$period <- "time"
  ts$period[ts$Yr > endyr+2] <- "fore"
  if(!forecastplot) ts$period[ts$Yr > endyr+1] <- "exclude"
  if(forecastplot) xlim=range(tsyears) else xlim=range(tsyears[ts$period=="time"])

  # a function to make the plot
  biofunc <- function(subplot){
    plot1 <- ts$Area==1 & ts$Era=="VIRG" # T/F for in area & is virgin value
    plot2 <- ts$Area==1 & ts$period=="time" & ts$Era!="VIRG" # T/F for in area & not virgin value
    plot3 <- ts$Area==1 & ts$period=="fore" & ts$Era!="VIRG" # T/F for in area & not virgin value

    # switch for which column of the TIME_SERIES table is being plotted

    # subplot1&2 = total biomass
    if(subplot %in% 1:2){
      yvals <- ts$Bio_all
      ylab <- labels[1]
    }
    # subplot3&4 = summary biomass
    if(subplot %in% 3:4){
      yvals <- ts$Bio_smry
      ylab <- labels[2]
    }
    # subplot5&6 = spawning biomass
    if(subplot %in% 5:6){
      yvals <- bioscale*ts$SpawnBio
      ylab <- labels[3]
    }
    # subplot7&8 = spawning depletion
    if(subplot %in% 7:8){
      # yvals for spatial models to be corrected later within loop over areas
      yvals <- ts$SpawnBio/ts$SpawnBio[1]
      ylab <- labels[4]
    }
    # subplot9&10 = recruitment
    if(subplot %in% 9:10){
      yvals <- ts$Recruit_0
      ylab <- labels[6]
    }
    main=ylab

    # sum up total across areas if needed
    if(nareas>1){
      if(subplot%in%c(2,4,6,8,10)){
        # these plots have separate lines for each area
        main=paste(main,"by area")
      }
      if(subplot %in% c(1,3,5,9)){
        # these plots have sum across areas
        yvals2 <- rep(NA,length(ts$Yr))
        for(iyr in 1:length(yvals)){
          y <- ts$Yr[iyr]
          yvals2[iyr] <- sum(yvals[ts$Yr==y])
        }
        yvals <- yvals2
      }
      if(subplot==7){
        # sum up total across areas differently for depletion
        yvals2 <- rep(NA,length(ts$Yr))
        for(iyr in 1:length(yvals)){
          y <- ts$Yr[iyr]
          yvals[iyr] <- sum(ts$SpawnBio[ts$Yr==y])
        }
        yvals <- yvals/yvals[1] # total depletion
      }
    }
    if(forecastplot) main <- paste(main,"with forecast")

    # calculating intervals around spawning biomass, depletion, or recruitment
    # area specific confidence intervals?
    if(uncertainty & subplot %in% c(5,7,9)){
      main <- paste(main,"with ~95% asymptotic intervals")
      if(!"SPB_Virgin" %in% derived_quants$LABEL){
        print("Skipping spawning biomass with uncertainty plot because 'SPB_Virgin' not in derived quantites.",quote=FALSE)
        print("  Try changing 'min yr for Spbio_sdreport' in starter file to -1.",quote=FALSE)
      }else{
        # get subset of DERIVED_QUANTITIES
        if(subplot==5){ # spawning biomass
          stdtable <- matchfun2("SPB_Virgin",0,"Recr_Virgin",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=derived_quants,objsubset=derived_quants,substr1=TRUE,substr2=TRUE)
          # year as part of the LABEL string starting with 5th character
          stdtable$Yr <- substring(stdtable$LABEL,5)
          # filling in Virgin and Initial years as 2 and 1 years prior to following years
          stdtable$Yr[1:2] <- as.numeric(stdtable$Yr[3])-(2:1)
          stdtable$Yr <- as.numeric(stdtable$Yr)
        }
        if(subplot==7){ # spawning depletion
          stdtable <- derived_quants[substring(derived_quants$LABEL,1,6)=="Bratio",]
          stdtable$Yr <- as.numeric(substring(stdtable$LABEL,8))
          bioscale <- 1
          if(abs(stdtable$Value[1] - 4)<.1) bioscale <- .25 # temporary fix
        }
        if(subplot==9){ # recruitment
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
      }
    }

    # y range for plot
    ymax <- max(yvals[plot1 | plot2 | plot3])
    # correct depeltion limits in mutli-area models
    if(subplot==8 & length(areas)>1){
      ymax <- 0
      for(iarea in areas){
        yvals <- ts$SpawnBio[ts$Area==iarea]/ts$SpawnBio[ts$Area==iarea][1]
        ymax <- max(ymax,max(yvals))
      }
    }
    if(uncertainty & subplot %in% c(5,7,9)) ymax <- max(ymax,stdtable$upper)

    if(print){ # if printing to a file
      # adjust file names
      filename <- main
      filename <- gsub(",","",filename,fixed=TRUE)
      filename <- gsub("~","",filename,fixed=TRUE)
      filename <- gsub("%","",filename,fixed=TRUE)
      ## filename <- gsub(")","",filename,fixed=TRUE)
      ## filename <- gsub("'","",filename,fixed=TRUE)
      ## filename <- gsub(" ","_",filename,fixed=TRUE)
      ## filename <- gsub("__","_",filename,fixed=TRUE)
      if(forecastplot) filename <- paste(filename,"forecast")
      if(uncertainty & subplot %in% c(5,7,9)) filename <- paste(filename,"intervals")
      filename <- paste(filename,".png",sep="")
      ## filename <- gsub("_.",".",filename,fixed=TRUE)
      filename <- paste(plotdir,filename,sep="")
      if(verbose) print(paste("printing plot to file:",filename))
      pngfun(file=filename)
    }

    # create an empty plot (if not adding to existing plot)
    if(!add) plot(ts$Yr[plot1 | plot2 | plot3],yvals[plot1 | plot2 | plot3],
                  type='n',xlab=xlab,ylim=c(0,ymax),ylab=ylab,main=main)

    # add stuff to plot
    if(subplot %in% c(1,3,5,7,9)) myareas <- 1 else myareas <- areas
    for(iarea in myareas){ # loop over chosen areas
      # subset for time period
      if(subplot==8){ # depletion by area needs different treatment
        yvals <- ts$SpawnBio[ts$Area==iarea]/(ts$SpawnBio[ts$Area==iarea][1])
        plot1 <- ts$Era=="VIRG" # T/F for in area & is virgin value
        plot2 <- ts$period=="time" & ts$Era!="VIRG" # T/F for in area & not virgin value
        plot3 <- ts$period=="fore" & ts$Era!="VIRG" # T/F for in area & not virgin value
      }else{
        plot1 <- ts$Area==iarea & ts$Era=="VIRG" # T/F for in area & is virgin value
        plot2 <- ts$Area==iarea & ts$period=="time" & ts$Era!="VIRG" # T/F for in area & not virgin value
        plot3 <- ts$Area==iarea & ts$period=="fore" & ts$Era!="VIRG" # T/F for in area & not virgin value
      }
      if(length(myareas)>1) mycol <- areacols[iarea] else mycol <- "blue"

      mytype <- "o" # overplotting points on lines for most time series
      if(subplot==9 & uncertainty) mytype <- "p" # just points without connecting lines if plotting recruitment with confidence intervals

      points(ts$Yr[plot1],yvals[plot1],pch=19,  col=mycol) # filled points for early part
      lines( ts$Yr[plot2],yvals[plot2],type=mytype,col=mycol) # open points and lines in middle
      points(ts$Yr[plot3],yvals[plot3],pch=19,  col=mycol) # filled points for forecast

      # add lines for confidence intervals areas if requested
      if(uncertainty){
        if(subplot %in% c(5,7,9)){
          # subset years for confidence intervals
          plot1 <- stdtable$Yr %in% ts$Yr[plot1]
          plot2 <- stdtable$Yr %in% ts$Yr[plot2]
          plot3 <- stdtable$Yr %in% ts$Yr[plot3]
          plotall <- plot1 | plot2 | plot3 # all years that are within ts$Yr
        }
        if(subplot %in% c(5,7)){
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
        if(subplot==9){ # confidence intervals as error bars because recruitment is more variable
          arrows(x0=stdtable$Yr[plotall], y0=stdtable$lower[plotall], y1=stdtable$upper[plotall],
                 length=0.01, angle=90, code=3, col=mycol)
        }
      } # end if uncertainty
    } # end loop over areas
    if(nareas>1 & subplot%in%c(2,4,6,8,10)) legend("topright",legend=areanames[areas],lty=1,pch=1,col=areacols[areas],bty="n")
    abline(h=0,col="grey")
    if(verbose) print(paste("  finished time series subplot ",subplot,": ",main,sep=""),quote=FALSE)
    if(print) dev.off()
  } # end biofunc

  # make plots
  for(iplot in subplots){
    # plots 2 and 4 are redundant for 1-area models
    if(!(nareas==1 & iplot %in% c(2,4))){
      biofunc(subplot=iplot)
    }
  }
}


