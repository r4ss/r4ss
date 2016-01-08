#' Plot selectivity
#' 
#' Plot selectivity, including retention and other quantities, with additional
#' plots for time-varying selectivity.
#' 
#' 
#' @param replist List created by \code{SS_output}
#' @param fleets Optional vector to subset fleets for which to make plots
#' @param infotable Optional table of information controlling appearance of
#' plot and legend. Is produced as output and can be modified and entered as
#' input.
#' @param fleetnames Optional replacement for fleenames used in data file
#' @param sizefactors Which elements of the factors column of SIZE_SELEX should
#' be included in plot of selectivity across multiple fleets?
#' @param agefactors Which elements of the factors column of AGE_SELEX should
#' be included in plot of selectivity across multiple fleets?
#' @param years Which years for selectivity are shown in multi-line plot
#' (default = last year of model).
#' @param season Which season (if seasonal model) for selectivity shown in
#' multi-line plot (default = 1).
#' @param sexes Optional vector to subset genders for which to make plots
#' (1=females, 2=males)
#' @param selexlines Vector to select which lines get plotted. values are 1.
#' Selectivity, 2. Retention, 3. Discard mortality, 4. Keep = Sel*Ret, 5. Dead
#' = Sel*(Ret+(1-Ret)*Mort).
#' @param subplot Vector controlling which subplots to create
#' @param skipAgeSelex10 Exclude plots for age selectivity type 10 (selectivity
#' = 1.0 for all ages beginning at age 1)?
#' @param lwd Line widths for plots
#' @param fleetcols Optional vector of colors for each fleet (in multi-fleet
#' plots)
#' @param fleetpch Optional vector of plot characters for each fleet (in
#' multi-fleet plots)
#' @param fleetlty Optional vector of line types for each fleet (in multi-fleet
#' plots)
#' @param spacepoints number of years between points shown on top of lines (for
#' long timeseries, points every year get mashed together)
#' @param staggerpoints number of years to stagger the first point (if
#' \code{spacepoints > 1}) for each line (so that adjacent lines have points in
#' different years)
#' @param legendloc location of legend. See ?legend for more info.
#' @param plot Plot to active plot device?
#' @param print Print to PNG files?
#' @param add Add to existing plot (not yet implemented)
#' @param labels vector of labels for plots (titles and axis labels)
#' @param col1 color for female growth curve
#' @param col2 color for male growth curve
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize point size for PNG file
#' @param cex.main character expansion for plot titles
#' @param showmain Include main title at top of plot?
#' @param plotdir Directory where PNG files will be written. By default it will
#' be the directory where the model was run.
#' @param verbose report progress to R GUI?
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
#' @keywords hplot
SSplotSelex <-
  function(replist, infotable=NULL,
           fleets="all", fleetnames="default",
           sizefactors=c("Lsel"),
           agefactors=c("Asel","Asel2"),
           years="endyr",
           season=1,
           sexes="all", 
           selexlines=1:6,
           subplot=1:25,
           skipAgeSelex10=TRUE,
           plot=TRUE, print=FALSE, add=FALSE,
           labels=c("Length (cm)", #1
                    "Age (yr)",    #2
                    "Year",        #3
                    "Selectivity", #4
                    "Retention",   #5
                    "Discard mortality"),  #6
           col1="red",col2="blue",lwd=2,
           fleetcols="default",
           fleetpch="default",
           fleetlty="default",
           spacepoints=5,
           staggerpoints=1,
           legendloc="bottomright",
           pwidth = 7, pheight = 7, punits = "in",
           res = 300, ptsize = 12,
           cex.main=1, showmain=TRUE, plotdir = "default",
           verbose = TRUE)
{
  # subplots:

  #### all fleets grouped (length, and then age)
  # 1.  selectivity at length in end year for all fleets shown together
  # 2.  selectivity at age in end year for all fleets shown together
  #     (this includes both age-based selectivity "Asel" and age values derived
  #      from length-based, "Asel2". You can choose only one using
  #      "agefactors" if needed.)

  ## time-varying stuff
  # 3.  selectivity at length time-varying surface
  # 4.  selectivity at length time-varying contour
  # 5.  retention at length time-varying surface
  # 6.  retention at length time-varying surface
  # 7.  discard mortality time-varying surface
  # 8.  discard mortality time-varying contour

  ## selectivity at length in end year by fleet
  # 9.  selectivity, retention, and discard mortality at length in ending year

  #### age based
  ## time-varying stuff
  # 11. selectivity at age time-varying surface
  # 12. selectivity at age time-varying contour

  ## selectivity at age in end year
  # 13. selectivity at age in ending year if time-varying
  # 14. selectivity at age in ending year if NOT time-varying

  #### both/either age or length
  # 21. selecitivity at age and length contour with overlaid growth curve
  # 22. selectivity with uncertainty if requested at end of control file

  # empty table into which information on line types etc. might be copied
  infotable2 <- NULL
  
  nsexes         <- replist$nsexes
  nseasons       <- replist$nseasons
  nfleets        <- replist$nfleets
  lbinspop       <- replist$lbinspop
  nlbinspop      <- replist$nlbinspop
  sizeselex      <- replist$sizeselex
  ageselex       <- replist$ageselex
  accuage        <- replist$accuage
  startyr        <- replist$startyr
  endyr          <- replist$endyr
  FleetNames     <- replist$FleetNames
  growdat        <- replist$endgrowth
  growthCVtype   <- replist$growthCVtype
  mainmorphs     <- replist$mainmorphs
  nareas         <- replist$nareas
  ngpatterns     <- replist$ngpatterns
  derived_quants <- replist$derived_quants
  
  
  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  if(plotdir=="default") plotdir <- replist$inputs$dir

  ians_blues <- c("white","grey","lightblue","skyblue","steelblue1","slateblue",topo.colors(6),"blue","blue2","blue3","blue4","black")

  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{
    if(length(intersect(fleets,1:nfleets))!=length(fleets)){
      return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
    }
  }
  if(fleetnames[1]=="default") fleetnames <- FleetNames # note lower-case value is the one used below (either equal to vector from replist, or input by user)

  if(sexes[1]=="all"){
    sexes <- 1:nsexes
  }else{
    if(length(intersect(sexes,1:nsexes))!=length(sexes)){
      return("Input 'sexes' should be 'all' or a vector of values between 1 and nsexes.")
    }
  }
  if(years[1]=="endyr") years <- endyr
  
  ################################################################################
  ### make plot of selectivity at length for all fleets together
  # make plots
  plotAllSel <- function(factor="Lsel"){
    # first subset values
    if(factor %in% unique(sizeselex$Factor)){
      agebased <- FALSE
      allselex <- sizeselex[sizeselex$Factor==factor &
                            sizeselex$Fleet %in% fleets &
                            sizeselex$gender %in% sexes,]
    }
    if(factor %in% unique(ageselex$factor)){
      agebased <- TRUE
      allselex <- ageselex[ageselex$factor==factor &
                           ageselex$seas==season &
                           ageselex$fleet %in% fleets &
                           ageselex$gender %in% sexes,]
    }
    if(!factor %in% unique(c(sizeselex$Factor,ageselex$factor))){
      cat("In selectivity plots, factor ",factor,"not found in age- or length-based selectivity.\n")
      return()
    }
    if(nrow(allselex)==0){
      cat("combination of season, fleets, & sexes didn't produce any results\n")
      return()
    }
    # make lowercase to remove inconsistensies between data frames
    names(allselex) <- tolower(names(allselex))
## print(head(allselex))
## print(tail(allselex))
    time <- rep(FALSE,nfleets)
    for(ifleet in fleets)
      time[ifleet] <- any(apply(allselex[allselex$fleet==ifleet & allselex$year%in%(startyr:endyr),], 2, function(x){any(x!=x[1])}))
    if(any(time)){
      if(length(years)>1 & length(fleets)>1) cat("plot not yet configured to work well with multiple years and multiple fleets\n")
      # do a bunch of tedious filtering to get unique year ranges
      inputyears <- years
      years <- NULL
      years2 <- NULL
      year_ranges <- NULL
      for(i in 1:length(inputyears)){
        if(inputyears[i]>=startyr){
          newyear <- min(endyr,allselex$year[allselex$year >= inputyears[i]])
          newyear2 <- max(startyr,allselex$year[allselex$year <= inputyears[i]])
          if(newyear2<=newyear){
            newyear_range <- paste(newyear2,"-",newyear,sep="")
            if(newyear==newyear2 & newyear>startyr-3) newyear_range <- newyear
            if(!newyear_range %in% year_ranges){
              years <- c(years,newyear)
              years2 <- c(years2,newyear2)
              year_ranges <- c(year_ranges,newyear_range)
            }
          }
        }
      }
      if(all(years2==startyr & years==endyr)){
        years <- endyr
        years2 <- startyr
        year_ranges <- paste(startyr,"-",endyr,sep="")
      }
      bad <- rep(FALSE,length(years))
      for(i in 1:length(years)){
        y  <- years[i]
        y2 <- years2[i]
        if(sum(years==y)>1) bad[years==y & years2==y] <- TRUE
        if(sum(years2==y2)>1) bad[years==y2 & years2==y2] <- TRUE
      }
      years <- years[!bad]
      years2 <- years2[!bad]
      year_ranges <- year_ranges[!bad]

      if((startyr-3) %in% inputyears){
        years <- c(years,startyr-3)
        year_ranges <- c(year_ranges,"Benchmarks")
      }
    }else{
      years <- endyr
    }
    allselex <- allselex2 <- allselex[allselex$year %in% years,]

    # do some processing
    gender <- allselex$gender
    if(!agebased){
      allselex <- allselex[,-(1:5)]
      xlab <- labels[1]
    }
    if(agebased){
      allselex <- allselex[,-(1:7)]
      xlab <- labels[2]
    }
    if(!is.null(infotable)){
      infotable2 <- infotable
      good <- gender %in% infotable$gender
      allselex <- allselex[good,]
      allselex2 <- allselex2[good,]
      if(nrow(infotable2)!=nrow(allselex)){
        stop("Problem with input 'infotable'. Number of rows doesn't match.")
      }
    }else{
      # make table of info for each row (unless it is supplied already)
      infotable2 <- allselex2[c("fleet","gender","year")]
      infotable2$ifleet <- NA
      infotable2$FleetName <- fleetnames[infotable2$fleet]
      infotable2$longname <- infotable2$FleetName
      for(i in 1:nrow(infotable2)) infotable2$year_range[i] <- year_ranges[years==infotable2$year[i]]

      if(length(unique(infotable2$year)) > 1){
        infotable2$longname <- paste(infotable2$FleetName,infotable2$year_range)
      }
      # check for whether there are differences between males and females
      twosex <- all(1:2 %in% infotable2$gender) && any(allselex[infotable2$gender==1,]!=allselex[infotable2$gender==2,])
      if(!twosex){ # show only sex with lowest number if no differences between genders
        good <- infotable2$gender==min(infotable2$gender)
        allselex <- allselex[good,]
        allselex2 <- allselex2[good,]
        infotable2 <- infotable2[good,]
      }else{
        infotable2$longname <- paste(infotable2$longname, c("(f)","(m)")[infotable2$gender])
      }
      
      # add index from 1 up to number of fleets plotted
      allfleets <- sort(unique(infotable2$fleet))
      for(ifleet in 1:length(allfleets))
        infotable2$ifleet[infotable2$fleet==allfleets[ifleet]] <- ifleet
      # choose colors
      colvec <- rich.colors.short(length(allfleets))
      infotable2$col <- colvec[infotable2$ifleet]
      # choose line types
      infotable2$lty <- 1
      # either line by gender
      infotable2$lwd <- lwd
      if(twosex) infotable2$lty <- infotable2$gender
      # or line by year (with line width by gender)
      allyears <- sort(unique(infotable2$year))
      if(length(allyears)>1){
        for(iyear in 1:length(allyears))
          infotable2$lty[infotable2$year==allyears[iyear]] <- iyear
        if(twosex) infotable2$lwd[infotable2$gender==2] <- lwd/2
      }
      # choose plot characters
      infotable2$pch <- infotable2$ifleet %% 25
    }

    main <- factor
    if(factor=="Lsel") main <- paste("Length-based selectivity")
    if(factor=="Asel") main <- paste("Age-based selectivity")
    if(factor=="Asel2") main <- paste("Derived age-based from length-based selectivity")
    if(factor=="Ret") main <- paste("Retention")
    if(length(fleets)>1) main <- paste(main, "by fleet")
    if(length(fleets)==1) main <- paste(main, "for", fleetnames[fleets])
       
    if(length(unique(infotable2$year))==1){
      main <- paste(main,"in",unique(infotable2$year))
    }
    if(!showmain) main <- NULL
## cat("info on plot for debugging:\n")    
## print(infotable2)
    
    bins <- as.numeric(names(allselex))

    # make empty plot
    if(!add) plot(0,xlim=range(bins),ylim=c(0,1),type='n',
                  main=main,cex.main=cex.main,xlab=xlab,ylab=labels[4])
    # add grey lines
    abline(h=0,col="grey")
    abline(h=1,col="grey")
    # add lines for selectivities
    matplot(x=bins, y=t(allselex), col=infotable2$col, 
            lty=infotable2$lty, lwd=infotable2$lwd, type="l",add=TRUE)
    # add points on top of lines (first subsetting to optionally show fewer points)
    allselex2 <- allselex
    if(spacepoints > 0){
      for(iline in 1:nrow(allselex))
        allselex2[iline,(1:ncol(allselex))%%spacepoints != (staggerpoints*iline)%%spacepoints] <- NA
      matplot(x=bins, y=t(allselex2), col=infotable2$col,
              lwd=infotable2$lwd, pch=infotable2$pch, type="p",add=TRUE)
    }else{
      # if 'spacepoints' is less than or equal to 0, don't show points
      infotable2$pch <- NA
    }
    # add legend
    if(nrow(infotable2)>1)
      legend(legendloc, inset=c(0,0.05), legend=infotable2$longname, col=infotable2$col,
             seg.len=4,
             lty=infotable2$lty, pch=infotable2$pch, lwd=infotable2$lwd, bty='n')
    return(infotable2)
  }

  if(1 %in% subplot){
    for(ifactor in 1:length(sizefactors)){
      if(plot) infotable2 <- plotAllSel(factor=sizefactors[ifactor])
      if(print){
        file <- paste(plotdir,"sel01_multiple_fleets_length",ifactor,".png",sep="")
        caption <- "Selectivity at length for multiple fleets."
        plotinfo <- pngfun(file=file, caption=caption)
        infotable2 <- plotAllSel(factor="Lsel")
        dev.off()
      }
    }
  }
  if(2 %in% subplot){
    for(ifactor in 1:length(agefactors)){
      factor <- agefactors[ifactor]
      if(plot) infotable2 <- plotAllSel(factor=factor)
      if(print){
        file <- paste(plotdir,"sel02_multiple_fleets_age",ifactor,".png",sep="")
        caption <- "Selectivity at age for multiple fleets."
        if(factor=="Asel2")
          caption <- paste("Selectivity at age derived from selectivity at length for multiple fleets.")
        plotinfo <- pngfun(file=file, caption=caption)
        infotable2 <- plotAllSel(factor=factor)
        dev.off()
      }
    }
  }

  ################################################################################
  ### loop over fleets and sexes to make individual plot of length-based patterns
  
  # selex and retention
  for(i in fleets)
  {
    for(m in sexes)
    {
      if(m==1 & nsexes==1) sextitle1 <- "Time-"
      if(m==1 & nsexes==2) sextitle1 <- "Female time-"
      if(m==2) sextitle1 <- "Male time-"
      if(m==1 & nsexes==1) sextitle2 <- "Ending"
      if(m==1 & nsexes==2) sextitle2 <- "Female ending"
      if(m==2) sextitle2 <- "Male ending"
      intret   <- sizeselex[sizeselex$Factor=="Ret"  & sizeselex$year!=startyr-3 & sizeselex$gender==m,]
      intmort  <- sizeselex[sizeselex$Factor=="Mort" & sizeselex$year!=startyr-3 & sizeselex$gender==m,]
      intkeep  <- sizeselex[sizeselex$Factor=="Keep" & sizeselex$year!=startyr-3 & sizeselex$gender==m,]
      intdead  <- sizeselex[sizeselex$Factor=="Dead" & sizeselex$year!=startyr-3 & sizeselex$gender==m,]
      intselex <- sizeselex[sizeselex$Factor=="Lsel" & sizeselex$year!=startyr-3 & sizeselex$gender==m,]
      plotselex <- intselex[intselex$Fleet==i,]
      plotret <- intret[intret$Fleet==i,]
      plotmort <- intmort[intmort$Fleet==i,]

      # test for time-varying length selectivity
      time <- any(apply(plotselex[-c(1,nrow(plotselex)),-(1:5)], 2, function(x){any(x!=x[1])}))      
      if(time)
      {
        x <- lbinspop
        y <- plotselex$year
        z <- plotselex[,-(1:5)]
        z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
        z <- t(z)
        main <- paste(sextitle1,"varying selectivity for ", fleetnames[i],sep="")
        if(plot)
        {
          if(3 %in% subplot) persp(x,y,z,col="white",xlab=labels[1],ylab=labels[3],zlab=labels[4],expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
          if(4 %in% subplot) contour(x,y,z,nlevels=5,xlab=labels[1],ylab=labels[3],main=main,cex.main=cex.main,col=ians_blues,lwd=lwd)
        }
        if(print)
        {
          if(3 %in% subplot){
            file <- paste(plotdir,"sel03_len_timevary_surf_flt",i,"sex",m,".png",sep="")
            caption <- paste("Surface plot of",main)
            plotinfo <- pngfun(file=file, caption=caption)
            persp(x,y,z,col="white",xlab=labels[1],ylab=labels[3],zlab=labels[4],expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
            dev.off()
          }
          if(4 %in% subplot){
            file <- paste(plotdir,"sel04_len_timevary_contour_flt",i,"sex",m,".png",sep="")
            caption <- paste("Countour plot of",main)
            plotinfo <- pngfun(file=file, caption=caption)
            contour(x,y,z,nlevels=5,xlab=labels[1],ylab=labels[3],main=main,cex.main=cex.main,col=ians_blues,lwd=lwd)
            dev.off()
          }
        }
      }
      # test for time-varying length retention
      time2 <- any(apply(plotret[-nrow(plotret),-(1:5)],2,function(x){any(x!=x[1])}))
      if(time2)
      {
        x <- lbinspop
        y <- intret$year[intret$Fleet==i]
        z <- intret[intret$Fleet==i,-(1:5)]
        z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
        z <- t(z)
        main <- paste(sextitle1,"varying retention for ", fleetnames[i],sep="")
        if(plot)
        {
          if(5 %in% subplot) persp(x,y,z,col="white",xlab=labels[1],ylab=labels[3],zlab=labels[5],expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
          if(6 %in% subplot) contour(x,y,z,nlevels=5,xlab=labels[1],ylab=labels[3],main=main,cex.main=cex.main,col=ians_blues,lwd=lwd)
        }
        if(print)
        {
          if(5 %in% subplot){
            file <- paste(plotdir,"sel05_timevary_ret_surf_flt",i,"sex",m,".png",sep="")
            caption <- paste("Surface plot of",main)
            plotinfo <- pngfun(file=file, caption=caption)
            persp(x,y,z,col="white",xlab=labels[1],ylab=labels[3],zlab=labels[5],expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
            dev.off()
          }
          if(6 %in% subplot){
            file <- paste(plotdir,"sel06_timevary_ret_contour_flt",i,"sex",m,".png",sep="")
            caption <- paste("Countour plot of",main)
            plotinfo <- pngfun(file=file, caption=caption)
            contour(x,y,z,nlevels=5,xlab=labels[1],ylab=labels[3],main=main,cex.main=cex.main,col=ians_blues,lwd=lwd)
            dev.off()
          }
        }
      }
      # test for time-varying discard mortality rates
      time3 <- any(apply(plotmort[-nrow(plotmort),-(1:5)],2,function(x){any(x!=x[1])}))
      if(time3)
      {
        x <- lbinspop
        y <- intmort$year[intmort$Fleet==i]
        z <- intmort[intmort$Fleet==i,-(1:5)]
        z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
        z <- t(z)
        main <- paste(sextitle1,"varying discard mortality for ", fleetnames[i],sep="")
        if(plot)
        {
          if(7 %in% subplot)
            persp(x,y,z,col="white",xlab=labels[1],ylab=labels[3],zlab=labels[6],
                  expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",
                  phi=35,theta=-10,zlim=c(0,max(z)))
          if(8 %in% subplot)
            contour(x,y,z,nlevels=5,xlab=labels[1],ylab=labels[3],main=main,
                    cex.main=cex.main,col=ians_blues,lwd=lwd)
        }
        if(print)
        {
          if(7 %in% subplot){
            file <- paste(plotdir,"sel07_timevary_mort_surf_flt",i,"sex",m,".png",sep="")
            caption <- paste("Surface plot of",main)
            plotinfo <- pngfun(file=file, caption=caption)
            persp(x,y,z,col="white",xlab=labels[1],ylab=labels[3],zlab=labels[6],expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
            dev.off()
          }
          if(8 %in% subplot){
            file <- paste(plotdir,"sel08_timevary_mort_contour_flt",i,"sex",m,".png",sep="")
            caption <- paste("Surface plot of",main)
            plotinfo <- pngfun(file=file, caption=caption)
            contour(x,y,z,nlevels=5,xlab=labels[1],ylab=labels[3],main=main,cex.main=cex.main,col=ians_blues,lwd=lwd)
            dev.off()
          }
        }
      }

      # make plot of end year selectivity (with retention and discard mortality if used)
      endselex <- plotselex[plotselex$year==endyr,-(1:5)]

      plotret <- plotret[nrow(plotret),-(1:5)] # final year only
      ylab <- labels[4]
      bins <- as.numeric(names(endselex))
      vals <- as.numeric(paste(endselex))
      retvals <- as.numeric(plotret)
      main <- paste(sextitle2," year selectivity for ", fleetnames[i],sep="")
      selfunc <- function()
      {
        # determine whether retention was used
        intret2 <- intret[intret$Fleet==i,]
        retchecktemp <- as.vector(unlist(intret2[1,]))
        retcheck <- as.numeric(retchecktemp[6:length(retchecktemp)])
        if(is.na(sum(retcheck))) retcheckuse <- 0
        # if minimum retention is less than 1, show additional stuff in plot
        if(!is.na(sum(retcheck))) retcheckuse <- 1 - min(retcheck)
        
        # make plot
        if(!add) plot(bins,vals,xlab=labels[1],ylim=c(0,1),
                      main=main,cex.main=cex.main,ylab="",type="n")
        abline(h=0,col="grey")
        abline(h=1,col="grey")
        if(1%in%selexlines) lines(bins,vals,type="o",col=col2,cex=1.1)
        if(retcheckuse > 0){
          # if retention, then add additional lines & legend
          useret <- intret[intret$Fleet==i,]
          usekeep <- intkeep[intkeep$Fleet==i,]
          usemort <- intmort[intmort$Fleet==i,]
          usedead <- intdead[intdead$Fleet==i,]
          if(endyr %in% as.numeric(useret$year)){
            useyr <- endyr
          }else{
            useyr <- max(as.numeric(useret$year))
          }
          plotret <- useret[useret$year==useyr,]
          plotkeep <- usekeep[usekeep$year==useyr,]
          plotmort <- usemort[usemort$year==useyr,]
          plotdead <- usedead[usedead$year==useyr,]
          # compute discard as function of size: selectivity*(1 - retention)
          plotdisc <- plotret
          plotdisc[-(1:5)] <- vals*(1-plotret[,-(1:5)])
          # add additional lines if requested
          if(2%in%selexlines){
            lines((as.numeric(as.vector(names(plotret)[-(1:5)]))),(as.numeric(as.character(plotret[1,-(1:5)]))),col="red",type="o",pch=3,cex=.9)
            ylab <- paste(ylab,", Retention",sep="")
          }
          if(3%in%selexlines){
            lines((as.numeric(as.vector(names(plotmort)[-(1:5)]))),(as.numeric(as.character(plotmort[1,-(1:5)]))),col="orange",type="o",pch=4,cex=.9)
            ylab <- paste(ylab,", Mortality",sep="")
          }
          if(4%in%selexlines) lines((as.numeric(as.vector(names(plotkeep)[-(1:5)]))),(as.numeric(as.character(plotkeep[1,-(1:5)]))),col="purple",type="o",pch=2,cex=.9)
          if(5%in%selexlines) lines((as.numeric(as.vector(names(plotdead)[-(1:5)]))),(as.numeric(as.character(plotdead[1,-(1:5)]))),col="green3",type="o",pch=5,cex=.9)
          if(6%in%selexlines) lines((as.numeric(as.vector(names(plotdead)[-(1:5)]))),(as.numeric(as.character(plotdisc[1,-(1:5)]))),col="grey50",type="o",pch=6,cex=.9)
          # add legend
          legend(legendloc,inset=c(0,0.05),bty="n",
      	   c(labels[4], labels[5], labels[6], "Keep = Sel*Ret",
             "Dead = Sel*(Ret+(1-Ret)*Mort)","Discard = Sel*(1-Ret)")[selexlines],
      	   lty=1,col=c("blue","red","orange","purple","green3","grey50")[selexlines],
      	   pch=c(1,3,4,2,5,6)[selexlines], pt.cex=c(1.1,.9,.9,.9,.9,.9)[selexlines])
        }
        mtext(ylab,side=2,line=3)
      }
      # make plot if selectivity is not constant at 0 or 1 for all bins
      if((min(vals)<1 & max(vals)>0) | (!is.na(diff(range(retvals))) && diff(range(retvals))!=0))
      {
        if(9 %in% subplot){
          if(plot) selfunc()
          if(print){
            file <- paste(plotdir,"sel09_len_flt",i,"sex",m,".png",sep="")
            caption <- main
            plotinfo <- pngfun(file=file, caption=caption)
            selfunc()
            dev.off()
          }
        }
      }
    } # sexes
  } # fleets

  ################################################################################
  ### loop over fleets and sexes to make individual plot of age-based patterns
  
  # Age based selex
  ylab <- labels[4]
  for(facnum in 1){
    factor <- c("Asel","Asel2")[facnum]
    for(i in fleets){
      for(m in sexes){
        if(m==1 & nsexes==1) sextitle1 <- "Time-"
        if(m==1 & nsexes==2) sextitle1 <- "Female time-"
        if(m==2) sextitle1 <- "Male time-"
        if(m==1 & nsexes==1) sextitle2 <- "Ending"
        if(m==1 & nsexes==2) sextitle2 <- "Female ending"
        if(m==2) sextitle2 <- "Male ending"
        ageselexcols <- (1:ncol(ageselex))[names(ageselex) %in% as.character(0:accuage)]
        plotageselex <- ageselex[ageselex$factor==factor & ageselex$fleet==i & ageselex$year!=startyr-3 & ageselex$gender==m,]
        # test for time-varying age selectivity
        time <- any(apply(plotageselex[-c(1,nrow(plotageselex)),ageselexcols],2,function(x){any(x!=x[1])}))      
        if(time){
          if((min(as.numeric(as.vector(t(plotageselex[,-(1:7)])))) < 1)){
            x <- seq(0,accuage,by=1)
            y <- as.numeric(plotageselex$year)
            z <- plotageselex[,-(1:7)]
            z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
            z <- t(z)
            main <- paste(sextitle1,"varying selectivity for ", fleetnames[i],sep="")
            if(plot){
              if(11 %in% subplot) persp(x,y,z,col="white",xlab=labels[2],ylab=labels[3],zlab=ylab,expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
              if(12 %in% subplot) contour(x,y,z,nlevels=5,xlab=labels[2],main=main,cex.main=cex.main,col=ians_blues,lwd=lwd)}
            if(print){
              if(11 %in% subplot){
                file <- paste(plotdir,"sel11_timevary_surf_flt",i,"sex",m,".png",sep="")
                caption <- main
                plotinfo <- pngfun(file=file, caption=caption)
                persp(x,y,z,col="white",xlab=labels[2],ylab=labels[3],zlab=ylab,expand=0.5,box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
                dev.off()
              }
              if(12 %in% subplot){
                file <- paste(plotdir,"sel12_timevary_contour_flt",i,"sex",m,".png",sep="")
                caption <- main
                plotinfo <- pngfun(file=file, caption=caption)
                contour(x,y,z,nlevels=5,xlab=labels[2],main=main,cex.main=cex.main,col=ians_blues,lwd=lwd)
                dev.off()
              }
            }
            plotageselex2 <- plotageselex[plotageselex$year %in% c(max(as.numeric(plotageselex$year))),]
            plotageselex2 <- plotageselex2[,-(1:7)]
            main <- paste(sextitle2," year selectivity for ", fleetnames[i],sep="")
            endselfunc <- function(){
              if(!add) plot((as.numeric(names(plotageselex2))),(as.numeric(paste(c(plotageselex2)))),
                            xlab=labels[2],ylim=c(0,1),main=main,cex.main=cex.main,ylab=ylab,
                            type="n",col=col2,cex=1.1)
              lines((as.numeric(names(plotageselex2))),(as.numeric(paste(c(plotageselex2)))),
                    type="o",col=col2,cex=1.1)
              abline(h=0,col="grey")
            }
            if(13 %in% subplot){
              if(plot) endselfunc()
              if(print){
                file <- paste(plotdir,"sel13_age_flt",i,"sex",m,".png",sep="")
                caption <- main
                plotinfo <- pngfun(file=file, caption=caption)
                endselfunc()
                dev.off()
              }
            }
          }
        }
        if(!time){
          plotageselex <- plotageselex[plotageselex$year==endyr,]
          plotageselex <- plotageselex[,-(1:7)]
          vals <- as.numeric(paste(c(plotageselex)))
          doplot <- diff(range(vals))!=0
          if(doplot & skipAgeSelex10) doplot <- !(vals[1]==0 & all(vals[-1]==1))
          #
          if(doplot){
            main <- paste(sextitle2," year selectivity for ", fleetnames[i],sep="")
            endselfunc2 <- function(){
              if(!add)
                plot((as.numeric(names(plotageselex))),vals,xlab=labels[2],ylim=c(0,1),
                     main=main,cex.main=cex.main,ylab=ylab,type="n")
              lines((as.numeric(names(plotageselex))),vals,type="o",col=col2,cex=1.1)
              abline(h=0,col="grey")
            }
            if(14 %in% subplot){
              if(plot) endselfunc2()
              if(print){
                file <- paste(plotdir,"sel14_age_flt",i,"sex",m,".png",sep="")
                caption <- main
                plotinfo <- pngfun(file=file, caption=caption)
                endselfunc2()
                dev.off()
              }
            }
          } # end if
        } # end if not time varying
      } # sexes
    } # fleets
    flush.console()
  } # factor (Asel vs. Asel2)
  
  ################################################################################
  ### Selectivity contours over age and length shown with growth curve
  
  if(21 %in% subplot & ngpatterns==1){ # need to connect growth patterns to fleets in future
    # subsetting for one season only. This could be replaced
    #   by info on the growth within the season when each fleet operates.
    growdat <- growdat[growdat$Seas==season,]
    if(nseasons>1) cat("Warning: plots showing growth curve with selectivity are using season",season,"growth,\nwhich may not match the timing of the fishery.\n")

    # Mid year mean length at age with 95% range of lengths (by sex if applicable)
    growdatF <- growdat[growdat$Gender==1 & growdat$Morph==mainmorphs[1],]
    growdatF$Sd_Size <- growdatF$SD_Mid
    if(growthCVtype=="logSD=f(A)"){ # lognormal distribution of length at age
      growdatF$high <- qlnorm(0.975, meanlog=log(growdatF$Len_Mid), sdlog=growdatF$Sd_Size)
      growdatF$low  <- qlnorm(0.025, meanlog=log(growdatF$Len_Mid), sdlog=growdatF$Sd_Size)
    }else{                        # normal distribution of length at age
      growdatF$high <- qnorm(0.975, mean=growdatF$Len_Mid, sd=growdatF$Sd_Size)
      growdatF$low  <- qnorm(0.025, mean=growdatF$Len_Mid, sd=growdatF$Sd_Size)
    }
    if(nsexes > 1){
      growdatM <- growdat[growdat$Gender==2 & growdat$Morph==mainmorphs[2],]
      growdatM$Sd_Size <- growdatM$SD_Mid
      if(growthCVtype=="logSD=f(A)"){ # lognormal distribution of length at age
        growdatM$high <- qlnorm(0.975, meanlog=log(growdatM$Len_Mid), sdlog=growdatM$Sd_Size)
        growdatM$low  <- qlnorm(0.025, meanlog=log(growdatM$Len_Mid), sdlog=growdatM$Sd_Size)
      }else{                        # normal distribution of length at age
        growdatM$high <- qnorm(0.975, mean=growdatM$Len_Mid, sd=growdatM$Sd_Size)
        growdatM$low  <- qnorm(0.025, mean=growdatM$Len_Mid, sd=growdatM$Sd_Size)
      }
    }

    xlab <- labels[2]
    ylab <- labels[1]
    zlab <- labels[4]
    for(i in fleets)
    {
      for(m in sexes)
      {
        if(m==1 & nsexes==1) sextitle2 <- "Ending"
        if(m==1 & nsexes==2) sextitle2 <- "Female ending"
        if(m==2) sextitle2 <- "Male ending"
        plotlenselex <- as.numeric(sizeselex[sizeselex$Factor=="Lsel" & sizeselex$year==endyr & sizeselex$Fleet==i & sizeselex$gender==m,-(1:5)])
        # test if there is any length-based selectivity (otherwise plot is uninformative)
        if(any(plotlenselex!=1)){ 
          plotageselex <- as.numeric(ageselex[ageselex$factor=="Asel" & ageselex$year==endyr & ageselex$fleet==i & ageselex$gender==m,-(1:7)])
          # x here should probably be replaced by $Age_Mid or some more informative value
          x <- seq(0,accuage,by=1)
          y <- lbinspop
          z <- plotageselex %o% plotlenselex # outer product of age- and length-selectivity
          
          main <- paste(sextitle2," year selectivity and growth for ", fleetnames[i],sep="")
    
          agelenselcontour <- function(){
            contour(x,y,z,nlevels=5,xlab=xlab,ylab=ylab,
                    main=main,cex.main=cex.main,col=ians_blues,lwd=lwd)
            if(m==1){
              lines(x,growdatF$Len_Mid,col='white',lwd=5)
              lines(x,growdatF$Len_Mid,col=col1,lwd=3)
              lines(x,growdatF$high,col='white',lwd=1,lty=1)
              lines(x,growdatF$high,col=col1,lwd=1,lty="dashed")
              lines(x,growdatF$low,col='white',lwd=1,lty=1)
              lines(x,growdatF$low,col=col1,lwd=1,lty="dashed")
            }
            if(m==2){
              lines(x,growdatM$Len_Mid,col='white',lwd=5)
              lines(x,growdatM$Len_Mid,col=col2,lwd=3)
              lines(x,growdatM$high,col='white',lwd=1,lty=1)
              lines(x,growdatM$high,col=col2,lwd=1,lty="dashed")
              lines(x,growdatM$low,col='white',lwd=1,lty=1)
              lines(x,growdatM$low,col=col2,lwd=1,lty="dashed")
            }
          }
          if(plot){
            if(21 %in% subplot) agelenselcontour()
          }
          if(print){
            if(21 %in% subplot){
              file=paste(plotdir,"sel21_agelen_contour_flt",i,"sex",m,".png",sep="")
              caption <- main
              plotinfo <- pngfun(file=file, caption=caption)
              agelenselcontour()
              dev.off()
            }
          }
        } # if there is any length-based selectivity
      } # sexes
    } # fleets
  } # end subplot

  ################################################################################
  ### Plot selectivity with uncertainty if "Extra SD reporting" requested in control file
  
  if(22 %in% subplot){
    # get values from Extra SD reporting if created by request at bottom of control file
    rows <- grep("Selex_std",derived_quants$LABEL)
    if(length(rows)>0){
      sel <- derived_quants[rows,]
      names <- sel$LABEL
      splitnames <- strsplit(names,"_")
      namesDF <- as.data.frame(matrix(unlist(strsplit(names,"_")),ncol=6,byrow=T))
      sel$fleet   <- as.numeric(as.character(namesDF$V3))
      sel$sex     <- as.character(namesDF$V4)
      sel$agelen  <- as.character(namesDF$V5)
      sel$bin     <- as.numeric(as.character(namesDF$V6))
      sel$lower   <- pmax(qnorm(0.025, mean=sel$Value, sd=sel$StdDev),0) # trim at 0
      sel$upper   <- pmin(qnorm(0.975, mean=sel$Value, sd=sel$StdDev),1) # trim at 1
      i <- sel$fleet[1]
      agelen <- sel$agelen[1]
      xlab <- labels[1:2][1 + (sel$agelen[1]=="A")] # decide label between length and age

      for(m in intersect(unique(sel$sex),c("Fem","Mal")[sexes])){
        seltemp <- sel[sel$sex==m,]
        if(m=="Fem" & nsexes==1) sextitle3 <- ""
        if(m=="Fem" & nsexes==2) sextitle3 <- "females"
        if(m=="Mal") sextitle3 <- "males"
        main <- paste("Uncertainty in selectivity for",fleetnames[i],sextitle3)
        no0 <- seltemp$StdDev>0.001

        if(FALSE){
          #Ian T.: this is the beginning of code to add the full selectivity line, 
          #        including bins for which no uncertainty was requested
          if(agelen=="L") plotselex <- sizeselex[sizeselex$Factor=="Lsel" & ageselex$fleet==i & sizeselex$gender==m,]
          if(agelen=="A") plotselex <- ageselex[ageselex$factor=="Asel" & ageselex$fleet==i & ageselex$gender==m,]
        }
        
        plot_extra_selex_SD <- function(){
          if(!add)
            plot(seltemp$bin,seltemp$Value,xlab=xlab,ylim=c(0,1),main=main,cex.main=cex.main,
                 ylab=labels[4],type="n",col=col2,cex=1.1,xlim=c(0,max(seltemp$bin)))
          lines(seltemp$bin,seltemp$Value,xlab=xlab,ylim=c(0,1),main=main,cex.main=cex.main,
                ylab=labels[4],type="o",col=col2,cex=1.1,xlim=c(0,max(seltemp$bin)))
          arrows(x0=seltemp$bin[no0], y0=seltemp$lower[no0], x1=seltemp$bin[no0], y1=seltemp$upper[no0],
                 length=0.01, angle=90, code=3, col=col2)
          abline(h=0,col="grey")
        }
        if(plot) plot_extra_selex_SD()
        if(print){
          file <- paste(plotdir,"sel22_uncertainty","sex",m,".png",sep="")
          caption <- main
          plotinfo <- pngfun(file=file, caption=caption)
          plot_extra_selex_SD()
          dev.off()
        }
      }
    } # end test for presence of selectivity uncertainty output
  } # check check for subplot in list
  if(!is.null(plotinfo)) plotinfo$category <- "Sel"
  return(invisible(list(infotable=infotable2,plotinfo=plotinfo)))
} 
