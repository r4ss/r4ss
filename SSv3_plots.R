SSv3_plots <- function(
    replist="ReportObject", plot=1:23, print=0, printfolder="", dir="default", fleets="all", areas="all",
    fleetcols="default", areacols="default", verbose=T, uncertainty=T,
    forecastplot=F, datplot=F, Natageplot=T, samplesizeplots=T, compresidplots=T,
    sprtarg=0.4, btarg=0.4, minbthresh=0.25, pntscalar=2.6, minnbubble=8, aalyear=-1, aalbin=-1,
    aalresids=F, maxneff=5000, smooth=T, showsampsize=T,showeffN=T,
    pwidth=7, pheight=7, punits="in", ptsize=12, res=300, cex.main=1,
    maxrows=6, maxcols=6, maxrows2=2, maxcols2=4, fixdims=T,...)
{
################################################################################
#
# SSv3_plots BETA May 14, 2009.
#
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: To sumarize the results of an SSv3 model run.
# Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
#          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
# Returns: Plots with plot history in R GUI and/or .png files.
# General: Updated for Stock Synthesilatagebases version 3.03A; R version 2.8.1
# Notes:   See users guide for documentation.
# Required SS3v_output function and plotrix package
# Credit:  Based loosely on an early version of "Scape" (A. Magnusson) and "Output viewer" (R. Methot)
#
################################################################################

  codedate <- "May 13, 2009"

  if(verbose){
    print(paste("R function updated:",codedate),quote=F)
    print("Check for new code and report problems at http://code.google.com/p/r4ss/",quote=F)
  }

#################################################################################
## embedded functions
#################################################################################

  bubble3 <- function (x,y,z,col=c(1,1),maxsize=3,do.sqrt=TRUE,
                       main="",cex.main=1,xlab="",ylab="",minnbubble=8,
                       xlimextra=1,add=F,las=1,allopen=TRUE)
  {
      # vaguely based on bubble() from gstat
      az <- abs(z)
      if (do.sqrt) az <- sqrt(az)
      cex <- maxsize * az/max(az)
      z.col <- ifelse(z < 0, col[1], col[2])
      xlim <- range(x)
      if(length(unique(x))<minnbubble) xlim=xlim+c(-1,1)*xlimextra
      pch <- z
      pch[pch==0] <- NA
      pch[pch>0] <- 16
      pch[pch<0] <- 1
      if(allopen) pch[!is.na(pch)] <- 1
      if(!add){
          plot(x,y,type='n',xlim=xlim,main=main,xlab=xlab,ylab=ylab,axes=F,cex.main=cex.main)
          axis(1,at=unique(x))
          axis(2,las=las)
          box()
      }
      points(x,y,pch=pch,cex=cex,col=z.col)
  }

  plotCI <- function(x,z=x,y=NULL,uiw,liw=uiw,ylo=NULL,yhi=NULL,...,sfrac = 0.01)
  {
    # Written by Venables; modified for access to ylim and contents
    if(is.list(x))
    { y <- x$y
      x <- x$x}
    if(is.null(y))
    { if(is.null(x)){stop("both x and y NULL")}
      y <- as.numeric(x)
      x <- seq(along = x)}
    ui <- y + uiw
    li <- y - liw
    ylim <- range(c(y,ui,li,ylo,yhi,z))
    plot(x,y,ylim=ylim,...)
    smidge <- diff(par("usr")[1:2]) * sfrac
    segments(x,li,x,ui)
    x2 <- c(x, x)
    ul <- c(li, ui)
    segments(x2 - smidge, ul, x2 + smidge, ul)
    invisible(list(x = x, y = y))
  }

  rich.colors.short <- function(n){
    # a subset of rich.colors by Arni Magnusson from the gregmisc package
    x <- seq(0, 1, length = n)
    r <- 1/(1 + exp(20 - 35 * x))
    g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
    b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
    rgb.m <- matrix(c(r, g, b), ncol = 3)
    rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3]))
  }

  matchfun2 <- function(string1,adjust1,string2,adjust2,cols=NA,matchcol1=1,matchcol2=1,
                        objmatch=rawrep,objsubset=rawrep,substr1=TRUE,substr2=TRUE)
  {
      # return a subset of values from the report file (or other file)
      # subset is defined by character strings at the start and end, with integer
      # adjustments of the number of lines to above/below the two strings
      line1 <- match(string1,if(substr1){substring(objmatch[,matchcol1],1,nchar(string1))}else{objmatch[,matchcol1]})
      line2 <- match(string2,if(substr2){substring(objmatch[,matchcol2],1,nchar(string2))}else{objmatch[,matchcol2]})
      if(!is.na(cols[1])){ out <- objsubset[(line1+adjust1):(line2+adjust2),cols]
                       }else{            out <- objsubset[(line1+adjust1):(line2+adjust2), ]}
      return(out)
  }

  # get quantities from the big list
  if(replist[[1]]=="ReportObject"){
    return("The input 'replist' should refer to an R object created by the function 'SSv3_output'.")
  }

  nfleets                        <- replist$nfleets
  nfishfleets                    <- replist$nfishfleets
  nsexes                         <- replist$nsexes
  lbins                          <- replist$lbins
  nlbins                         <- replist$nlbins
  lbinspop                       <- replist$lbinspop
  nlbinspop                      <- replist$nlbinspop
  agebins                        <- replist$agebins
  nagebins                       <- replist$nagebins
  accuage                        <- replist$accuage
  nareas                         <- replist$nareas
  startyr                        <- replist$startyr
  endyr                          <- replist$endyr
  nseasons                       <- replist$nseasons
  seasfracs                      <- replist$seasfracs
  nforecastyears                 <- replist$nforecastyears
  morph_indexing                 <- replist$morph_indexing
  biology                        <- replist$biology
  endgrowth                      <- replist$endgrowth
  growthseries                   <- replist$growthseries
  sizeselex                      <- replist$sizeselex
  retention                      <- replist$retention
  ageselex                       <- replist$ageselex
  timeseries                     <- replist$timeseries
  F_method                       <- replist$F_method
  depletion_basis                <- replist$depletion_basis
  depletion_level                <- replist$depletion_level
  mnwgt                          <- replist$mnwgt
  sprseries                      <- replist$sprseries
  recruit                        <- replist$recruit
  cpue                           <- replist$cpue
  natage                         <- replist$natage
  movement                       <- replist$movement
  ALK                            <- replist$ALK
  AAK                            <- replist$AAK
  compdbase                      <- replist$composition_database
  derived_quants                 <- replist$derived_quants
  parameters                     <- replist$parameters
  FleetNames                     <- replist$FleetNames
  CoVar                          <- replist$CoVar
  stdtable                       <- replist$stdtable
  #rawstd                         <- replist$rawstd
  SS_version                     <- replist$SS_version
  Run_time                       <- replist$Run_time
  Files_used                     <- replist$Files_used
  used_likelihoods               <- replist$used_likelihoods
  raw_likelihoods_by_fleet       <- replist$raw_likelihoods_by_fleet
  variance_adjustments_by_fleet  <- replist$variance_adjustments_by_fleet
  N_estimated_parameters         <- replist$N_estimated_parameters
  estimated_non_rec_devparameters <- replist$estimated_non_rec_devparameters
  log_det_hessian                <- replist$log_det_hessian
  maximum_gradient_component     <- replist$maximum_gradient_component
  sigma_R_in                     <- replist$sigma_R_in
  sigma_R_out                    <- replist$sigma_R_out
  SBzero                         <- replist$SBzero
  current_depletion              <- replist$current_depletion
  #fmax                           <- replist$fmax
  #endyrcatch                     <- replist$endyrcatch
  #endyrlandings                  <- replist$endyrlandings
  #endyrspr                       <- replist$endyrspr
  last_years_sprmetric           <- replist$last_years_sprmetric
  inputs                         <- replist$inputs
  managementratiolabels          <- replist$managementratiolabels
  equil_yield 			 <- replist$equil_yield

  # check for internal consistency
  if(uncertainty==T & inputs$covar==F)
    return("To use uncertainty=T, you need to have covar=T in the input to the SSv3_output function")
  if(forecastplot==T & inputs$forecast==F)
    return("To use forecastplot=T, you need to have forecast=T in the input to the SSv3_output function")

  # derived quantities
  mainmorphs <- morph_indexing$Index[morph_indexing$Bseas==1]
  FleetNumNames <- paste(1:nfleets,FleetNames,sep="_")
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

  # time series (but no forecast) quantities used for multiple plots
  timeseries$Yr <- timeseries$Yr + (timeseries$Seas-1)/nseasons
  ts <- timeseries[timeseries$Yr <= endyr+1,]
  tsyears <- ts$Yr[ts$Seas==1]
  tsarea <- ts$Area[ts$Seas==1]
  tsspaw_bio <- ts$SpawnBio[ts$Seas==1]
  if(nsexes==1) tsspaw_bio <- tsspaw_bio/2
  dep <- tsspaw_bio/tsspaw_bio[1]
  tsall <- timeseries
  tsallyears <- tsall$Yr[tsall$Seas==1]
  tsallarea <- tsall$Area[tsall$Seas==1]
  tsallspaw_bio <- tsall$SpawnBio[tsall$Seas==1]
  if(nsexes==1) tsallspaw_bio <- tsallspaw_bio/2
  depall <- tsallspaw_bio/tsallspaw_bio[1]

  if(verbose) print("Finished defining objects",quote=F)
  if(nareas>1){
    print(paste("! Warning: some plots are not configured for mult-area models (nareas=",nareas,")",sep=""),quote=F)
    areanames <- paste("Area",1:nareas)
  }

  #### prepare for plotting
  if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
  # make plot window (operating system specific)
  nplots <- length(intersect(1:25,plot))
  nprints <- length(intersect(1:25,print))

  if(length(grep('linux',version$os)) > 0) OS <- "Linux"
  if(length(grep('mingw',version$os)) > 0) OS <- "Windows"
  # need appropriate line to support Mac operating systems

  if(nplots>0){
    if(OS=="Windows") windows(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
    if(OS=="Linux") X11(width=pwidth,height=pheight,pointsize=ptsize)
    if(OS=="Mac") quartz(width=pwidth,height=pheight,pointsize=ptsize)
    plotdir <- NULL
  }
  if(nprints>0){
    if(dir=="default") dir <- inputs$dir
    dir.create(dir,showWarnings=F)
    plotdir <- paste(dir,printfolder,"/",sep="")
    dir.create(plotdir,showWarnings=F)
    if(verbose) print(paste("Plots specified by 'print' will be written to",plotdir),quote=F)
  }

  # colors
  ians_blues <- c("white","grey","lightblue","skyblue","steelblue1","slateblue",topo.colors(6),"blue","blue2","blue3","blue4","black")
  ians_contour <- c("white",rep("blue",100))
  if(fleetcols[1]=="default"){
    fleetcols <- rich.colors.short(nfleets)
    if(nareas==3) areacols <- rainbow(nareas)
  }
  if(areacols[1]=="default"){
    areacols  <- rich.colors.short(nareas)
    if(nfleets==3) fleetcols <- rainbow(nareas)
  }

  #### plot 1
  # Static growth (mean weight, maturity, fecundity, spawning output)
  if(1 %in% c(plot, print))
  {
    growdat <- replist$endgrowth
    xlab <- "Length (cm)"
    x <- biology$Mean_Size
    ylab <- "Mean weight (kg) in last year"
    ylab2 <- "Spawning output"
    fec_ylab <- "Eggs per gram"
    fec_xlab <- "Female weight (kg)"
    gfunc1 <- function(){
      plot(x,biology$Wt_len_F,xlab=xlab,ylab=ylab,type="o",col="red")
      abline(h=0,col="grey")
      if(nsexes > 1){
        lines(x,biology$Wt_len_M,col="blue",type="o")
        legend("topleft",bty="n", c("Females","Males"), lty=1, col = c("red","blue"))}}
    gfunc2 <- function(){
      if(min(biology$Mat_len)<1){ plot(x,biology$Mat_len,xlab="Length (cm)",ylab="Maturity",type="o",col="red")
      }else{ plot(growdat$Age, growdat$Age_Mat,xlab="Age",ylab="Maturity",type="o",col="red") }
      abline(h=0,col="grey")}
    gfunc4 <- function(){
      par1 <- parameters[substr(parameters[,2],1,nchar("Eg/gm_inter_Fem"))=="Eg/gm_inter_Fem",3]
      par2 <- parameters[substr(parameters[,2],1,nchar("Eg/gm_slope_wt_Fem"))=="Eg/gm_slope_wt_Fem",3]
      ymin <- 0
      ymax <- max(1.1*(par1 + par2*biology$Wt_len_F))
      plot(biology$Wt_len_F, (par1 + par2*biology$Wt_len_F),xlab=fec_xlab,ylab=fec_ylab,ylim=c(ymin,ymax),col="blue",pch=19)
      lines(biology$Wt_len_F,rep(par1,length(biology$Wt_len_F)),col="red")
      text((max(biology$Wt_len_F)-min(biology$Wt_len_F))/2,par1-0.02*ymax,"Egg output proportional to spawning biomass")
      }
    gfunc3 <- function(){
      plot(x,biology$Spawn,xlab="Length (cm)",ylab=ylab2,type="o",col="red")
      abline(h=0,col="grey")}
    if(1 %in% plot){
      gfunc1()
      gfunc2()
      gfunc4()
      gfunc3()}
    if(1 %in% print){
      png(file=paste(plotdir,"01_weightatsize.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      gfunc1()
      dev.off()
      png(file=paste(plotdir,"01_maturity.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      gfunc2()
      dev.off()
      png(file=paste(plotdir,"01_fecundity.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      gfunc4()
      dev.off()
      png(file=paste(plotdir,"01_spawningoutput.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      gfunc3()
      dev.off()}

    # Mid year mean length at age with 95% range of lengths (by sex if applicable)
    growdatF <- growdat[growdat$Gender==1 & growdat$Morph==min(growdat$Morph[growdat$Gender==1]),]
    #growdatF <- growdat[growdat$Morph==mainmorphs[1],]
    growdatF$Sd_Size <- growdatF$SD_Mid
    growdatF$high <- growdatF$Len_Mid + 1.96*growdatF$Sd_Size
    growdatF$low <- growdatF$Len_Mid - 1.96*growdatF$Sd_Size
    if(nsexes > 1){
        growdatM <- growdat[growdat$Gender==2 & growdat$Morph==min(growdat$Morph[growdat$Gender==2]),]
        #growdatM <- growdat[growdat$Morph==mainmorphs[2],]
        xm <- growdatM$Age
        growdatM$Sd_Size <- growdatM$SD_Mid
        growdatM$high <- growdatM$Len_Mid + 1.96*growdatM$Sd_Size
        growdatM$low <- growdatM$Len_Mid - 1.96*growdatM$Sd_Size
     }
    maxy <- max(growdatF$high)
    if(nsexes > 1){maxy <- max(maxy,growdatM$high)}
    x <- growdatF$Age
    header <- "Ending year expected growth"
   # if(nseasons > 1){header <- paste(header," season 1",sep="")}
    ylab <- "Length (cm, middle of the year)"
    gfunc4 <- function()
    {
      plot(x,growdatF$Len_Mid,col="red",lwd=2,ylim=c(0,maxy),type="l",ylab=ylab,xlab="Age (yr)")
      abline(h=0,col="grey")
      lines(x,growdatF$high,col="red",lwd=1,lty="dashed")
      lines(x,growdatF$low,col="red",lwd=1,lty="dashed")
      mtext(header,3,1.5)
      if(nsexes > 1)
      {
        lines(xm,growdatM$Len_Mid,col="blue",lwd=2,type="l")
        lines(xm,growdatM$high,col="blue",lwd=1,lty="dashed")
        lines(xm,growdatM$low,col="blue",lwd=1,lty="dashed")
        grid()
        legend("topleft",bty="n", c("Females","Males"), lty=1, col = c("red","blue"))
      }
    }
    if(1 %in% plot) gfunc4()
    if(1 %in% print){
      png(file=paste(plotdir,"01_sizeatage.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      gfunc4()
      dev.off()}

    # Natural mortality (if time or sex varying)
    M <- growdatF$M
    if(min(M)!=max(M))
    {
      ymax <- max(M)
      ylab <- "Natural mortality"
      mfunc <- function()
      {
        plot(growdatF$Age,M,col="red",lwd=2,ylim=c(0,ymax),type="o",ylab=ylab,xlab="Age (yr)")
        abline(h=0,col="grey")
        if(nsexes > 1){
          growdatM <- growdat[growdat$Morph==mainmorphs[2],]
           lines(growdatM$Age,growdatM$M,col="blue",lwd=2,type="o")
        }
        # return(head(growdatF))
      }
      if(1 %in% plot) mfunc()
      if(1 %in% print){
        png(file=paste(plotdir,"01_natmort.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        mfunc()
        dev.off()}
    }
    if(verbose) print("Finished plot 1: Static growth (mean weight, maturity, spawning output)",quote=F)
  } # end if 1 in plot or print

  ### plot 2: Time-varying growth
if(nseasons == 1){ # temporarily disable multi-season plotting of time-varying growth
  if(2 %in% c(plot, print))
  {
    if(is.null(growthseries))
    {
      print("! Warning: no time-varying growth info because 'detailed age-structured reports' turned off in starter file.",quote=F)
    }else{
      for(i in 1:nsexes)
      {
        growdatuse <- growthseries[growthseries$Morph==mainmorphs[i],]
        x <- seq(0,accuage,by=1)
        y <- seq(startyr,endyr-1,by=1)
        z <- growdatuse[,-(1:4)]
        z <- as.matrix(z)
        time <- FALSE
        for(t in 1:ncol(z)) if(max(z[,t])!=min(z[,t])) time <- T
        if(time)
        {
          z <- t(z)
          if(i==1){main <- "Female time-varying growth"}
          if(nsexes==1){main <- "Time-varying growth"}
          if(i==2){main <- "Male time-varying growth"}
          if(nseasons > 1){main <- paste(main," season 1",sep="")}
          if(2 %in% plot){
            persp(x,y,z,col="white",xlab="Age (yr)",ylab="",zlab="Length (cm)",expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
            contour(x,y,z,nlevels=12,xlab="Age (yr)",main=main,col=ians_contour,lwd=2)}
          if(2 %in% print){
            png(file=paste(plotdir,"02_timevarygrowthsurf",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            persp(x,y,z,col="white",xlab="Age (yr)",ylab="",zlab="Length (cm)",expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
            dev.off()
            png(file=paste(plotdir,"02_timevarygrowthcontour",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            contour(x,y,z,nlevels=12,xlab="Age (yr)",main=main,col=ians_contour,lwd=2)
            dev.off()}
        }
      }
    }
    if(verbose) print("Finished plot 2: Time-varying growth",quote=F)
  } # end if 2 in plot or print
} # end temporary disable of time-varying growth for multi-season models

  ### plots 3 and 4 selectivity and retention
  # Length selex and retention
  if(length(intersect(c(3,4), c(plot,print)))>0)
  {
    for(m in 1:nsexes)
    {
      if(m==1 & nsexes==1) sextitle1 <- "Time-"
      if(m==1 & nsexes==2) sextitle1 <- "Female time-"
      if(m==2) sextitle1 <- "Male time-"
      if(m==1 & nsexes==1) sextitle2 <- "Ending"
      if(m==1 & nsexes==2) sextitle2 <- "Female ending"
      if(m==2) sextitle2 <- "Male ending"
      intret <- sizeselex[sizeselex$Factor=="Ret" & sizeselex$gender==m,]
      intselex <- sizeselex[sizeselex$Factor=="Lsel" & sizeselex$gender==m,]
      for(i in fleets)
      {
        plotselex <- intselex[intselex$Fleet==i,]
        time <- FALSE
        for(t in 5 + 1:nlbinspop) if(length(unique(plotselex[,t]))>1){time <- TRUE}
        if(m==1 & nsexes==1) sextitle <- "Time-"
        if(m==1 & nsexes==2) sextitle <- "Female time-"
        if(m==2) sextitle="Male time-"
        if(time)
        {
          x <- lbinspop
          y <- plotselex$year
          z <- plotselex[,-(1:5)]
          z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
          z <- t(z)
          main <- paste(sextitle1,"varying selectivity for ", FleetNames[i],sep="")
          if(3 %in% plot)
          { persp(x,y,z,col="white",xlab="Length (cm)",ylab="Year",zlab="Selectivity",expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
            contour(x,y,z,nlevels=5,xlab="Length (cm)",ylab="Year",main=main,col=ians_blues,lwd=2)}
          if(3 %in% print)
          { png(file=paste(plotdir,"03_timevarylenselsurf_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            persp(x,y,z,col="white",xlab="Length (cm)",ylab="Year",zlab="Selectivity",expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
            dev.off()
            png(file=paste(plotdir,"03_timevarylenselcontour_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            contour(x,y,z,nlevels=5,xlab="Length (cm)",ylab="Year",main=main,col=ians_blues,lwd=2)
            dev.off()}
        }
        time2 <- FALSE
        for(t in 5 + 1:nlbinspop) if(length(unique(intret[intret$Fleet==i,t]))>1){time2 <- TRUE}
        if(time2)
        {
          x <- lbinspop
          y <- intret$year[intret$Fleet==i]
          z <- intret[intret$Fleet==i,-(1:5)]
          z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
          z <- t(z)
          main <- paste(sextitle1,"varying selectivity for ", FleetNames[i],sep="")
          if(3 %in% plot)
          { persp(x,y,z,col="white",xlab="Length (cm)",ylab="Year",zlab="Retention",expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
            contour(x,y,z,nlevels=5,xlab="Length (cm)",ylab="Year",main=main,col=ians_blues,lwd=2)}
          if(3 %in% print)
          { png(file=paste(plotdir,"03_timevaryretsurf_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            persp(x,y,z,col="white",xlab="Length (cm)",ylab="Year",zlab="Retention",expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
            dev.off()
            png(file=paste(plotdir,"03_timevaryretcontour_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            contour(x,y,z,nlevels=5,xlab="Length (cm)",ylab="Year",main=main,col=ians_blues,lwd=2)
            dev.off()}
        }
        plotselex <- plotselex[plotselex$year==endyr & plotselex$gender==m,-(1:5)]
        ylab <- "Selectivity and retention"
        bins <- as.numeric(names(plotselex))
        vals <- as.numeric(paste(plotselex))
        main <- paste(sextitle2," year selectivity for ", FleetNames[i],sep="")
        selfunc <- function()
        {
          plot(bins,vals,xlab="Length bin (cm)",ylim=c(0,1),main=main,ylab=ylab,type="o",col="blue",cex=1.1)
          abline(h=0,col="grey")
          intret2 <- intret[intret$Fleet==i,]
          retchecktemp <- as.vector(unlist(intret2[1,]))
          retcheck <- as.numeric(retchecktemp[6:length(retchecktemp)])
          if(is.na(sum(retcheck))) retcheckuse <- 0
          if(!is.na(sum(retcheck))) retcheckuse <- max(retcheck)-min(retcheck)
          if(retcheckuse==0 & max(vals)-min(vals)!=0) legend("bottomright",inset=c(0,0.05),bty="n","Selectivity",pch=21,pt.bg="white",lty=1,col="blue")
          if(retcheckuse > 0){
            useret <- intret[intret$Fleet==i,]
            plotret <- useret[useret$year==max(as.numeric(useret$year)),]
            lines((as.numeric(as.vector(names(plotret)[-(1:5)]))),(as.numeric(as.character(plotret[1,-(1:5)]))),col="red",type="l",cex=1.1)
            legend("bottomright",inset=c(0,0.05),bty="n", c("Selectivity","Retention"), lty=1, col = c("blue","red"))}
        }
        if(max(vals) - min(vals) != 0) # only make plot of selectivity is not constant
        {
          if(4 %in% plot) selfunc()
          if(4 %in% print){
            png(file=paste(plotdir,"04_lenselex_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            selfunc()
            dev.off()}
        }
      } # fleets
    } # sexes
    if(verbose) print("Finished length based selectivity plots (subset of plots 3 and 4)",quote=F)

    # Age based selex
    ylab <- "Selectivity"
    for(m in 1:nsexes)
    {
      if(m==1 & nsexes==1) sextitle1 <- "Time-"
      if(m==1 & nsexes==2) sextitle1 <- "Female time-"
      if(m==2) sextitle1 <- "Male time-"
      if(m==1 & nsexes==1) sextitle2 <- "Ending"
      if(m==1 & nsexes==2) sextitle2 <- "Female ending"
      if(m==2) sextitle2 <- "Male ending"
      for(i in fleets)
      {
        plotageselex <- ageselex[ageselex$factor=="Asel" & ageselex$fleet==i & ageselex$gender==m,]
        time <- FALSE
        for(t in (1:ncol(ageselex))[names(ageselex) %in% as.character(0:accuage)]){
          if(length(unique(plotageselex[,t]))>1){time <- TRUE} }
        if(time)
        {
          if((min(as.numeric(as.vector(t(plotageselex[,-(1:7)])))) < 1))
          {
            x <- seq(0,accuage,by=1)
            y <- as.numeric(plotageselex$year)
            z <- plotageselex[,-(1:7)]
            z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
            z <- t(z)
            main <- paste(sextitle1,"varying selectivity for ", FleetNames[i],sep="")
            if(3 %in% plot){
              persp(x,y,z,col="white",xlab="Age (yr)",ylab="Year",zlab=ylab,expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
              contour(x,y,z,nlevels=5,xlab="Age (yr)",main=main,col=ians_blues,lwd=2)}
            if(3 %in% print){
              png(file=paste(plotdir,"03_timevaryageselsurf_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              persp(x,y,z,col="white",xlab="Age (yr)",ylab="Year",zlab=ylab,expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
              dev.off()
              png(file=paste(plotdir,"03_timevaryageselcontour_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              contour(x,y,z,nlevels=5,xlab="Age (yr)",main=main,col=ians_blues,lwd=2)
              dev.off()}
            plotageselex2 <- plotageselex[plotageselex$year %in% c(max(as.numeric(plotageselex$year))),]
            plotageselex2 <- plotageselex2[,-(1:7)]
            main <- paste(sextitle2," year selectivity for ", FleetNames[i],sep="")
            endselfunc <- function()
             {plot((as.numeric(names(plotageselex2))),(as.numeric(paste(c(plotageselex2)))),xlab="Age (yr)",ylim=c(0,1),main=main,ylab=ylab,type="o",col="blue",cex=1.1)
             abline(h=0,col="grey")
             legend("bottomright",inset=c(0,0.05),bty="n","Selectivity",pch=21,pt.bg="white",lty=1,col="blue")}
            if(4 %in% plot){endselfunc()}
            if(4 %in% print)
             {png(file=paste(plotdir,"04_ageselex_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
             endselfunc()
             dev.off()}
           }
         }
        if(!time)
        {
          plotageselex <- plotageselex[plotageselex$year==endyr,]
          plotageselex <- plotageselex[,-(1:7)]
          vals <- as.numeric(paste(c(plotageselex)))
          if(!(((max(vals))-(min(vals)))==0))
          {
            main <- paste(sextitle2," year selectivity for ", FleetNames[i],sep="")
            endselfunc2 <- function(){
              plot((as.numeric(names(plotageselex))),vals,xlab="Age (yr)",ylim=c(0,1),main=main,ylab=ylab,type="o",col="blue",cex=1.1)
              abline(h=0,col="grey")
              legend("bottomright",inset=c(0,0.05),bty="n","Selectivity",pch=21,pt.bg="white",lty=1,col="blue")}
            if(4 %in% plot) endselfunc2()
            if(4 %in% print){
              png(file=paste(plotdir,"04_ageselex_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              endselfunc2()
              dev.off()}
          }
        }
      } # fleets
    } # sexes
    flush.console()
    if(verbose) print("Finished age based selectivity plots (subset of plots 3 and 4)",quote=F)
  } # end if 3 and 4 in plot or print

  ### plot 5: Basic time series
  # stats and dimensions
  if(5 %in% c(plot, print))
  {
    # Total and summary biomass
    tbiofunc <- function(){
      tsplotyr <- ts$Yr[tsarea==1]
      tsplotbio <- ts$Bio_all[tsarea==1]
      plot(tsplotyr[2:length(tsplotyr)],tsplotbio[2:length(tsplotbio)],xlab="Year",ylim=c(0,max(ts$Bio_all)),ylab="Total biomass",type="o",col=areacols[1])
      points(tsplotyr[1],tsplotbio[1],col=areacols[1],pch=19)
      if(nareas>1){
        for(iarea in 2:nareas) lines(ts$Yr[tsarea==iarea],ts$Bio_all[tsarea==iarea],type="o",col=areacols[iarea])
        legend("topright",legend=areanames,lty=1,pch=1,col=areacols,bty="n")
      }
      abline(h=0,col="grey")
    }
    sbiofunc <- function(){
      tsplotyr <- ts$Yr[tsarea==1]
      tsplotbiosum <- ts$Bio_smry[tsarea==1]
      plot(tsplotyr[2:length(tsplotyr)],tsplotbiosum[2:length(tsplotbiosum)],xlab="Year",ylim=c(0,max(ts$Bio_smry)),ylab="Summary biomass",type="o",col=areacols[1])
      points(tsplotyr[1],tsplotbiosum[1],col=areacols[1],pch=19)
      if(nareas>1){
        for(iarea in 2:nareas) lines(ts$Yr[tsarea==iarea],ts$Bio_smry[tsarea==iarea],type="o",col=areacols[iarea])
        legend("topright",legend=areanames,lty=1,pch=1,col=areacols,bty="n")
      }
      abline(h=0,col="grey")
    }

    if(5 %in% plot){
      tbiofunc()
      sbiofunc()}
    if(5 %in% print){
      png(file=paste(plotdir,"05_totbio.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      tbiofunc()
      dev.off()
      png(file=paste(plotdir,"05_summarybio.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      sbiofunc()
      dev.off()}


    ## embedded modified function "stackpoly" by Jim Lemon from "plotrix" used in next plot
    ## see http://cran.r-project.org/web/packages/plotrix/index.html
    stackpoly <- function (x, y, main="", xlab="", ylab="", xat=NA, 
                           xaxlab=NA, xlim=NA, ylim=NA, lty=1, border=NA, 
                           col=NA, axis4=F, ...) 
      {
        ydim <- dim(y)
        x <- matrix(rep(x, ydim[2]), ncol = ydim[2])
        y <- t(unlist(apply(as.matrix(y), 1, cumsum)))
        if (is.na(xlim[1])) xlim <- range(x)
        if (is.na(ylim[1])) ylim <- c(0,max(y))
        plot(0, main = main, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim,
             type = "n", xaxs = "i", yaxs = "i", axes = T,...)
        plotlim <- par("usr")
        if (is.na(col[1])) 
          col = rainbow(ydim[2])
        else if (length(col) < ydim[2]) 
          col <- rep(col, length.out = ydim[2])
        if (length(lty) < ydim[2]) 
          lty <- rep(lty, length.out = ydim[2])
        for (pline in seq(ydim[2], 1, by = -1)) {
          if (pline == 1) {
            polygon(c(x[1], x[, pline], x[ydim[1]]),
                    c(plotlim[3], y[, pline], plotlim[3]),
                    border = border, col = col[pline], 
                    lty = lty[pline])
          }
          else polygon(c(x[, pline], rev(x[, pline - 1])), 
                       c(y[, pline], rev(y[, pline - 1])), border = border, 
                       col = col[pline], lty = lty[pline])
        }
        if (axis4)  axis(4)
      }
    ## end embedded stackpoly

    # total landings (not adapted for multi-area models)
    ls <- nrow(ts)
    totretainedmat <- as.matrix(ts[,substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
    ts$totretained <- 0
    ts$totretained[3:ls] <- rowSums(totretainedmat)[3:ls]
    polyts <- totretainedmat[3:(ls-1),]
    landfunc <- function(){
      plot(ts$Yr[2:(ls-1)],ts$totretained[2:(ls-1)],xlab="Year",ylab="Landings (mt)",type="o",col="black")
      abline(h=0,col="grey")
      for(xx in 1:nfishfleets){
        lines(ts$Yr[3:(ls-1)],totretainedmat[3:(ls-1),xx],type="l",col=fleetcols[xx])
      }
      if(nfishfleets > 1){
        ## old function call to plotrix function
        ## stackpoly(x=polyts[,(nfishfleets+1):(nfishfleets*2)],xaxlab=as.character(c(polyts[,1])),
        ##           ylim=c(0,max(ts$totretained[2:(ls-1)])),stack=T,axis4=F,
        ##         xlab="Year",ylab="Landings (mt)",col=fleetcols)

        ## call to embedded, modified function
        stackpoly(x=ts$Yr[3:(ls-1)],y=polyts,
                  xlab="Year",ylab="Landings (mt)",col=fleetcols)
      } # end if nfishfleets > 1
    } # end landfunc

    if(5 %in% plot) landfunc()
    if(5 %in% print){
      png(file=paste(plotdir,"05_landings.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      landfunc()
      dev.off()}

    # total catch (not adapted for multi-area models)
    totcatchmat <- as.matrix(ts[,substr(names(ts),1,nchar("enc(B)"))=="enc(B)"])
    ts$totcatch <- 0
    ts$totcatch[3:ls] <- rowSums(totcatchmat)[3:ls]
    ## not sure why the following line was there--why not plot when totcatch equals totretained?
    #    if(max(ts$totcatch[3:ls] - ts$totretained[3:ls] != 0))
    {
      catfunc <- function()
      {plot(ts$Yr[2:(ls-1)],ts$totcatch[2:(ls-1)],xlab="Year",ylab="Total catch (mt)",type="o",col="black")
        abline(h=0,col="grey")
      for(xx in 1:nfishfleets){lines(ts$Yr[3:(ls-1)],totcatchmat[3:(ls-1),xx],type="l",col=fleetcols[xx])}}
      if(5 %in% plot) catfunc()
      if(5 %in% print){
        png(file=paste(plotdir,"05_totcatch.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        catfunc()
      dev.off()}
    }

    # harvest rates (not adapted for multi-area models)
    ymax <- 0
    fmax <- 0
    if(F_method==1){
        stringmatch <- "Hrate:_"
        ylab <- "Harvest rate/Year"
    }else{
        stringmatch <- "F:_"
        ylab <- "Continuous F" # ?maybe should add "Hybrid F" label for F_method==3
    }
    Hrates <- as.matrix(ts[,substr(names(ts),1,nchar(stringmatch))==stringmatch])
    if(max(is.na(Hrates))==1) print("!warning: you have a bad value in the TIME_SERIES section of the report file")
    fmax <- max(Hrates[!is.na(Hrates)])
    Hratefunc <- function(){
      plot(ts$Yr[2:(ls-1)],2:(ls-1),xlab="Year",ylim=c(0,fmax),ylab=ylab,type="n")
        abline(h=0,col="grey")
        for(xx in 1:nfishfleets) lines(ts$Yr[2:(ls-1)],Hrates[2:(ls-1),xx],type="o",col=fleetcols[xx])
    }
    if(5 %in% plot) Hratefunc()
    if(5 %in% print){
      png(file=paste(plotdir,"05_harvestrates.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      Hratefunc()
      dev.off()}

    # discard series (not adapted for multi-area models)
    ts$discardall <- ts$totcatch - ts$totretained
    discardmat <- totcatchmat - totretainedmat
    if(max(is.na(ts$discardall))==1) print("!warning: you have a bad value in the TIME_SERIES section of the report file")
    if(!(max(ts$discardall[!is.na(ts$discardall)])==0))
    {
      ylab <- "Predicted Discards (mt)"
      discfunc <- function()
      {
        plot(ts$Yr[2:(ls-1)],ts$discardall[2:(ls-1)],xlab="Year",ylab=ylab,type="o",col="black")
        abline(h=0,col="grey")
        for(xx in 1:nfishfleets) lines(ts$Yr[3:(ls-1)],discardmat[3:(ls-1),xx],col=fleetcols[xx])
      }
      if(5 %in% plot){discfunc()}
      if(5 %in% print)
      {png(file=paste(plotdir,"05_discards.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        discfunc()
      dev.off()}
    }
    # Discard fraction by weight
    discfracmat <- discardmat/totcatchmat
    discfracmat[discfracmat %in% NaN] <- 0
    ts$discfrac <- 0
    ts$discfrac[3:ls] <- ts$discardall[3:ls]/ts$totcatch[3:ls]
    ts$discfrac[ts$discfrac %in% NaN] <- 0
    ymax <- max(ts$discfrac)
    if(max(is.na(ts$discfrac))==1) print("!warning: you have a bad value in the TIME_SERIES section of the report file")
    if(max(ts$discfrac[!is.na(ts$discfrac)]>0)){
      for(xx in 1:nfishfleets)
      {
        ylab <- "Discard fraction by weight"
        discfunc2 <- function()
        {
          plot(ts$Yr[3:ls],ts$discfrac[3:ls],xlab="Year",ylim=c(0,ymax),ylab=ylab,type="o",col="black")
          abline(h=0,col="grey")
          for(xx in 1:nfishfleets) lines(ts$Yr[3:ls],discfracmat[3:ls,xx],col=fleetcols[xx])
        }
        if(5 %in% plot){discfunc2()}
        if(5 %in% print)
        {png(file=paste(plotdir,"05_discardfraction.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
          discfunc2()
        dev.off()}
      }
    }
    if(verbose) print("Finished plot 5: Basic time series",quote=F)
  } # end if 5 in plot or print

  # Plot 6: recruitment (not adapted for multi-area models)
  if(6 %in% c(plot, print))
   {
    x <- ts$Yr
    y <- ts$Recruit_0
    ylab <- "Age-0 recruits (1,000s)"
    recfunc <- function(){
      plot(x[3:length(x)],y[3:length(y)],xlab="Year",ylab=ylab,xlim=c(x[1]-1,x[length(x)]+1),ylim=c(0,max(y)),type="o",col="blue")
      points(x[1],y[1],col="blue",pch=19)
      points(x[2],y[2],col="blue")
      abline(h=0,col="grey")}
    if(6 %in% plot) recfunc()
    if(6 %in% print){
      png(file=paste(plotdir,"06_recruits.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      recfunc()
      dev.off()}

    if(forecastplot){
      x <- timeseries$Yr
      y <- timeseries$Recruit_0
      ymax <- max(y)
      termyr <- min(timeseries$Yr[timeseries$Era=="FORE"])
      tsfore <- timeseries$Era=="FORE"
      recfunc2 <- function(){
        plot(x[x<=termyr+1],y[x<=termyr+1],xlab="Year",ylab=ylab,xlim=range(timeseries$Yr),ylim=c(0,max(y)),type="o",col="blue")
        abline(h=0,col="grey")
        lines(x[x>termyr],y[x>termyr],lwd=1,col="red",lty="dashed")
        points(x[x>termyr],y[x>termyr],col="red",pch=19)}
      if(6 %in% plot) recfunc2()
      if(6 %in% print){
        png(file=paste(plotdir,"06_recruitswforecast.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        recfunc2()
        dev.off()}
      }

    # recruitment with asymptotic interval
    if(uncertainty){
      recstd <- matchfun2("Recr_Virgin",0,"SPRratio",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=derived_quants,objsubset=derived_quants,substr1=TRUE,substr2=TRUE)
      recstd$Yr <- substring(recstd$LABEL,6,nchar(recstd$LABEL[1])-1)
      recstd$Yr[2] <- as.numeric(recstd$Yr[3])-1
      recstd$Yr[1] <- as.numeric(recstd$Yr[2])-1
      recstd$Yr <- as.numeric(recstd$Yr)

print(recstd)
print("ok to here")

      v <- recstd$Value
      recstd$val1 <- log(v)
      recstd$logint <- sqrt(log(1+(recstd$StdDev/v)^2))
      recstd$lower <- exp(recstd$val1 - 1.96*recstd$logint)
      recstd$upper <- exp(recstd$val1 + 1.96*recstd$logint)
      plottitle <- "~95% Asymptotic confidence interval"
      uiw <- recstd$upper - v
      liw <- v - recstd$lower
      recfunc3 <- function(maxyr){
        plotCI(x=recstd$Yr[recstd$Yr<=(endyr+1)],y=v[recstd$Yr<=(endyr+1)],sfrac=0.001,z=2,
          uiw=uiw[recstd$Yr<=maxyr],liw=liw[recstd$Yr<=maxyr],xlab="Year",ylo=0,
          col="black",ylab=ylab,lty=1,main=plottitle)
        abline(h=0,col="grey")}
      if(6 %in% plot) recfunc3(maxyr=endyr+1)
      if(6 %in% print){
        png(file=paste(plotdir,"06_recswintervals.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        recfunc3(maxyr=endyr+1)
        dev.off()}

      if(forecastplot){
      recfunc4 <- function(maxyr){
        plotCI(x=recstd$Yr,y=v,sfrac=0.001,z=2,uiw=uiw,liw=liw,xlab="Year",ylo=0,
          col="black",ylab=ylab,lty=1,main=plottitle)
        points(x=recstd$Yr[recstd$Yr>(endyr+1)],y=v[recstd$Yr>(endyr+1)],col="red",pch=19)
        abline(h=0,col="grey")}
        if(6 %in% plot) recfunc4(maxyr)
        if(6 %in% print){
          png(file=paste(plotdir,"06_recswforecastintervals.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
          recfunc4(maxyr)
          dev.off()}
      }
    } # end if uncertainty
    if(verbose) print("Finished plot 6: Recruitment",quote=F)
  } # end if 6 in plot or print

  # Plot 7: spawning biomass
  if(7 %in% c(plot, print))
  {
    sbfunc <- function(timeseries,forecastplot){
      tsplotyr <- timeseries$Yr[!is.na(timeseries$SpawnBio)]
      tsplotSB <- timeseries$SpawnBio[!is.na(timeseries$SpawnBio)]
      tsplotarea <- timeseries$Area[!is.na(timeseries$SpawnBio)]
      noforecast <- tsplotyr <= endyr+1
      if(forecastplot==T){noforecast <- tsplotyr <= endyr+2}
      inforecast <- tsplotyr > endyr+1
      if(forecastplot) xlim=range(tsplotyr) else xlim=range(tsplotyr[noforecast])
      plot(0,xlab="Year", ylab="Spawning biomass (mt)",
           xlim=xlim, ylim=c(0,max(tsplotSB)), type="n")
      for(iarea in 1:nareas){
        tsplotyrarea <- tsplotyr[tsplotarea==iarea & noforecast]
        tsplotSBarea <- tsplotSB[tsplotarea==iarea & noforecast]
        lines(tsplotyrarea[-1],tsplotSBarea[-1],type="o",col=areacols[iarea])
        points(tsplotyrarea[1],tsplotSBarea[1],col=areacols[iarea],pch=19)
        if(forecastplot){
          tsplotyrarea <- tsplotyr[tsplotarea==iarea & inforecast]
          tsplotSBarea <- tsplotSB[tsplotarea==iarea & inforecast]
          lines(tsplotyrarea,tsplotSBarea,type="o",
                lty="dashed",pch=20,col=areacols[iarea])
          points(tsplotyrarea,tsplotSBarea,col=areacols[iarea],pch=19)
        }
      }
      if(nareas>1) legend("topright",legend=areanames,lty=1,pch=1,col=areacols,bty="n")
      abline(h=0,col="grey")
    }
    if(7 %in% plot) sbfunc(timeseries=timeseries,forecastplot=F)
    if(7 %in% print){
      png(file=paste(plotdir,"07_spawnbio.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      sbfunc(timeseries=timeseries,forecastplot=F)
      dev.off()}

    if(forecastplot){
      if(7 %in% plot) sbfunc(timeseries=timeseries,forecastplot=T)
      if(7 %in% print){
        png(file=paste(plotdir,"07_spawnbiowforecast.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        sbfunc(timeseries=timeseries,forecastplot=T)
        dev.off()}
    }

    if(uncertainty){
      xlab="Year"
      ylab="Spawning biomass (mt)"
      bioscale <- 1 #scaling factor for single sex models
      if(nsexes==1) bioscale <- 0.5 # should allow flexible input
      # with interval
      sbstd <- matchfun2("SPB_Virgin",0,"Recr_Virgin",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=derived_quants,objsubset=derived_quants,substr1=TRUE,substr2=TRUE)
      sbstd$Yr <- substring(sbstd$LABEL,5,nchar(sbstd$LABEL[1])-1)
      sbstd$Yr[2] <- as.numeric(sbstd$Yr[3])-1
      sbstd$Yr[1] <- as.numeric(sbstd$Yr[2])-1
      sbstd$Yr <- as.numeric(sbstd$Yr)
      v <- sbstd$Value*bioscale
      sbstd$upper <- v + 1.96*sbstd$StdDev*bioscale
      sbstd$lower <- v - 1.96*sbstd$StdDev*bioscale
      sbstd$lower[sbstd$lower < 0] <- 0
      ymax <- max(as.numeric(sbstd$upper))
      plottitle <- "~95% Asymptotic confidence interval"
     sbfunc3 <- function(){
        plotsbstdyr <- sbstd$Yr[sbstd$Yr<=(endyr+1)]
        plotsbstv <- v[sbstd$Yr<=(endyr+1)]
        plotsbstdup <- sbstd$upper[sbstd$Yr<=(endyr+1)]
        plotsbstdlo <- sbstd$lower[sbstd$Yr<=(endyr+1)]
        plot(plotsbstdyr[-1],plotsbstv[-1],xlim=range(plotsbstdyr),main= plottitle,xlab="Year",ylab=ylab,ylim=c(0,ymax),type="o",col="blue")
        abline(h=0,col="grey")
        lines(plotsbstdyr[2:length(plotsbstdyr)],plotsbstdup[2:length(plotsbstdup)],lwd=1,col="blue",lty="dashed")
        lines(plotsbstdyr[2:length(plotsbstdyr)],plotsbstdlo[2:length(plotsbstdlo)],lwd=1,col="blue",lty="dashed")
        points(plotsbstdyr[1],plotsbstv[1],col="blue",pch=19)
        points(plotsbstdyr[1],plotsbstdup[1],col="blue",pch="-")
        points(plotsbstdyr[1],plotsbstdlo[1],col="blue",pch="-")
        }
      if(7 %in% plot) sbfunc3()
      if(7 %in% print){
        png(file=paste(plotdir,"07_spawnbiointerval.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        sbfunc3()
        dev.off()}

      if(forecastplot){
        if(max(sbstd$Yr>=(endyr+2))){
          sbfunc4  <- function(){
           plotsbstdyr <- sbstd$Yr[sbstd$Yr<=(endyr+2)]
           plotsbstv <- v[sbstd$Yr<=(endyr+2)]
           plotsbstdup <- sbstd$upper[sbstd$Yr<=(endyr+2)]
           plotsbstdlo <- sbstd$lower[sbstd$Yr<=(endyr+2)]
           plotsbstdyr2 <- sbstd$Yr[sbstd$Yr>=(endyr+2)]
           plotsbstv2 <- v[sbstd$Yr>=(endyr+2)]
           plotsbstdup2 <- sbstd$upper[sbstd$Yr>=(endyr+2)]
           plotsbstdlo2 <- sbstd$lower[sbstd$Yr>=(endyr+2)]
           plot(plotsbstdyr[-1],plotsbstv[-1],xlim=range(plotsbstdyr,plotsbstdyr2),main= plottitle,xlab="Year",ylab=ylab,ylim=c(0,ymax),type="o",col="blue")
           abline(h=0,col="grey")
           lines(plotsbstdyr[2:length(plotsbstdyr)],plotsbstdup[2:length(plotsbstdup)],lwd=1,col="blue",lty="dashed")
           lines(plotsbstdyr[2:length(plotsbstdyr)],plotsbstdlo[2:length(plotsbstdlo)],lwd=1,col="blue",lty="dashed")
           points(plotsbstdyr[1],plotsbstv[1],col="blue",pch=19)
           points(plotsbstdyr[1],plotsbstdup[1],col="blue",pch="-")
           points(plotsbstdyr[1],plotsbstdlo[1],col="blue",pch="-")
           points(plotsbstdyr2,plotsbstv2,col="blue",pch=19)
           points(plotsbstdyr2,plotsbstdup2,col="blue",pch="-")
           points(plotsbstdyr2,plotsbstdlo2,col="blue",pch="-")
            }
          if(7 %in% plot) sbfunc4()
          if(7 %in% print){
            png(file=paste(plotdir,"07_spawnbioforecastinterval.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            sbfunc4()
            dev.off()}
        } # forecastplot
      } # sexes==1

    } # if uncertainty==T
    if(verbose) print("Finished plot 7: Spawning biomass",quote=F)
  } # end if 7 in plot or print

  # Plot 8: depletion
  if(8 %in% c(plot, print))
  {
    if(nseasons > 1) print("Skipped Plot 8 (depletion) because it is not yet configured for multi-season models",quote=F)
    if(nseasons==1){ ## Temporary filter until calculations can be cleaned up
     ylab <- "Spawning depletion"
     depfunc <- function(iarea){
      plottitle <- NULL
      if(nareas>1) plottitle <- paste("Spawning depletion in area",iarea)
      tsplotyr <- tsyears[tsarea==iarea]
      tsplotdep <- dep[tsarea==iarea]
      plot(tsplotyr[2:length(tsplotyr)],tsplotdep[2:length(tsplotdep)],xlab="Year",ylab=ylab,ylim=c(0,(max(dep))),type="o",col="blue",main=plottitle)
      points(tsplotyr[1],tsplotdep[1],col="blue",pch=19)
      abline(h=0,col="grey")
      abline(h=c(btarg,minbthresh),col="red")
      text(startyr+4,btarg+0.03,"Management target",adj=0)
      text(startyr+4,minbthresh+0.03,"Minimum stock size threshold",adj=0)
      }
     for(iarea in areas){
      if(8 %in% plot) depfunc(iarea)
      if(8 %in% print){
       png(file=paste(plotdir,"08_depletion.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
       depfunc(iarea)
       dev.off()}
      }
     if(uncertainty){
      depstd <- derived_quants[substring(derived_quants$LABEL,1,6)=="Bratio",]
      depstd$Yr <- as.numeric(substring(depstd$LABEL,8))
      depstd$period <- "fore"
      depstd$period[depstd$Yr<=(endyr+1)] <- "time"
      depstd$upper <- depstd$Value + 1.96*depstd$StdDev
      depstd$lower <- depstd$Value - 1.96*depstd$StdDev
      ymax <- max(dep,depstd$upper[depstd$period=="time"])
      depfunc2 <- function(){
        plot(depstd$Yr[depstd$period=="time"],depstd$Value[depstd$period=="time"],xlab="Year",ylab=ylab,ylim=c(0,ymax),type="o",col="blue")
        abline(h=0,col="grey")
        abline(h=c(btarg,minbthresh),col="red")
        text((min(depstd$Yr)+4),(btarg+0.03),"Management target",adj=0)
        text((min(depstd$Yr)+4),(minbthresh+0.03),"Minimum stock size threshold",adj=0)
        lines(depstd$Yr[depstd$period=="time"],depstd$upper[depstd$period=="time"],col="blue",lty="dashed")
        lines(depstd$Yr[depstd$period=="time"],depstd$lower[depstd$period=="time"],col="blue",lty="dashed")
       }
      if(8 %in% plot) depfunc2()
      if(8 %in% print){
        png(file=paste(plotdir,"08_depletioninterval.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        depfunc2()
        dev.off()}
      } # end if uncertainty

    if(forecastplot){
     fts <- timeseries[timeseries$Yr >= endyr+1,]
     ftsyears <- fts$Yr[fts$Seas==1]
     ftsarea <- fts$Area[fts$Seas==1]
     ftsspaw_bio <- fts$SpawnBio[fts$Seas==1]
     if(nsexes==1) ftsspaw_bio <- ftsspaw_bio/2
     fdep <- ftsspaw_bio/tsspaw_bio[1]
     ymax <- max(dep,fdep)
     xmin <- min(tsyears)-1
     xmax <- max(ftsyears)+1
     depfunc3 <- function(iarea){
      plottitle <- NULL
      tsplotyr <- tsyears[tsarea==iarea]
      tsplotdep <- dep[tsarea==iarea]
      if(nareas>1) plottitle <- paste("Spawning depletion in area",iarea)
      plot(tsplotyr[2:length(tsplotyr)],tsplotdep[2:length(tsplotdep)],xlab="Year",
              ylab=ylab,xlim=c(xmin,xmax),ylim=c(0,ymax),type="o",col="blue",main=plottitle)
      points(tsplotyr[1],tsplotdep[1],col="blue",pch=19)
      abline(h=0,col="grey")
      abline(h=c(btarg,minbthresh),col="red")
      text(startyr+4,btarg+0.03,"Management target",adj=0)
      text(startyr+4,minbthresh+0.03,"Minimum stock size threshold",adj=0)
      ftsplotyr <- ftsyears[ftsarea==iarea]
      ftsplotdep <- fdep[ftsarea==iarea]
      lines(ftsplotyr[1:2],ftsplotdep[1:2],lwd=1,col="blue",lty="dashed")
      lines(ftsplotyr[2:length(ftsplotyr)],ftsplotdep[2:length(ftsplotdep)],lwd=1,col="red",lty="dashed")
      points(ftsplotyr[2:length(ftsplotyr)],ftsplotdep[2:length(ftsplotdep)],col="red",pch=19)
      }
      for(iarea in areas){
       if(8 %in% plot) {
        depfunc3(iarea)}
       if(8 %in% print){
        png(file=paste(plotdir,"08_depletionforecast.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        depfunc3(iarea)
        dev.off()}
      } # end areas

     if(uncertainty){
      depstd <- derived_quants[substring(derived_quants$LABEL,1,6)=="Bratio",]
      depstd$Yr <- as.numeric(substring(depstd$LABEL,8))
      depstd$period <- "fore"
      depstd$period[depstd$Yr<=(endyr+1)] <- "time"
      depstd$upper <- depstd$Value + 1.96*depstd$StdDev
      depstd$lower <- depstd$Value - 1.96*depstd$StdDev
      ymax <- max(depstd$upper)
      xmin <- min(tsyears)-1
      xmax <- max(ftsyears)+1

      depfunc4 <- function(iarea){
       plottitle <- NULL
       tsplotyr <- tsyears[tsarea==iarea]
       tsplotdep <- dep[tsarea==iarea]
       if(nareas>1) plottitle <- paste("Spawning depletion in area",iarea)
       plot(tsplotyr[2:length(tsplotyr)],tsplotdep[2:length(tsplotdep)],xlab="Year",
            ylab=ylab,xlim=c(xmin,xmax),ylim=c(0,ymax),type="o",col="blue",main=plottitle)
       points(tsplotyr[1],tsplotdep[1],col="blue",pch=19)
       abline(h=0,col="grey")
       abline(h=c(btarg,minbthresh),col="red")
       text(startyr+4,btarg+0.03,"Management target",adj=0)
       text(startyr+4,minbthresh+0.03,"Minimum stock size threshold",adj=0)
       ftsplotyr <- ftsyears[ftsarea==iarea]
       ftsplotdep <- fdep[ftsarea==iarea]
       lines(ftsplotyr[1:2],ftsplotdep[1:2],lwd=1,col="blue",lty="dashed")
       lines(ftsplotyr[2:length(ftsplotyr)],ftsplotdep[2:length(ftsplotdep)],lwd=1,col="red",lty="dashed")
       points(ftsplotyr[2:length(ftsplotyr)],ftsplotdep[2:length(ftsplotdep)],col="red",pch=19)
       lines(depstd$Yr[depstd$period=="time"],depstd$upper[depstd$period=="time"],col="blue",lty="dashed")
       lines(depstd$Yr[depstd$period=="time"],depstd$lower[depstd$period=="time"],col="blue",lty="dashed")
       lines(depstd$Yr[depstd$Yr>=(endyr+1)],depstd$upper[depstd$Yr>=(endyr+1)],col="red",lty="dashed")
       lines(depstd$Yr[depstd$Yr>=(endyr+1)],depstd$lower[depstd$Yr>=(endyr+1)],col="red",lty="dashed")
       }
      if(8 %in% plot){
       depfunc4(iarea)}
      if(8 %in% print){
       png(file=paste(plotdir,"08_depletionforecastinterval.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
       depfunc4(iarea)
       dev.off()}
      } # end if uncertainty==T
    } # end if forecastplot==T

    if(verbose) print("Finished plot 8: Depletion",quote=F)
    } # end temporary exclusion of multi-season models
  } # end if 8 in plot or print

  # Plot 9: rec devs and asymptotic error check
  if(9 %in% c(plot, print))
  {
    recdev <- parameters[substring(parameters$Label,1,7)=="RecrDev",]
     if(!max(recdev$Value)>0){
     if(verbose) print("Skipped plot 9: Rec devs and asymptotic error check - no rec devs estimated",quote=F)}
     if(max(recdev$Value)>0){
      if(nrow(recdev)>0){
        recdev$Yr <- as.numeric(substring(recdev$Label,9))
        ylab <- "Log Recruitment deviation"
        recdevfunc <- function(){
          plot(recdev$Yr,recdev$Value,xlab="Year",main="",ylab=ylab,type="b")
          abline(h=0,col="black")}
        if(9 %in% plot) recdevfunc()
        if(9 %in% print){
          png(file=paste(plotdir,"09_recdevs.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
          recdevfunc()
          dev.off()}
    if(uncertainty){
      sigr <- as.numeric(parameters$Value[parameters$Label=="SR_sigmaR"])
        ymax <- max(recdev$Parm_StDev,sigr)
        main <- "Recruitment deviation variance check"
        ylab <- "Asymptotic standard error estimate"
        recdevfunc2 <- function(){
          plot(recdev$Yr,recdev$Parm_StDev,xlab="Year",main=main,ylab=ylab,ylim=c(0,ymax),type="b")
          abline(h=0,col="grey")
          abline(h=sigr,col="red")}
        if(9 %in% plot) recdevfunc2()
        if(9 %in% print){
          png(file=paste(plotdir,"09_recdevvarcheck.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
          recdevfunc2()
          dev.off()}
      } # rec devs
    } # end if uncertainty==T
    if(verbose) print("Finished plot 9: Rec devs and asymptotic error check",quote=F)
    flush.console()
   }
  } # end if 9 in plot or print

  ### Plot 10: average body weight observations ###
  if(10 %in% c(plot, print))
  {
    if(!is.na(mnwgt)[1])
    {
      for(i in unique(mnwgt$Fleet))
      {
        usemnwgt <- mnwgt[mnwgt$Fleet==i & mnwgt$Obs>0,]
        usemnwgt$Mkt <- usemnwgt$Mkt
        for(j in unique(mnwgt$Mkt))
        {
          yr <- usemnwgt$Yr[usemnwgt$Mkt==j]
          ob <- usemnwgt$Obs[usemnwgt$Mkt==j]
          cv <- usemnwgt$CV[usemnwgt$Mkt==j]
          ex <- usemnwgt$Exp[usemnwgt$Mkt==j]
          xmin <- min(yr)-3
          xmax <- max(yr)+3
          uiw <- ob*1.96*cv
          liw <- ob*1.96*cv
          liw[(ob-liw)<0] <- ob[(ob-liw)<0]
          ymax <- max(ob + uiw)
          ymax <- max(ymax,ex)
          ptitle <- paste("Mean weight in discard for fleet",i,sep=" ")
          if(j==2) ptitle <- paste("Mean weight in retained catch for fleet",i,sep=" ")
          if(j==0) ptitle <- paste("Mean weight in whole catch for fleet",i,sep=" ")
          ylab <- "Mean individual body weight (kg)"
          bdywtfunc <- function(){
            plotCI(x=yr,y=ob,uiw=uiw,liw=liw,xlab="Year",main=ptitle,ylo=0,col="red",sfrac=0.001,z=ymax,ylab=ylab,lty=1,xlim=c(xmin,xmax))
            abline(h=0,col="grey")
            points(yr,ex,col="blue",cex=2,pch="-")}
          if(10 %in% plot) bdywtfunc()
          if(10 %in% print){
            png(file=paste(plotdir,"10_bodywtfit",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            bdywtfunc()
            dev.off()}
        } # market
      } # fleets
    } # if mean weight data exists
    if(verbose) print("Finished plot 10: Average body weight observations",quote=F)
    flush.console()
  } # end if 10 in plot or print

  ### Plot 11: SPR and fishing intensity plots ###
  if(11 %in% c(plot, print))
  {
    sprfunc <- function(){
      plot(sprseries$Year,sprseries$spr,xlab="Year",ylab="SPR",ylim=c(0,max(1,max(sprseries$spr[!is.na(sprseries$spr)]))),type="o",col="blue")
      abline(h=sprtarg,col="red",lty=2)
      abline(h=0,col="grey")
      abline(h=1,col="grey")}
    if(11 %in% plot) sprfunc()
    if(11 %in% print){
      png(file=paste(plotdir,"11_sprseries",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      sprfunc()
      dev.off()}

if(nseasons>1) print("Skipped 1-SPR series plot because it's not yet configured for multi-season models",quote=F)
if(nseasons==1){ # temporary disable until code cleanup
    sprfunc <- function(){
      plot(sprseries$Year,(1-sprseries$spr),xlab="Year",ylab="1-SPR",ylim=c(0,1),type="o",col="blue")
      abline(h=(1-sprtarg),col="red",lty=2)
      abline(h=0,col="grey")
      abline(h=1,col="grey")}
    if(11 %in% plot) sprfunc()
    if(11 %in% print){
      png(file=paste(plotdir,"11_1minussprseries",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      sprfunc()
      dev.off()}

    if(uncertainty){
      sprratiostd <- derived_quants[substring(derived_quants$LABEL,1,8)=="SPRratio",]
      sprratiostd$Yr <- as.numeric(substring(sprratiostd$LABEL,10))
      sprratiostd$period <- "fore"
      sprratiostd$period[sprratiostd$Yr<=(endyr)] <- "time"
      sprratiostd$upper <- sprratiostd$Value + 1.96*sprratiostd$StdDev
      sprratiostd$lower <- sprratiostd$Value - 1.96*sprratiostd$StdDev
      ylab <- managementratiolabels[1,2]
      ylim=c(0,max(1,sprratiostd$upper[sprratiostd$period=="time"]))
      sprfunc2 <- function(){
        plot(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$Value[sprratiostd$period=="time"],xlab="Year",ylim=ylim,ylab=ylab,type="o",col="blue")
        abline(h=0,col="grey")
        abline(h=1,col="red")
        text((min(sprratiostd$Yr)+4),(1+0.02),"Management target",adj=0)
        lines(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$upper[sprratiostd$period=="time"],col="blue",lty="dashed")
        lines(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$lower[sprratiostd$period=="time"],col="blue",lty="dashed")
       }
      if(11 %in% plot) sprfunc2()
      if(11 %in% print){
        png(file=paste(plotdir,"11_sprratiointerval.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        sprfunc2()
        dev.off()}
       }

    timeseries$Yr <- timeseries$Yr + (timeseries$Seas-1)/nseasons
    ts <- timeseries[timeseries$Area==1 & timeseries$Yr <= endyr+1,] #!subsetting to area 1 only. This should be generalized
    tsyears <- ts$Yr[ts$Seas==1]
    tsspaw_bio <- ts$SpawnBio[ts$Seas==1]
    if(nsexes==1) tsspaw_bio <- tsspaw_bio/2
    depletionseries <- tsspaw_bio/tsspaw_bio[1]
    reldep <- depletionseries[tsyears %in% sprseries$Year]/btarg
    relspr <- (1-sprseries$spr)/(1-sprtarg)
    xmax <- 1.1*max(reldep)
    ymax <- 1.1*max(1,relspr[!is.na(relspr)])
    phasefunc <- function(){
      plot(reldep,relspr,xlab="B/Btarget",xlim=c(0,xmax),ylim=c(0,ymax),ylab="(1-SPR)/(1-SPRTarget)",type="o",col="blue")
      abline(h=0,col="grey")
      abline(v=0,col="grey")
      lines(reldep,relspr,type="o",col="blue")
      points(reldep[length(reldep)],relspr[length(relspr)],col="red",pch=19)
      abline(h=1,col="red",lty=2)
      abline(v=1,col="red",lty=2)}
    if(11 %in% plot) phasefunc()
    if(11 %in% print){
      png(file=paste(plotdir,"11_sprphase",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      phasefunc()
      dev.off()}
    if(verbose) print("Finished plot 11: SPR",quote=F)
    flush.console()
} # end temporary multi-season disable of section
  } # end if 11 in plot or print

  ### Plot 12: spawner-recruit curve ###
  if(12 %in% c(plot, print))
  {
    recruit <- recruit[recruit$era=="Main",]
    ymax <- max(recruit$pred_recr)
    x <- recruit$spawn_bio
    xmax <- max(x)
    xlab <- "Spawning biomass (mt)"
    ylab <- "Recruitment (1,000s)"
    recruitfun <- function(){
      plot(x[order(x)],recruit$with_env[order(x)],xlab=xlab,ylab=ylab,type="l",col="blue",ylim=c(0,ymax),xlim=c(0,xmax))
      abline(h=0,col="grey")
      biasad <- recruit$bias_adj
      lines(x[order(x)],biasad[order(x)],col="green")
      lines(x[order(x)],recruit$exp_recr[order(x)],lwd=2,col="black")
      points(x,recruit$pred_recr,col="red")}
    if(12 %in% plot) recruitfun()
    if(12 %in% print){
      png(file=paste(plotdir,"12_srcurve.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      recruitfun()
      dev.off()}
    if(verbose) print("Finished plot 12: Spawner-recruit curve",quote=F)
    flush.console()
  } # end if 12 in plot or print


  ### Plot 13: CPUE plots ###
  if(13 %in% c(plot, print))
  {
    for(i in unique(cpue$Fleet)){
      cpueuse <- cpue[cpue$Obs > 0 & cpue$Fleet==i,]
      x <- cpueuse$Yr
      y <- cpueuse$Obs
      z <- cpueuse$Exp
      uiw <- qlnorm(.975,meanlog=log(y),sdlog=cpueuse$SE) - y
      liw <- y - qlnorm(.025,meanlog=log(y),sdlog=cpueuse$SE)
      main=paste("Index ", i,sep="")
      xlab <- "Observed index"
      if(13 %in% plot){
        plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab="Year",ylo=0,col="red",ylab="Index",main=main,lty=1)
        abline(h=0,col="grey")
        lines(x,z,lwd=2,col="blue")
        plot(y,z,xlab=xlab,main=main,ylim=c(0,max(z)),xlim=c(0,max(y)),col="blue",pch=19,ylab="Expected index")
        abline(h=0,col="grey")
        lines(x=c(0,max(z)),y=c(0,max(z)),col="black")
        npoints <- length(z)
        if(npoints > 6 & smooth){
          psmooth <- loess(z~y,degree=1)
          lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}}
      if(13 %in% print){
        png(file=paste(plotdir,"13_cpuefit",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab="Year",ylo=0,col="red",ylab="Index",main=main,lty=1)
        abline(h=0,col="grey")
        lines(x,z,lwd=2,col="blue")
        dev.off()
        png(file=paste(plotdir,"13cpuecheck",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        plot(y,z,xlab=xlab,main=main,ylim=c(0,max(z)),xlim=c(0,max(y)),col="blue",pch=19,ylab="Expected index")
        abline(h=0,col="grey")
        lines(x=c(0,max(z)),y=c(0,max(z)),col="black")
        npoints <- length(z)
        if(npoints > 6 & smooth){
          psmooth <- loess(z~y,degree=1)
          lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
        dev.off()}
      ylab <- "Log index"
      main <- paste("Log index ", i,sep="")
      xlab <- "Log observed index"
      ylab2 <- "Log expected index"
      uiw <- qnorm(.975,mean=log(y),sd=cpueuse$SE) - log(y)
      liw <- log(y) - qnorm(.025,mean=log(y),sd=cpueuse$SE)
      if(13 %in% plot){
        plotCI(x=x,y=log(y),z=log(z),sfrac=0.001,uiw=uiw,liw=liw,xlab="Year",col="red",ylab=ylab,main=main,lty=1)
        lines(x,log(z),lwd=2,col="blue")
        plot(log(y),log(z),xlab=xlab,main=main,ylim=c(log(min(z)),log(max(z))),xlim=c(log(min(y)),log(max(y))),col="blue",pch=19,ylab=ylab2)
        lines(x=c(log(min(z)),log(max(z))),y=c(log(min(z)),log(max(z))),col="black")
        if(npoints & smooth){
          psmooth <- loess(log(z)~log(y),degree=1)
          lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
      }
      if(13 %in% print){
        png(file=paste(plotdir,"13_logcpuefit",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        plotCI(x=x,y=log(y),z=log(z),sfrac=0.001,uiw=uiw,liw=liw,xlab="Year",col="red",ylab=ylab,main=main,lty=1)
        lines(x,log(z),lwd=2,col="blue")
        dev.off()
        png(file=paste(plotdir,"13_logcpuecheck",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        plot(log(y),log(z),xlab=xlab,main=main,ylim=c(log(min(z)),log(max(z))),xlim=c(log(min(y)),log(max(y))),col="blue",pch=19,ylab=ylab2)
        lines(x=c(log(min(z)),log(max(z))),y=c(log(min(z)),log(max(z))),col="black")
        if(npoints > 3 & smooth){
          psmooth <- loess(log(z)~log(y),degree=1)
          lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
        dev.off()}

    } # nfleets
    if(verbose) print("Finished plot 13: CPUE plots",quote=F)
    flush.console()
  } # end if 13 in plot or print

  ### Plot 14: numbers at age ###
  if(14 %in% c(plot, print))
  {
    for(iarea in areas)
    {
      for(m in 1:nsexes)
      {
        # warning implementation of birthseasons may not be correct in this section
        if(max(morph_indexing$Index) > nareas*nsexes*nseasons)
        {
          print("Expected numbers at age plot not yet configured for multiple morphs.",quote=F)
        }else{
          natagetemp0 <- natage[natage$Area==iarea & natage$Gender==m & natage$Seas==1 &
                            natage$Era!="VIRG" & natage$Yr <= (endyr+1),]
          nyrsplot <- nrow(natagetemp0)
          resx <- rep(natagetemp0$Yr, accuage+1)
          resy <- NULL
          for(i in 0:accuage) resy <- c(resy,rep(i,nyrsplot))
          resz <- NULL
          for(i in 11+0:accuage) resz <- c(resz,natagetemp0[,i])

          if(m==1 & nsexes==1) sextitle <- ""
          if(m==1 & nsexes==2) sextitle <- " of females"
          if(m==2) sextitle=" of males"
          if(nareas>1) sextitle <- paste("in area",iarea,sextitle)
          plottitle <- paste("Expected numbers",sextitle," at age in thousands (max=",max(resz),")",sep="")

          tempfun <- function(){
            bubble3(x=resx, y=resy, z=resz,
                    xlab="Year",ylab="Age (yr)",col=c("black","black"),main=plottitle,maxsize=(pntscalar+1.0),
                    las=1,cex.main=cex.main,allopen=1)
          }

          natagetemp1 <- as.matrix(natagetemp0[,-(1:10)])
          ages <- 0:accuage
          datsum <- as.vector(apply(natagetemp1,1,sum))
          natagetemp2 <- as.data.frame(natagetemp1)
          natagetemp2$sum <- datsum
          prodmat <- t(natagetemp1)*ages
          prodsum <- as.vector(apply(prodmat,2,sum))
          natagetemp2$sumprod <- prodsum
          natagetemp2$meanage <- natagetemp2$sumprod/natagetemp2$sum - (natagetemp0$BirthSeas-1)/nseasons
          meanageyr <- sort(unique(natagetemp0$Yr))
          meanage <- 0*meanageyr
          for(i in 1:length(meanageyr)){ # averaging over values within a year (depending on birth season)
            meanage[i] <- sum(natagetemp2$meanage[natagetemp0$Yr==meanageyr[i]]*natagetemp2$sum[natagetemp0$Yr==meanageyr[i]])/sum(natagetemp2$sum[natagetemp0$Yr==meanageyr[i]])}
          ylim <- c(0,max(meanage))
          ylab <- plottitle <- paste("Mean age",sextitle," in the population (yr)",sep="")
          if(14 %in% plot){
            tempfun()
            plot(meanageyr,meanage,xlab="Year",ylim=ylim,type="o",ylab=ylab,col="black",main=plottitle)}
          if(14 %in% print){
            filepart <- paste("_sex",m,sep="")
            if(nareas > 1) filepart <- paste("_area",iarea,filepart,sep="")
            png(file=paste(plotdir,"14_natage",filepart,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            tempfun()
            dev.off()
            png(file=paste(plotdir,"14_meanage",filepart,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            plot(meanageyr,meanage,xlab="Year",ylim=ylim,type="o",ylab=ylab,col="black",main=plottitle)
            dev.off()}
        }
      } # end gender loop
    } # end area loop

    # plot the ageing imprecision for all age methods
    if(!is.null(AAK)){
      sd_vectors <- as.data.frame(AAK[,1,])
      n_age_error_keys <- 1
      if(!is.null(nrow(AAK[,1,]))){n_age_error_keys <- nrow(AAK[,1,])}
      if(is.null(nrow(AAK[,1,]))){xvals <- seq(0.5,length(sd_vectors[,1])-0.5,by=1)}
      if(!is.null(nrow(AAK[,1,]))){xvals <- seq(0.5,length(sd_vectors[1,]-0.5),by=1)}
      ylim <- c(0,max(sd_vectors))
      if(n_age_error_keys==1){ploty <- sd_vectors[,1]}
      if(n_age_error_keys>1){ploty <- sd_vectors[1,]}
      if(14 %in% plot){
        plot(xvals,ploty,ylim=ylim,type="o",col="black",xlab="True age (yr)",ylab="SD of observed age (yr)")
        if(n_age_error_keys > 1){
          for(i in 2:n_age_error_keys){
            lines(xvals,sd_vectors[i,],type="o",col=rich.colors.short(n_age_error_keys)[i])
          } # close for n keys loop
        } # close if more than one key statement
      } # end if 14 in plot
      if(14 %in% print){
        png(file=paste(plotdir,"14_ageerrorkeys.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        plot(xvals,ploty,ylim=ylim,type="o",col="black",xlab="True age (yr)",ylab="SD of observed age (yr)")
        if(n_age_error_keys > 1){
          for(i in 2:n_age_error_keys){
            lines(xvals,sd_vectors[i,],type="o",col=rich.colors.short(n_age_error_keys)[i])
          } # close for n keys loop
        } # close if more than one key statement
        dev.off()
      } # close if 14 in print
    } # end if AAK
    if(verbose) print("Finished plot 14: Numbers at age",quote=F)
    flush.console()
  } # close if 14 in plot or print

###############################################################
# two embedded functions for composition plots (plots 15 through 20)
#
#   make_multifig       generalized function for multi-figure plots
#   SSv3_plot_comps     dedicated for SS composition plots
#
###############################################################
  
  make_multifig <- function(ptsx, ptsy, yr, linesx=0, linesy=0,
       sampsize=0, effN=0, minsampsize=0, sampsizeround=1,
       maxrows=6, maxcols=6, fixdims=T, main="",cex.main=1,xlab="",ylab="",
       size=1,maxsize=3,do.sqrt=TRUE,minnbubble=8,allopen=TRUE,
       horiz_lab="default",xbuffer=c(.1,.1),ybuffer=c(0,0.15),ymin0=T,
       axis1="default",axis2="default",linepos=1,
       bars=F,barwidth="default",ptscol=1,ptscol2=1,linescol=2,lty=1,lwd=1,pch=1,
       nlegends=3,legtext=list("yr","sampsize","effN"),legx="default",legy="default",
       legadjx="default",legadjy="default",legsize=c(1.2,1.0),legfont=c(2,1),
       ipage=0)
  {
    ################################################################################
    #
    # make_multifig March 23, 2009
    #
    # Purpose: To plot a multifigure environment similar to lattice but simpler
    #          and with easier controls over some things
    # Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
    # Returns: a plot
    # General:
    # Notes:
    # Required packages: none
    #
    ################################################################################
  
    # notes on inputs for make_multifig
    # ptsx                    = vector of x values for points or bars
    # ptsy                    = vector of y values for points or bars  of same length as ptsx
    # yr                       = vector of category values (years) of same length as ptsx
    # linesx=0                = optional vector of x values for lines
    # linesy=0                = optional vector of y values for lines
    # sampsize=0              = optional sample size vector of same length as ptsx
    # minsampsize=0           = optional lower limit on sample sizes to be printed on the plot
    # sampsizeround=1         = number of decimal places to include in the sample size text
    # maxrows=6               = the maximum number of rows of plots
    # maxcols=6               = the maximum number of columns of plots
    # fixdims=T               = should the number of rows & columns be fixed to the maximum (T/F)
    # main=""                 = the main title for the plot
    # cex.main=1               = the font size multiplier for the main title
    # xlab=""                 = the x-axis label
    # ylab=""                 = the y-axis label
    # horiz_lab="default"     = axis labels set horizontal all the time (T), never (F) or
    #                           only when relatively short ("default")
    # xbuffer=c(.1,.1)        = extra space around points on the left and right as fraction of total width of plot
    # axis1="default"         = vector of values for x-axis, "default" chooses in typical R fashion
    # axis2="default"         = vector of values for y-axis, "default" chooses in typical R fashion
    # linepos=1               = position of lines relative to point/bars (0=no lines, 1=behind, 2=in front)
    # bars=F                  = should the ptsx/ptsy values be bars instead of points (T/F)
    # barwidth="default"      = width of bars in barplot, default method chooses based on quick and dirty formula
    #                           also, current method of plot(...type='h') could be replaced with better approach
    # ptscol=1                = color for points/bars
    # linescol=2              = color for lines
    # lty=1                   = line type
    # lwd=1                   = line width
    # pch=1                   = point character
    # nlegends=2              = number of legends
    # legtext=list("yr","sampsize") = text in legend, a list of length=nlegends values may be
    #                           1. "yr" to make the legend for each plot equal to the yr input for the values within
    #                           2. "sampsize" to make the legend be "n=99.9" where 99.9 is the sample size for the values
    #                              rounded according to the input sampsizeround
    #                           3. "effN" to make the legend be "effN=88.8" where 88.8 is the effective sample size
    #                           4. a vector of length = ptsx
    # legx="default"          = vector of length=nlegends of x-values of legends (default is first one on left,
    #                           all after on right)
    # legy="default"          = vector of length=nlegends of y-values of legends (default is top for all plots)
    # legadjx="default"       = left/right adjustment of legends around legx
    # legadjy="default"       = left/right adjustment of legends around legy
    # legsize=c(1.2,1.0)      = font size for legends
    # legfont=c(2,1)          = font type for legends, same as "font" under ?par
  
    bubble3 <- function (x,y,z,col=c(1,1),maxsize=3,do.sqrt=TRUE,
                         main="",cex.main=1,xlab="",ylab="",minnbubble=8,
                         xlimextra=1,add=F,las=1,allopen=TRUE)
    {
        # vaguely based on bubble() from gstat
        az <- abs(z)
        if (do.sqrt) az <- sqrt(az)
        cex <- maxsize * az/max(az)
        z.col <- ifelse(z < 0, col[1], col[2])
        xlim <- range(x)
        if(length(unique(x))<minnbubble) xlim=xlim+c(-1,1)*xlimextra
        pch <- z
        pch[pch==0] <- NA
        pch[pch>0] <- 16
        pch[pch<0] <- 1
        if(allopen) pch[!is.na(pch)] <- 1
        if(!add){
            plot(x,y,type='n',xlim=xlim,main=main,xlab=xlab,ylab=ylab,axes=F,cex.main=cex.main)
            axis(1,at=unique(x))
            axis(2,las=las)
            box()
        }
        points(x,y,pch=pch,cex=cex,col=z.col)
    }
  
    # define dimensions
    yrvec <- sort(unique(yr))
    npanels <- length(yrvec)
    nvals <- length(yr)
  
    nrows <- min(ceiling(sqrt(npanels)), maxrows)
    ncols <- min(ceiling(npanels/nrows), maxcols)
    if(fixdims){
        nrows <- maxrows
        ncols <- maxcols
    }
  
    npages <- ceiling(npanels/nrows/ncols) # how many pages of plots
  
    # if no input on lines, then turn linepos to 0
    if(length(linesx)==1 | length(linesy)==1){
      linepos <- 0
      linesx <- ptsx
      linesy <- ptsy
    }
  
    # quick and dirty formula to get width of bars (if used) based on
    #   number of columns and maximum number of bars within a in panel
    if(bars & barwidth=="default") barwidth <- 400/max(table(yr)+2)/ncols
  
    # make size vector have full length
    if(length(size)==1) size <- rep(size,length(yr))
  
    # get axis limits
    xrange <- range(c(ptsx,linesx,ptsx,linesx))
    if(ymin0) yrange <- c(0,max(ptsy,linesy)) else yrange <- range(c(ptsy,linesy,ptsy,linesy))
    xrange_big <- xrange+c(-1,1)*xbuffer*diff(xrange)
    yrange_big <- yrange+c(-1,1)*ybuffer*diff(yrange)
  
    # get axis labels
    yaxs_lab <- pretty(yrange)
    maxchar <- max(nchar(yaxs_lab))
    if(horiz_lab=="default") horiz_lab <- maxchar<6 # should y-axis label be horizontal?
  
    if(axis1=="default") axis1=pretty(xrange)
    if(axis2=="default") axis2=pretty(yrange)
  
    if(length(sampsize)==1) sampsize <- 0
    if(length(effN)==1) effN <- 0
  
    # create multifigure layout and set inner margins all to 0 and add outer margins
    par(mfcol=c(nrows,ncols),mar=rep(0,4),oma=c(5,5,4,2)+.1)
  
    panelrange <- 1:npanels
    if(npages > 1 & ipage!=0) panelrange <- intersect(panelrange, 1:(nrows*ncols) + nrows*ncols*(ipage-1))
  
    for(ipanel in panelrange)
    {
      # subset values
      yr_i <- yrvec[ipanel]
      ptsx_i <- ptsx[yr==yr_i]
      ptsy_i <- ptsy[yr==yr_i]
  
      linesx_i <- linesx[yr==yr_i]
      linesy_i <- linesy[yr==yr_i]
  
      # sort values in lines
      linesy_i <- linesy_i[order(linesx_i)]
      linesx_i <- sort(linesx_i)
  
      z_i <- size[yr==yr_i]
  
      # make plot
      plot(0,type='l',axes=F,xlab="",ylab="",xlim=xrange_big,ylim=yrange_big,
        xaxs="i",yaxs=ifelse(bars,"i","r"))
      abline(h=0,col="grey") # grey line at 0
      if(linepos==1) lines(linesx_i,linesy_i,col=linescol,lwd=lwd,lty=lty) # lines first
      if(diff(range(size))!=0){ # if size input is provided then use bubble function
        bubble3(x=ptsx_i,y=ptsy_i,z=z_i,col=c(ptscol,ptscol2),
                maxsize=maxsize,minnbubble=minnbubble,allopen=allopen,add=T) # bubble plot
      }else{
        if(!bars) points(ptsx_i,ptsy_i,pch=pch,col=ptscol)  # points
        if( bars) points(ptsx_i,ptsy_i,type='h',lwd=barwidth,col=ptscol,lend=1)  # histogram-style bars
      }
      if(linepos==2) lines(linesx_i,linesy_i,col=linescol,lwd=lwd,lty=lty)
  
      # add legends
      usr <- par("usr")
      for(i in 1:nlegends)
      {
        text_i <- ""
        legtext_i <- legtext[[i]] # grab element of list
        # elements of list can be "default" to make equal to yr
        # or vector of length 1, npanels, or the full length of the input vectors
        if(length(legtext_i)==1){      text_i <- legtext_i           # one value repeated
          if(legtext_i=="yr")          text_i <- yr_i                # values in "yr" input
          if(legtext_i=="sampsize"){                                 # sample sizes
            if(max(sampsize) > minsampsize){
              text_i <- unique(sampsize[yr==yr_i])
              if(length(text_i)>1){
                  print(paste("Warning: sampsize values are not all equal--choosing the first value:",text_i[1]),quote=F)
                  print(paste("         yr=",yr_i,", and all sampsize values:",paste(text_i,collapse=","),sep=""),quote=F)
                  text_i <- text_i[1]
              }
              text_i <- paste("N=",round(sampsize[yr==yr_i],sampsizeround),sep="")
            }else{
              text_i <- ""
            }
          }
          if(legtext_i=="effN"){                                     # effective sample sizes
            if(max(effN) > minsampsize){
              text_i <- unique(effN[yr==yr_i])
              if(length(text_i)>1){
                  print(paste("Warning: effN values are not all equal--choosing the first value:",text_i[1]),quote=F)
                  print(paste("         all effN values:",paste(text_i,collapse=",")),quote=F)
                  text_i <- text_i[1]
              }
              text_i <- paste("effN=",round(effN[yr==yr_i],sampsizeround),sep="")
            }else{
              text_i <- ""
            }
          }
        }
        if(length(legtext_i)==npanels) text_i <- legtext_i[ipanel]      # one input value per panel
        if(length(legtext_i)==nvals)   text_i <- legtext_i[yr==yr_i][1] # one input value per element
        if(length(legtext_i)==1)       text_i <- text_i[1]              # yr, sampsize, or effN
        if(legx[1]=="default"){
          # default is left side for first plot, right thereafter
          textx <- ifelse(i==1, usr[1], usr[2])
        }else{ textx <- legx[i] }
        if(legy[1]=="default"){
          texty <- usr[4]         # default is top for all plots
        }else{ texty <- legy[i] }
        if(legadjx[1]=="default"){
          adjx <- ifelse(i==1, -.1, 1.0) # default is left side for first legend, right thereafter
        }else{ adjx <- legadjx[i] }
        if(legadjy[1]=="default"){
          adjy <- ifelse(i<3, 1.3, 1.3 + 1.3*(i-2))  # default is top for first 2 legends, below thereafter
        }else{ adjy <- legadjy[i] }
  
        # add legend text
        text(x=textx,y=texty,labels=text_i,adj=c(adjx,adjy),cex=legsize[i],font=legfont[i])
      }
  
      # add axes in left and lower outer margins
      mfg <- par("mfg")
      if(mfg[1]==mfg[3] | ipanel==npanels) axis(side=1,at=axis1) # axis on bottom panels and final panel
      if(mfg[2]==1) axis(side=2,at=axis2,las=horiz_lab)        # axis on left side panels
      box()
  
      if(ipanel %% (nrows*ncols) == 1) # if this is the first panel of a given page
      {
        # add title after plotting first panel on each page of panels
        fixcex = 1 # fixcex compensates for automatic adjustment caused by par(mfcol)
        if(max(nrows,ncols)==2) fixcex = 1/0.83
        if(max(nrows,ncols)>2) fixcex = 1/0.66
  
        title(main=main, line=c(2,0,3,3), outer=T, cex.main=cex.main*fixcex)
        title(xlab=xlab, outer=T, cex.lab=fixcex)
        title(ylab=ylab, line=ifelse(horiz_lab,max(3,2+.4*maxchar),3.5), outer=T, cex.lab=fixcex)
      }
    }
    # restore default single panel settings
    par(mfcol=c(1,1),mar=c(5,5,4,2)+.1,oma=rep(0,4))
  
    # return information on what was plotted
    return(list(npages=npages, npanels=npanels, ipage=ipage))
  }

  SSv3_plot_comps <- function(
      replist="ReportObject", kind="LEN", aalyear=-1, aalbin=-1, GUI=T, png=F, plotdir=NA, fleets="all",
      datonly=F, Natageplot=T, samplesizeplots=T, compresidplots=T, bub=F, showsampsize=T, showeffN=T,
      minnbubble=8, pntscalar=2.6, pwidth=7, pheight=7, punits="in", ptsize=12, res=300, cex.main=1,
      linepos=1, fitbar=F,maxsize=3,do.sqrt=TRUE,smooth=TRUE,
      agelab="Age (years)", lenlab="Length (cm)",proplab="Proportion",yearlab="Year", lenunits="cm",
      osslab="Observed sample size",esslab="Effective sample size",printmkt=T,printsex=T,
      maxrows=6,maxcols=6,maxrows2=2,maxcols2=4,fixdims=T,maxneff=5000,returntitles=T,verbose=T,...)
  {
  ################################################################################
  #
  # SSv3_plot_comps BETA March 23, 2009
  # This function comes with no warranty or guarantee of accuracy
  #
  # Purpose: test subset of SSv3_plots to show compositional data with or without fits
  # Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
  #          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
  # Returns: Plots with plot history in R GUI and/or .png files.
  # General: Updated for Stock Synthesis version 3.02f March, 2009; R version 2.8.1
  # Notes:   See users guide for documentation: http://code.google.com/p/r4ss/wiki/Documentation
  # Required SS3v_output function
  #
  ################################################################################
    if(!exists("make_multifig"))
      print("you are missing the function 'make_mulitifig', which should have been embedded within this file SSv3_plots.R")

    titles <- NULL
    if(png) if(is.na(plotdir)) return("plotdir must be specified to write png files.")

    nfleets    <- replist$nfleets
    FleetNames <- replist$FleetNames
    nseasons   <- replist$nseasons
    compdbase  <- replist$composition_database
    if(nseasons>1) compdbase$YrSeasName <- paste(floor(compdbase$Yr),"s",compdbase$Seas,sep="") else compdbase$YrSeasName <- compdbase$Yr

    lendbase   <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
    agedbase   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0 & compdbase$Lbin_lo != compdbase$Lbin_hi,]
    condbase   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0 & compdbase$Lbin_lo == compdbase$Lbin_hi,]

    # not sure why these are not converted to numeric in SSv3_output, nor why they aren't done in 1 step a few lines above
    lendbase$effN <- as.numeric(lendbase$effN)
    agedbase$effN <- as.numeric(agedbase$effN)
    condbase$effN <- as.numeric(condbase$effN)

    if(fleets[1]=="all"){
      fleets <- 1:nfleets
    }else{
      if(length(intersect(fleets,1:nfleets))!=length(fleets)){
        return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
      }
    }

    # a few quantities related to data type and plot number
    if(kind=="LEN"){
      dbase_kind <- lendbase
      kindlab=lenlab
      if(datonly){
        filenamestart <- "15_lendat_"
        titledata <- "length comp data, "
      }else{
        filenamestart <- "18_lenfit_"
        titledata <- "length comps, "
      }
    }
    if(kind=="AGE"){
      dbase_kind <- agedbase
      kindlab=agelab
      if(datonly){
        filenamestart <- "16_agedat_"
        titledata <- "age comp data, "
      }else{
        filenamestart <- "19_agefit_"
        titledata <- "age comps, "
      }
    }
    if(kind=="cond"){
      dbase_kind <- condbase
      kindlab=agelab
      if(datonly){
        filenamestart <- "17_condAALdat_"
        titledata <- "conditional length at age data, "
      }else{
        filenamestart <- "20_condAALfit_"
        titledata <- "conditional length at age, "
      }
    }

    if(!(kind%in%c("LEN","AGE","cond"))) return("Input 'kind' to SSv3_plot_comps should be 'LEN' or 'AGE'.")
    # loop over fleets
    for(f in fleets)
    {
      # check for the presence of data
      if(length(dbase_kind$Obs[dbase_kind$Fleet==f])>0)
      {
        dbasef <- dbase_kind[dbase_kind$Fleet==f,]
        testor    <- length(dbasef$Gender[dbasef$Gender==1 & dbasef$Pick_gender==0 ])>0
        testor[2] <- length(dbasef$Gender[dbasef$Gender==1 & dbasef$Pick_gender %in% c(1,3)])>0
        testor[3] <- length(dbasef$Gender[dbasef$Gender==2])>0

        # loop over genders combinations
        for(k in (1:3)[testor])
        {
          if(k==1){dbase_k <- dbasef[dbasef$Gender==1 & dbasef$Pick_gender==0,]}
          if(k==2){dbase_k <- dbasef[dbasef$Gender==1 & dbasef$Pick_gender %in% c(1,3),]}
          if(k==3){dbase_k <- dbasef[dbasef$Gender==2,]}
          sex <- ifelse(k==3, 2, 1)

          # loop over partitions (discard, retain, total)
          for(j in unique(dbase_k$Part))
          {
            # dbase is the final data.frame used in the individual plots
            # it is subset based on the kind (age, len, age-at-len), fleet, gender, and partition
            dbase <- dbase_k[dbase_k$Part==j,]

            ## assemble pieces of plot title
            # sex
            if(k==1) titlesex <- "sexes combined, "
            if(k==2) titlesex <- "female, "
            if(k==3) titlesex <- "male, "
            titlesex <- ifelse(printsex,titlesex,"")

            # market category
            if(j==0) titlemkt <- "whole catch, "
            if(j==1) titlemkt <- "discard, "
            if(j==2) titlemkt <- "retained, "
            titlemkt <- ifelse(printmkt,titlemkt,"")

            # plot bars for data only or if input 'fitbar=T'
            if(datonly | fitbar) bars <- T else bars <- F

            # aggregating identifiers for plot titles and filenames
            title_sexmkt <- paste(titlesex,titlemkt,sep="")
            filename_fltsexmkt <- paste("flt",f,"sex",sex,"mkt",j,sep="")

            ### subplot 1: multi-panel composition plot
            if(kind!="cond") # for age or length comps, but not conditional AAL
            {
              ptitle <- paste(titledata,title_sexmkt, FleetNames[f],sep="") # total title
              titles <- c(ptitle,titles) # compiling list of all plot titles
              tempfun <- function(ipage,...){
                  make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
                                sampsize=showsampsize*dbase$N,effN=showeffN*dbase$effN,bars=bars,linepos=(1-datonly)*linepos,
                                nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
                                main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=proplab,
                                maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,ipage=ipage,...)
              }
              if(GUI) tempfun(ipage=0,...)
              if(png){ # set up plotting to png file if required
                npages <- ceiling(length(unique(dbase$Yr))/maxrows/maxcols)
                for(ipage in 1:npages)
                {
                  if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                  filename <- paste(plotdir,filenamestart,filename_fltsexmkt,pagetext,".png",sep="")
                  png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                  tempfun(ipage=ipage,...)
                  dev.off()
                }
              }
            }

            # some things related to the next two bubble plots (single or multi-panel)
            if(datonly){
                z <- dbase$Obs
                col <- rep("black",2)
                titletype <- titledata
                filetype <- "bub"
                allopen <- TRUE
            }else{
                z <- dbase$Pearson
                col <- rep("blue",2)
                titletype <- "Pearson residuals, "
                filetype <- "resids"
                allopen <- FALSE
            }

            ### subplot 2: single panel bubble plot for numbers at length or age
            if(bub & kind!="cond")
            {
              ptitle <- paste(titletype, title_sexmkt, FleetNames[f],sep="")
              ptitle <- paste(ptitle," (max=",round(max(z),digits=2),")",sep="")
              titles <- c(ptitle,titles) # compiling list of all plot titles
              if(GUI) bubble3(x=dbase$Yr, y=dbase$Bin, z=z, xlab=yearlab,ylab=kindlab,col=col,
                      las=1,main=ptitle,cex.main=cex.main,maxsize=pntscalar,allopen=allopen,minnbubble=minnbubble)
              if(png){ # set up plotting to png file if required
                filename <- paste(plotdir,filenamestart,filetype,filename_fltsexmkt,".png",sep="")
                png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                bubble3(x=dbase$Yr, y=dbase$Bin, z=z, xlab=yearlab,ylab=kindlab,col=col,
                        las=1,main=ptitle,cex.main=cex.main,maxsize=pntscalar,allopen=allopen,minnbubble=minnbubble)
                dev.off() # close device if png
              }
            } # end bubble plot

            ### subplot 3: multi-panel bubble plots for conditional age at length
            if(kind=="cond")
            {
              ptitle <- paste(titletype, title_sexmkt, FleetNames[f],sep="")
              ptitle <- paste(ptitle," (max=",round(max(z),digits=2),")",sep="")
              titles <- c(ptitle,titles) # compiling list of all plot titles
              tempfun <- function(ipage,...){
                  make_multifig(ptsx=dbase$Bin,ptsy=dbase$Lbin_lo,yr=dbase$Yr,size=z,
                                sampsize=showsampsize*dbase$N,
                                nlegends=1,legtext=list(dbase$YrSeasName),
                                bars=F,linepos=0,main=ptitle,cex.main=cex.main,
                                xlab=agelab,ylab=lenlab,ymin0=F,maxrows=maxrows2,maxcols=maxcols2,
                                fixdims=fixdims,allopen=allopen,minnbubble=minnbubble,
                                ptscol=col[1],ptscol2=col[2],ipage=ipage,...)
              }
              if(GUI) tempfun(ipage=0,...)
              if(png){ # set up plotting to png file if required
                npages <- ceiling(length(unique(dbase$Yr))/maxrows2/maxcols2)
                for(ipage in 1:npages)
                {
                  if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                  filename <- paste(plotdir,filenamestart,filetype,filename_fltsexmkt,pagetext,".png",sep="")
                  png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                  tempfun(ipage=ipage,...)
                  dev.off() # close device if png
                }
              }
            } # end conditional bubble plot

            ### subplots 4 and 5: multi-panel plot of point and line fit to conditional age at length
            #                     and Pearson residuals of A-L key for specific years
            if(aalyear[1] > 0 & kind=="cond")
            {
              for(y in 1:length(aalyear))
              {
                aalyr <- aalyear[y]
                if(length(dbase$Obs[dbase$Yr==aalyr])>0)
                {
                  ### subplot 4: multi-panel plot of fit to conditional age at length for specific years
                  ptitle <- paste(aalyr," age at length bin, ",title_sexmkt,FleetNames[f],sep="")
                  titles <- c(ptitle,titles) # compiling list of all plot titles
                  ydbase <- dbase[dbase$Yr==aalyr,]
                  lenbinlegend <- paste(ydbase$Lbin_lo,lenunits,sep="")
                  tempfun <- function(ipage,...){ # temporary function to aid repeating the big function call
                    make_multifig(ptsx=ydbase$Bin,ptsy=ydbase$Obs,yr=ydbase$Lbin_lo,
                                  linesx=ydbase$Bin,linesy=ydbase$Exp,
                                  sampsize=showsampsize*ydbase$N,effN=showeffN*ydbase$effN,
                                  nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
                                  bars=F,linepos=linepos,main=ptitle,cex.main=cex.main,
                                  xlab=agelab,ylab=proplab,maxrows=maxrows,maxcols=maxcols,
                                  fixdims=fixdims,ipage=ipage,...)
                  }
                  if(GUI) tempfun(ipage=0,...)
                  if(png){
                    npages <- ceiling(length(unique(ydbase$Yr))/maxrows/maxcols)
                    for(ipage in 1:npages)
                    {
                      if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                      filename <- paste(plotdir,filenamestart,filename_fltsexmkt,"_",aalyr,"_",pagetext,".png",sep="")
                      png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                      tempfun(ipage=ipage,...)
                      dev.off() # close device if png
                    }
                  }

                  ### subplot 5: Pearson residuals for A-L key
                  z <- ydbase$Pearson
                  ptitle <- paste(aalyr," Pearson residuals for A-L key, ",title_sexmkt,FleetNames[f],sep="")
                  ptitle <- paste(ptitle," (max=",round(abs(max(z)),digits=2),")",sep="")
                  titles <- c(ptitle,titles) # compiling list of all plot titles
                  tempfun <- function(){
                    bubble3(x=ydbase$Bin,y=ydbase$Lbin_lo,z=z,xlab=agelab,ylab=lenlab,col=rep("blue",2),
                          las=1,main=ptitle,cex.main=cex.main,maxsize=pntscalar,allopen=F,minnbubble=minnbubble)
                  }
                  if(GUI) tempfun()
                  if(png)
                  {
                    filename <- paste(plotdir,filenamestart,"yearresids_",filename_fltsexmkt,"_",aalyr,".png",sep="")
                    png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                    tempfun()
                    dev.off() # close device if png
                  }
                }
              }
            }

            ### subplot 6: multi-panel plot of point and line fit to conditional age at length
            #              for specific length bins
            if(aalbin[1] > 0)
            {
              badbins <- setdiff(aalbin, dbase$Lbin_hi)
              goodbins <- intersect(aalbin, dbase$Lbin_hi)
              if(length(badbins)>0){
                  print(paste("Error! the following inputs for 'aalbin' do not match the Lbin_hi values for the conditional age at length data:",badbins),quote=F)
                  print(paste("       the following inputs for 'aalbin' are fine:",goodbins),quote=F)
              }
              if(length(goodbins)>0)
              {
                for(ibin in 1:length(goodbins)) # loop over good bins
                {
                  ilenbin <- goodbins[ibin]
                  abindbase <- dbase[dbase$Lbin_hi==ilenbin,]
                  if(nrow(abindbase)>0) # check for data associated with this bin
                  {
                    ptitle <- paste("Age at length ",ilenbin,lenunits,", ",title_sexmkt,FleetNames[f],sep="")
                    titles <- c(ptitle,titles) # compiling list of all plot titles
                    tempfun <- function(ipage,...){ # temporary function to aid repeating the big function call
                        make_multifig(ptsx=abindbase$Bin,ptsy=abindbase$Obs,yr=abindbase$Yr,linesx=abindbase$Bin,linesy=abindbase$Exp,
                                      sampsize=showsampsize*abindbase$N,effN=showeffN*abindbase$effN,
                                      nlegends=3,legtext=list(abindbase$YrSeasName,"sampsize","effN"),
                                      bars=bars,linepos=(1-datonly)*linepos,
                                      main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=proplab,maxrows=maxrows,maxcols=maxcols,
                                      fixdims=fixdims,ipage=ipage,...)
                    }
                    if(GUI) tempfun(ipage=0,...)
                    if(png){
                      npages <- ceiling(length(unique(abindbase$Yr))/maxrows/maxcols)
                      for(ipage in 1:npages)
                      {
                        if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
                        filename <- paste(plotdir,filenamestart,filename_fltsexmkt,"_length",ilenbin,lenunits,pagetext,".png",sep="")
                        png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                        tempfun(ipage=ipage,...)
                        dev.off() # close device if png
                      }
                    }
                  } # end if data
                } # end loop over length bins
              } # end if length(goodbins)>0
            } # end if plot requested

            ### subplot 7: sample size plot
            if(samplesizeplots & !datonly)
            {
              ptitle <- paste("N-EffN comparison, ",titledata,title_sexmkt,FleetNames[f], sep="")
              titles <- c(ptitle,titles) # compiling list of all plot titles
              lfitfunc <- function()
              {
                if(kind=="cond"){
                    # trap nonrobust effective n's
                    # should this only be for conditional age at length or all plots?
                    dbasegood <- dbase[dbase$Obs>=0.0001 & dbase$Exp<0.99 & !is.na(dbase$effN) & dbase$effN<maxneff,]
                }else{
                    dbasegood <- dbase
                }
                if(nrow(dbasegood)>0)
                {
                  plot(dbasegood$N,dbasegood$effN,xlab=osslab,main=ptitle,cex.main=cex.main,
                       ylim=c(0,1.05*max(dbasegood$effN)),xlim=c(0,1.05*max(dbasegood$N)),
                       col="blue",pch=19,ylab=esslab,xaxs='i',yaxs='i')
                  abline(h=0,col="grey")
                  abline(0,1,col="black")
                  # add loess smoother if there's at least 6 points with a range greater than 2
                  if(smooth & length(unique(dbasegood$N)) > 6 & diff(range(dbasegood$N))>2)
                  {
                    psmooth <- loess(dbasegood$effN~dbasegood$N,degree=1)
                    lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")
                  }
                }
              }
              if(GUI) lfitfunc()
              if(png){ # set up plotting to png file if required
                filename <- paste(plotdir,filenamestart,"sampsize_",filename_fltsexmkt,".png",sep="")
                png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                lfitfunc()
                dev.off()
              }
            }
          } # end loop over partitions
        } # end loop over combined/not-combined genders
      }# end if data
    } # end loop over fleet
    if(returntitles) return(titles)
  } # end embedded SSv3_plot_comps function
  ###########################

  # now make use of embedded SSv3_plot_comps function to make composition plots
  if(!datplot)
  {
    if(length(intersect(15:17,c(plot,print)))>0)
      print("skipped data-only plots 15-17 (comp data without fit) because input 'datplot=F'",quote=F)
  }else{
    if(15 %in% c(plot,print))  # data only aspects
    {
      # length comp bar plot
      SSv3_plot_comps(replist=replist,datonly=T,kind="LEN",bub=F,verbose=verbose,fleets=fleets,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
                      png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      # length comp bubble plot
      SSv3_plot_comps(replist=replist,datonly=T,kind="LEN",bub=T,verbose=verbose,fleets=fleets,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
                      png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      if(verbose) print("Finished plot 15: length comp data",quote=F)
      flush.console()
    }
    if(16 %in% c(plot,print))
    {
      # age comp bar plot
      SSv3_plot_comps(replist=replist,datonly=T,kind="AGE",bub=F,verbose=verbose,fleets=fleets,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
                      png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      # age comp bubble plot
      SSv3_plot_comps(replist=replist,datonly=T,kind="AGE",bub=T,verbose=verbose,fleets=fleets,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
                      png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      if(verbose) print("Finished plot 16: age comp data",quote=F)
      flush.console()
    }
    if(17 %in% c(plot,print))
    {
      # conditional age plot
      SSv3_plot_comps(replist=replist,datonly=T,kind="cond",bub=T,verbose=verbose,fleets=fleets,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
                      maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,
                      fixdims=fixdims,
                      png=(17%in%print),GUI=(17%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      if(verbose) print("Finished plot 17: conditional age at length data",quote=F)
      flush.console()
    }
  } # end if datplot

  # plot of length comp data with fits, sample size, etc.
  if(18 %in% c(plot,print)){
    SSv3_plot_comps(replist=replist,datonly=F,kind="LEN",bub=T,verbose=verbose,fleets=fleets,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
                    png=(18%in%print),GUI=(18%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,...)
    if(verbose) print("Finished plot 18: length comps with fits",quote=F)
    flush.console()
  }

  # plot of age comp data with fits, sample size, etc.
  if(19 %in% c(plot,print)){
    SSv3_plot_comps(replist=replist,datonly=F,kind="AGE",bub=T,verbose=verbose,fleets=fleets,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
                    png=(19%in%print),GUI=(19%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,...)
    if(verbose) print("Finished plot 19: age comps with fits",quote=F)
    flush.console()
  } # end if 19 in plot or print

  if(20 %in% c(plot,print)){
    SSv3_plot_comps(replist=replist,datonly=F,kind="cond",bub=T,verbose=verbose,fleets=fleets,
                    aalbin=aalbin,aalyear=aalyear,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                    maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,
                    png=(20%in%print),GUI=(20%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,...)
    if(verbose) print("Finished plot 20: conditional age at length with fits",quote=F)
    flush.console()
  } # end if 20 in plot or print

  # this didn't work, explore later: add a histogram of the Pearson residuals
  #
  # par(mfrow=c(2,2)) #pearsons <- mcmc(as.numeric(plotfems$Pearson))
  # pearsons[pearsons > 5] <- 5 #pearsons[pearsons < -5] <- -5
  # bins <- 30 #binwidth <- (max(pearsons) - min(pearsons))/bins
  # truehist(pearsons,h=binwidth,col="grey",xlim=c(-5,5),xlab="Pearson/Standard normal",ylab="Relative density")
  # # add a standard normal #distbins <- seq(-5,5,by=binwidth)
  # dens <- dnorm(x=distbins,mean=0,sd=1,log=FALSE) #lines(distbins,dens,col="black",lwd=1.5)
  # qq.plot(pearsons,ylab="Pearson residual",xlab="Standard normal",col=c("black")) #par(mfrow=c(1,1))


  # Plot 21: length at age data
  if(21 %in% c(plot, print))
  {
    latagebase <- compdbase[compdbase$Kind=="L@A" & compdbase$N > 0,]
    for(i in fleets)
    {
      if(length(latagebase$Obs[latagebase$Fleet==i])>0)
      {
        plotlens <- latagebase[latagebase$Fleet==i,]
        plotlens <- plotlens[!plotlens$Obs %in% c(NA),]
        testor <- length(plotlens$Obs[plotlens$Gender==1])>0
        testor[2] <- length(plotlens$Obs[plotlens$Gender==2])>0
        for(m in (1:2)[testor])
        {
          la <- plotlens[plotlens$Gender==m,] # females or males
          if(nseasons > 1){la$Yr <- la$Yr + (la$Seas - 1)/nseasons + 0.5/nseasons}
          la <- la[!is.na(la$Bin), ]

          # Disable until effective N calculation is re-implemented in SS
          #plottitle <- paste("Sample size for length-at-age for ",c("fe","")[m],"males, ", FleetNames[i],sep="")
          #ymax <- max(la$effN)
          #xmax <- max(la$N)
          #lenatagefunc <- function(){
            #plot(la$N,la$effN,xlab="Observed sample size",main=plottitle,ylim=c(0,ymax),xlim=c(0,xmax),col="blue",pch=19,ylab="Effective sample size")
            #abline(h=0,col="grey")
            #lines(x=c(0,ymax),y=c(0,ymax),col="black")
            #npoints <- length(la$N)
            #if(npoints > 6 & smooth & length(unique(la$N))>6){
             # psmooth <- loess(la$effN~la$N,degree=1)
             # lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
          #  }
          #if(21 %in% plot){lenatagefunc()}
          #if(21 %in% print)
          # {png(file=paste(plotdir,"21_lenatagesampsize_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
          # lenatagefunc()
          # dev.off()}
          plottitle <- paste("Length-at-age fits for sexes combined, ", FleetNames[i],sep="")
          if(nsexes > 1){plottitle <- paste("Length-at-age fits for ",c("fe","")[m],"males, ", FleetNames[i],sep="")}

          if(nseasons>1) la$YrSeasName <- paste(floor(la$Yr),"s",la$Seas,sep="") else la$YrSeasName <- la$Yr
          tempfun <- function(ipage,...){
            make_multifig(ptsx=la$Bin,ptsy=la$Obs,yr=la$Yr,linesx=la$Bin,linesy=la$Exp,
                        sampsize=showsampsize*la$N,effN=0,
                        nlegends=3,legtext=list(la$YrSeasName,"sampsize","effN"),
                        bars=F,linepos=1,
                        main=plottitle,cex.main=cex.main,xlab="Age (yr)",ylab="Length (cm)",
                        maxrows=maxrows,maxcols=maxcols,
                        fixdims=fixdims,ipage=ipage,...)
          }
          if(21 %in% plot) tempfun(ipage=0)
          if(21 %in% print){ # set up plotting to png file if required
            png(file=paste(plotdir,"21_lenatagefit_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            npages <- ceiling(length(unique(la$Yr))/maxrows/maxcols)
            for(ipage in 1:npages){
              tempfun(ipage=ipage,...)
            }
            dev.off()
          }

          plottitle <- paste("Pearson residuals for sexes combined, ", FleetNames[i],sep="")
          if(nsexes > 1){plottitle <- paste("Pearson residuals for ",c("fe","")[m],"males, ", FleetNames[i],sep="")}
          plottitle <- paste(plottitle," (max=",round(abs(max(la$Pearson)),digits=2),")",sep="")
          tempfun <- function(){
            bubble3(x=la$Yr,y=la$Bin,z=la$Pearson,xlab="Year",ylab="Age (yr)",col=rep("blue",2),
                    las=1,main=plottitle,cex.main=cex.main,maxsize=pntscalar,allopen=F,minnbubble=minnbubble)
          }
          if(21 %in% plot) tempfun()
          if(21 %in% print){
            png(file=paste(plotdir,"21_lenatageresids_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            tempfun()
            dev.off()
          }
        } # m
      } # if lengths
    } # fleets
    if(verbose) print("Finished plot 21: length at age data",quote=F)
    flush.console()
  } # end if 21 in plot or print

  # Yield curve
  if(22 %in% c(plot, print))
  {
    if(!is.null(equil_yield[1,1])){
      yieldfunc <- function(){
        plot(equil_yield$Depletion,equil_yield$Catch,xlab="Relative depletion",ylab="Equilibrium yield (mt)",
             type="l",lwd=2,col="blue")
        abline(h=0,col="grey")
        abline(v=0,col="grey")}
      if(22 %in% plot){yieldfunc()}
      if(22 %in% print){
        png(file=paste(plotdir,"22_yield.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        yieldfunc()
        dev.off()}
      if(verbose) print("Finished plot 22: yield curve",quote=F)
    }

    if(nareas==1)
    {
      ls <- nrow(ts)
      sprodfunc <- function(){
        totcatchmat <- as.matrix(ts[,substr(names(ts),1,nchar("enc(B)"))=="enc(B)"])
        ts$totcatch <- 0
        ts$totcatch[3:ls] <- rowSums(totcatchmat)[3:ls]
        ts$sprod <- NA
        ts$sprod[3:(ls-1)] <- ts$Bio_all[4:ls]-ts$Bio_all[3:(ls-1)]+ts$totcatch[3:(ls-1)]
        sprodgood <- !is.na(ts$sprod)
        Bio_all_good <- ts$Bio_all[sprodgood]
        sprod_good <- ts$sprod[sprodgood]
        xlim <- c(0,max(Bio_all_good,na.rm=T))
        ylim <- c(min(0,sprod_good,na.rm=T),max(sprod_good,na.rm=T))
        plot(Bio_all_good,sprod_good,ylim=ylim,xlim=xlim,xlab="Total biomass (mt)",ylab="Surplus production (mt)",type="l",col="black")

        # make arrows
        old_warn <- options()$warn  # previous setting
        options(warn=-1)            # turn off "zero-length arrow" warning
        s <- seq(length(sprod_good)-1)
        arrows(Bio_all_good[s],sprod_good[s],Bio_all_good[s+1],sprod_good[s+1],length=0.06,angle=20,col="black",lwd=1.2)
        options(warn=old_warn)      #returning to old value

        abline(h=0,col="grey")
        abline(v=0,col="grey")
        points(Bio_all_good[1],sprod_good[1],col="blue",pch=19)

      } # end sprodfunc
      if(22 %in% plot){sprodfunc()}
      if(22 %in% print){
        png(file=paste(plotdir,"22_surplus_prod.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        sprodfunc()
        dev.off()}
      if(verbose) print("Finished plot 22: Surplus production",quote=F)
    }
    if(nareas>1) print("Surplus production plot not implemented for multi-area models",quote=F)
  } # close plot section 22

  ### Plot 23: CPUE data-only plots ###
  if(23 %in% c(plot, print))
  {
    if(!datplot)
    {
      print("skipped plots 23 (CPUE without fit) because input 'datplot=F'",quote=F)
    }else{
      for(i in unique(cpue$Fleet)){
        cpueuse <- cpue[cpue$Obs > 0 & cpue$Fleet==i,]
        x <- cpueuse$Yr
        y <- cpueuse$Obs
        z <- cpueuse$Exp
        uiw <- qlnorm(.975,meanlog=log(y),sdlog=cpueuse$SE) - y
        liw <- y - qlnorm(.025,meanlog=log(y),sdlog=cpueuse$SE)
        main=paste("Index ", i,sep="")
        xlab <- "Observed index"
        if(23 %in% plot){
          plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab="Year",ylo=0,col="red",ylab="Index",main=main,lty=1)
          abline(h=0,col="grey")}
        if(23 %in% print){
          png(file=paste(plotdir,"23_cpue_dataonly",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
          plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab="Year",ylo=0,col="red",ylab="Index",main=main,lty=1)
          abline(h=0,col="grey")
          dev.off()}
      } # nfleets
      if(verbose) print("Finished plot 23: CPUE data plots",quote=F)
      flush.console()
    } # end if 23 in plot or print
  }

  if(verbose) print("Finished all requested plots",quote=F)
  ### end of SSv3_plots function
}
