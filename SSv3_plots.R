SSv3_plots <- function(
    replist="ReportObject", plot=1:21, print=0, printfolder="", dir="default", fleets="all", areas="all",
    fleetcols="default", areacols="default", verbose=T, uncertainty=T, forecastplot=F, datplot=F, Natageplot=T,
    sprtarg=0.4, btarg=0.4, minbthresh=0.25, pntscalar=2.6, minnbubble=8, aalyear=-1, aalbin=-1,
    aalresids=F, maxneff=5000, smooth=T, samplesizeplots=T, compresidplots=T, showsampsize=T,
    pwidth=7, pheight=7, punits="in", ptsize=12, res=300, cex.main=1,
    maxrows = 6, maxcols = 6, maxrows2 = 2, maxcols2 = 4, fixrows = T, fixcols = T, newcompplots=F,...)
{
################################################################################
#
# SSv3_plots BETA March 19, 2009.
#
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: To sumarize the results of an SSv3 model run.
# Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
#          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
# Returns: Plots with plot history in R GUI and/or .png files.
# General: Updated for Stock Synthesis version 3.02B through 3.02F; R version 2.8.1
# Notes:   See users guide for documentation.
# Required SS3v_output function and lattice package
# Credit:  Based loosely on an early version of "Scape" (A. Magnusson) and "Output viewer" (R. Methot)
#
################################################################################

# if you have never installed the required package,
# copy and paste the following install.packages command (without the "#").
# Note: this may require administrative priviledges on your computer
#
# install.packages(lattice)
# install.packages(plotrix)

  # load required package
  require(lattice)
  require(plotrix)
  if(newcompplots & !exists("SSv3_plot_comps"))
      print("to use newcompplots=T, you need to source the file 'SSv3_plot_comps.R'")

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
  bubble2 <- function (data,xcol=1,ycol=2,zcol=3,fill=TRUE,maxsize=3,do.sqrt=TRUE,pch,col=c(2,3),
                       key.entries=quantile(data[,zcol]),main=ifelse(is.numeric(zcol),names(data)[zcol],zcol),
                       identify=FALSE,labels=row.names(data),...)
  {
    # Modified version of bubble() from gstat to eliminate the default key
    x = data[, xcol]
    y = data[, ycol]
    z = data[, zcol]
    d = data.frame(x = x, y = y)
    if(missing(pch)){pch = ifelse(fill, 16, 1)}
    z.col = ifelse(z < 0, col[1], col[2])
    q = key.entries
    q.pch = rep(pch, length(q))
    q.text = as.character(round(q, 3))
    q.col = ifelse(q < 0, col[1], col[2])
    az = abs(z)
    q = abs(q)
    if (do.sqrt){
      az = sqrt(az)
      q = sqrt(q)}
    cex = maxsize * az/max(az)
    q.cex = maxsize * q/max(az)
    if (identify){
      plot(data[,xcol],data[,ycol],asp=1,cex=cex,main=main, ...)
      return(identify(data[, xcol], data[, ycol], labels))}
    key = list(space="right",points=list(pch=q.pch,col=q.col,cex=q.cex),text=list(q.text))
    xyplot(y~x,d,col=z.col,cex=cex,pch=pch,main=main,...)
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

  if(replist[[1]]=="ReportObject"){
    return("The input 'replist' should refer to an R object created by the function 'SSv3_output'.")
  }

  # get quantities from the big list
  # this could also be done using attach()
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
    if(OS=="Windows") windows(record=TRUE)
    if(OS=="Linux") X11()
    if(OS=="Mac") quartz()
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
      intret <- retention[retention$gender==m,]
      intselex <- sizeselex[sizeselex$gender==m,]
      for(i in fleets)
      {
        plotselex <- intselex[intselex$Fleet==i,]
        time <- FALSE
        for(t in 4 + 1:nlbinspop) if(length(unique(plotselex[,t]))>1){time <- TRUE}
        if(m==1 & nsexes==1) sextitle <- "Time-"
        if(m==1 & nsexes==2) sextitle <- "Female time-"
        if(m==2) sextitle="Male time-"
        if(time)
        {
          x <- lbinspop
          y <- plotselex$year
          z <- plotselex[,-(1:4)]
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
        for(t in 4 + 1:nlbinspop) if(length(unique(intret[intret$Fleet==i,t]))>1){time2 <- TRUE}
        if(time2)
        {
          x <- lbinspop
          y <- as.numeric(intret$year[intret$Fleet==i])
          z <- intret[intret$Fleet==i,-(1:4)]
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
        plotselex <- plotselex[plotselex$year==endyr & plotselex$gender==m,-(1:4)]
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
          retcheck <- as.numeric(retchecktemp[5:length(retchecktemp)])
          if(is.na(sum(retcheck))) retcheckuse <- 0
          if(!is.na(sum(retcheck))) retcheckuse <- max(retcheck)-min(retcheck)
          if(retcheckuse==0 & max(vals)-min(vals)!=0) legend("bottomright",inset=c(0,0.05),bty="n","Selectivity",pch=21,pt.bg="white",lty=1,col="blue")
          if(retcheckuse > 0){
            useret <- intret[intret$Fleet==i,]
            plotret <- useret[useret$year==max(as.numeric(useret$year)),]
            lines((as.numeric(as.vector(names(plotret)[-(1:4)]))),(as.numeric(as.character(plotret[1,-(1:4)]))),col="red",type="l",cex=1.1)
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
        plotageselex <- ageselex[ageselex$fleet==i & ageselex$gender==m,]
        time <- FALSE
        for(t in 5+0:accuage){if(length(unique(plotageselex[,t]))>1){time <- TRUE}}
        if(time)
        {
          if((min(as.numeric(as.vector(t(plotageselex[,-(1:4)])))) < 1))
          {
            x <- seq(0,accuage,by=1)
            y <- as.numeric(plotageselex$year)
            z <- plotageselex[,-(1:4)]
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
            plotageselex2 <- plotageselex2[,-(1:4)]
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
          plotageselex <- plotageselex[as.numeric(plotageselex$year)==endyr,]
          plotageselex <- plotageselex[,-(1:4)]
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

    # total landings (not adapted for multi-area models)
    ls <- nrow(ts)
    totretainedmat <- as.matrix(ts[,substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
    ts$totretained <- 0
    ts$totretained[3:ls] <- rowSums(totretainedmat)[3:ls]
    polyts <- data.frame(ts$Yr[3:(ls-1)])
    row.names(polyts) <- seq(1,length(polyts[,1]),by=1)
    polyts <- polyts[,rep(1,nfishfleets)]
    landfunc <- function(){
      plot(ts$Yr[2:(ls-1)],ts$totretained[2:(ls-1)],xlab="Year",ylab="Landings (mt)",type="o",col="black")
      abline(h=0,col="grey")
      for(xx in 1:nfishfleets){
       lines(ts$Yr[3:(ls-1)],totretainedmat[3:(ls-1),xx],type="l",col=fleetcols[xx])
       polyts <- cbind(polyts,totretainedmat[3:(ls-1),xx])
       }
      if(nfishfleets > 1){
      stackpoly(x=polyts[,(nfishfleets+1):(nfishfleets*2)],xaxlab=as.character(c(polyts[,1])),
                ylim=c(0,max(ts$totretained[2:(ls-1)])),stack=T,axis4=F,
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
    if(max(ts$totcatch[3:ls] - ts$totretained[3:ls] != 0))
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
    fmax <- max(Hrates)
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
    if(!(max(ts$discardall[1:ls])==0))
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
    if(max(ts$discfrac>0)){
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
      recstd <- matchfun2("VirginRecr",0,"SPRratio",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=derived_quants,objsubset=derived_quants,substr1=TRUE,substr2=TRUE)
      recstd$Yr <- substring(recstd$LABEL,6,nchar(recstd$LABEL[1])-1)
      recstd$Yr[2] <- as.numeric(recstd$Yr[3])-1
      recstd$Yr[1] <- as.numeric(recstd$Yr[2])-1
      recstd$Yr <- as.numeric(recstd$Yr)
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
      sbstd <- matchfun2("VirginSPB",0,"VirginRecr",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=derived_quants,objsubset=derived_quants,substr1=TRUE,substr2=TRUE)
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
  if(nseasons ==1){ ## Temporary filter until calculations can be cleaned up
   if(8 %in% c(plot, print))
    {
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
  } # end if 8 in plot or print
} # end temporary exclusion of multi-season models

  # Plot 9: rec devs and asymptotic error check
  if(9 %in% c(plot, print))
  {
    recdev <- parameters[substring(parameters$Label,1,7)=="RecrDev",]
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

if(nseasons>1) print("skipping 1-SPR series plot because it's not yet configured for multi-season models",quote=F)
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
        if(max(morph_indexing$Index) > nareas*nsexes)
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
          plotbub <- cbind(resx,resy,resz)
          if(m==1 & nsexes==1) sextitle <- ""
          if(m==1 & nsexes==2) sextitle <- " of females"
          if(m==2) sextitle=" of males"
          if(nareas>1) sextitle <- paste("in area",iarea,sextitle)
          plottitle <- paste("Expected numbers",sextitle," at age in thousands (max=",max(resz),")",sep="")

          trellis.device(theme=col.whitebg(),new=F)
          nage <- bubble2(plotbub,xlab="Year",ylab="Age (yr)",col=c("black","black"),main=plottitle,maxsize=(pntscalar+1.0),
                        key.entries=c(0.0),pch=c(NA,1)[1+(resz>0)],scales=list(relation="same",alternating="1",tck=c(1,0)))
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
            print(nage)
            plot(meanageyr,meanage,xlab="Year",ylim=ylim,type="o",ylab=ylab,col="black",main=plottitle)}
          if(14 %in% print){
            filepart <- paste("_sex",m,sep="")
            if(nareas > 1) filepart <- paste("_area",iarea,filepart,sep="")
            png(file=paste(plotdir,"14_natage",filepart,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            print(nage)
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


if(!newcompplots) # switch to allow transition to new non-trellis composition plots
{
  # Plots of data only
  lendbase   <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
  agedbase   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0,]
  latagebase <- compdbase[compdbase$Kind=="L@A" & compdbase$N > 0,]
  lendbase$effN <- as.numeric(lendbase$effN)
  agedbase$effN <- as.numeric(agedbase$effN)

  if(length(intersect(15:17,c(plot, print)))>0) # plots 15-17
  if(!datplot)
  {
    print("skipped data-only plots 15-17 because input 'datplot=F'",quote=F)
  }else{
   # Index data plots only
    for(i in unique(cpue$Fleet)){
      cpueuse <- cpue[cpue$Obs > 0 & cpue$Fleet==i,]
      x <- cpueuse$Yr
      y <- cpueuse$Obs
      uiw <- qlnorm(.975,meanlog=log(y),sdlog=cpueuse$SE) - y
      liw <- y - qlnorm(.025,meanlog=log(y),sdlog=cpueuse$SE)
      main=paste("Index ", i,sep="")
      xlab <- "Observed index"
      if(13 %in% plot){
        plotCI(x=x,y=y,z=y,sfrac=0.001,uiw=uiw,liw=liw,xlab="Year",ylo=0,col="red",ylab="Index",main=main,lty=1)
        abline(h=0,col="grey")}
      if(13 %in% print){
        png(file=paste(plotdir,"13cpuedata",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        plotCI(x=x,y=y,z=y,sfrac=0.001,uiw=uiw,liw=liw,xlab="Year",ylo=0,col="red",ylab="Index",main=main,lty=1)
        abline(h=0,col="grey")
        dev.off()}
    } # nfleets

    # length data
    for(i in fleets)
    {
      if(length(lendbase$Obs[lendbase$Fleet==i])>0)
      {
        ldatall <- lendbase[lendbase$Fleet==i,]
        testor    <- length(ldatall$Gender[ldatall$Gender==1 & ldatall$Pick_gender==0 ])>0
        testor[2] <- length(ldatall$Gender[ldatall$Gender==1 & ldatall$Pick_gender %in% c(1,3)])>0
        testor[3] <- length(ldatall$Gender[ldatall$Gender==2])>0
        for(k in (1:3)[testor])
        {
          if(k==1){ldat <- ldatall[ldatall$Gender==1 & ldatall$Pick_gende==0,]}
          if(k==2){ldat <- ldatall[ldatall$Gender==1 & ldatall$Pick_gende %in% c(1,3),]}
          if(k==3){ldat <- ldatall[ldatall$Gender==2,]}
          for(j in unique(ldat$Part))
          {
            ldat2 <- ldat[ldat$Part%in%j,]
            if(nseasons > 1){ldat2$Yr <- ldat2$Yr + (ldat2$Seas - 1)/nseasons + 0.5/nseasons}
            ldat2$plotyear <- as.factor(ldat2$Yr)
            ldat2$plotbins <- ldat2$Bin
            ldat2 <- ldat2[!is.na(ldat2$plotbins),]
            ldat2$plotobs <- ldat2$Obs
            bincounter <- as.integer(nlbins/6)
            usebins <- seq(1,bincounter*6,by=bincounter)
            binlabs <- as.character(lbins[usebins])
            if(k==1){
              ptitle <- paste("Sexes combined discard lengths for ", FleetNames[i],sep="")
              if(j==2){ptitle <- paste("Sexes combined retained lengths for ", FleetNames[i],sep="")}
              if(j==0){ptitle <- paste("Sexes combined whole catch lengths for ", FleetNames[i],sep="")}}
            if(k==2){
              ptitle <- paste("Female discard lengths for ", FleetNames[i],sep="")
              if(j==2){ptitle <- paste("Female retained lengths for ", FleetNames[i],sep="")}
              if(j==0){ptitle <- paste("Female whole catch lengths for ", FleetNames[i],sep="")}}
            if(k==3){
              ptitle <- paste("Male discard lengths for ", FleetNames[i],sep="")
              if(j==2){ptitle <- paste("Male retained lengths for ", FleetNames[i],sep="")}
              if(j==0){ptitle <- paste("Male whole catch lengths for ", FleetNames[i],sep="")}}
            trellis.device(theme=col.whitebg(),new=FALSE)
            trellis2 <-barchart(plotobs~plotbins|plotyear,as.table=T,ylab="Proportion",xlab="Length bin (cm)",col="GREY",main=ptitle,
                                box.ratio=100,subset=TRUE,strip=strip.custom(bg="grey"),horizontal=FALSE,groups=NULL,
                                scales=list(y=list(limits=c(0,max(ldat2$plotobs)+0.02)),x=list(limits=c(0,(nlbins+1)),at=(usebins),labels=(binlabs)),
                                relation="same",alternating="1",tck=c(1,0)),data=ldat2)

            if(15 %in% plot) print(trellis2)
            if(15 %in% print){
              sex <- 1
              if(k==3) sex <- 2
              png(file=paste(plotdir,"15lendatbar_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              print(trellis2)
              dev.off()}
            resx <- as.numeric(as.character(ldat2$plotyear))
            resy <- as.numeric(as.character(ldat2$plotbins))
            resz <- ldat2$plotobs
            if(length(unique(resx))<minnbubble){
              resx <- c(resx,(max(resx)+1),(min(resx)-1))
              resy <- c(resy,(min(resy)),(min(resy)))
              resz <- c(resz,0,0)}
            plotbub <- cbind(resx,resy,resz)
            pch <- resz
            pch[pch>0] <- 1
            pch[pch<=0] <- NA
            ptitle <- paste(ptitle," (max=",round(max(resz),digits=2),")",sep="")
            bub <- bubble2(plotbub,xlab="Year",ylab="Length (cm)",col=c("black","black"),main=ptitle,maxsize=pntscalar,
                           key.entries=c(0.0),pch=pch,scales=list(relation="same",alternating="1",tck=c(1,0)))
            if(15 %in% plot) print(bub)
            if(15 %in% print){
              sex <- 1
              if(k==3){sex <- 2}
              png(file=paste(plotdir,"15_lendatbub_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              print(bub)
              dev.off()}
          } # market
        } # k sexes
      } # fleet state
    } # fleet loop
    if(verbose) print("Finished plot 15: length comp data ",quote=F)

    # age data
    for(i in fleets) # loop over fleets
    {
      if(length(agedbase$Obs[agedbase$Fleet==i])>0) # if there are ages for fleet i
      {
        plotlendat <- agedbase[agedbase$Fleet==i,]
        if(length(plotlendat$Obs[plotlendat$Lbin_hi!=plotlendat$Lbin_lo])>0) # check for non-conditional data
        {
          plotlendat <- plotlendat[plotlendat$Lbin_hi!=plotlendat$Lbin_lo,]# trap out the conditional age data
          testor    <- length(plotlendat$Gender[plotlendat$Gender==1 & plotlendat$Pick_gender==0])>0
          testor[2] <- length(plotlendat$Gender[plotlendat$Gender==1 & plotlendat$Pick_gender %in% c(1,3)])>0
          testor[3] <- length(plotlendat$Gender[plotlendat$Gender==2])>0
          for(k in (1:3)[testor])
          {
            if(k==1){adat <- plotlendat[plotlendat$Gender==1 & plotlendat$Pick_gender==0,]} # combined sex
            if(k==2){adat <- plotlendat[plotlendat$Gender==1 & plotlendat$Pick_gender %in% c(1,3),]} # females
            if(k==3){adat <- plotlendat[plotlendat$Gender==2,]} # males
            for(j in unique(adat$Part))
            {
              adat2 <- adat[adat$Part%in%j,]
              if(nseasons > 1){adat2$Yr <- adat2$Yr + (adat2$Seas - 1)/nseasons + 0.5/nseasons}
              adat2$plotyear <- as.factor(adat2$Yr)
              adat2$plotbins <- adat2$Bin
              adat2 <- adat2[!is.na(adat2$plotbins), ]
              adat2$plotobs <- adat2$Obs
              maxbin <- max(agebins)
              minbin <- min(agebins)
            bincounter <- as.integer(nagebins/6)
            usebins <- seq(1,bincounter*6,by=bincounter)
            binlabs <- as.character(agebins[usebins])
              if(k==1){
                plottitle <- paste("Sexes combined discard ages for ", FleetNames[i],sep="")
                if(j==2){plottitle <- paste("Sexes combined retained ages for ", FleetNames[i],sep="")}
                if(j==0){plottitle <- paste("Sexes combined whole catch ages for ", FleetNames[i],sep="")}}
              if(k==2){
                plottitle <- paste("Female discard ages for ", FleetNames[i],sep="")
                if(j==2){plottitle <- paste("Female retained ages for ", FleetNames[i],sep="")}
                if(j==0){plottitle <- paste("Female whole catch ages for ", FleetNames[i],sep="")}}
              if(k==3){
                plottitle <- paste("Male discard ages for ", FleetNames[i],sep="")
                if(j==2){plottitle <- paste("Male retained ages for ", FleetNames[i],sep="")}
                if(j==0){plottitle <- paste("Male whole catch ages for ", FleetNames[i],sep="")}}
                trellis.device(theme=col.whitebg(),new = FALSE)
                trellis2 <-barchart(plotobs~plotbins|plotyear,as.table=T,ylab="Proportion",xlab="Age bin (yr)",col="GREY",main=plottitle,
                                    box.ratio=80,subset=TRUE,strip=strip.custom(bg="grey"),horizontal=FALSE,groups=NULL,
                                    scales=list(y=list(limits=c(0,max(adat2$plotobs)+0.02)),drop.unused.levels=F,
                                   # x=list(tick.number=5,limits=c(0,(maxbin+1))),relation="same",alternating="1",tck=c(1,0)),
                                    x=list(limits=c(0,(nagebins+1)),at=(usebins),labels=(binlabs)),relation="same",alternating="1",tck=c(1,0)),
                                    data=adat2)
              if(16 %in% plot) print(trellis2)
              if(16 %in% print)
              { sex <- 1
                if(k==3){sex <- 2}
                png(file=paste(plotdir,"16_agedatbar_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                print(trellis2)
                dev.off()}
              resx <- as.numeric(as.character(adat2$plotyear))
              resy <- as.numeric(as.character(adat2$plotbins))
              resz <- adat2$plotobs
              if(length(unique(resx))<minnbubble)
              { resx <- c(resx,(max(resx)+1),(min(resx)-1))
                resy <- c(resy,(min(resy)),(min(resy)))
                resz <- c(resz,0,0)}
              plotbub <- cbind(resx,resy,resz)
              pch <- resz
              pch[pch>0] <- 1
              pch[pch<=0] <- NA
              plottitle <- paste(plottitle," (max=",round(max(resz),digits=2),")",sep="")
              bub <- bubble2(plotbub,xlab="Year",ylab="Age (yr)",col=c("black","black"),main=plottitle,maxsize=pntscalar,
                             key.entries=c(0.0),pch=pch,scales=list(relation="same",alternating="1",tck=c(1,0)))
              if(16 %in% plot){print(bub)}
              if(16 %in% print)
              { sex <- 1
                if(k==3){sex <- 2}
                png(file=paste(plotdir,"16_lendatbub_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                print(trellis2)
                dev.off()}
            } # market
          } # k loop
        } # non-conditional check
      } # if ages
    } # fleets
    if(verbose) print("Finished plot 16: age comp data ",quote=F)

    # conditional data
    for(i in fleets)
    {
      if(length(agedbase$Obs[agedbase$Fleet==i])>0)
      {
        agedbasefleet <- agedbase[agedbase$Fleet==i & agedbase$Lbin_hi==agedbase$Lbin_lo,]
        testor <- length(agedbasefleet$Obs[agedbasefleet$Gender==1])>0
        testor[2] <- length(agedbasefleet$Obs[agedbasefleet$Gender==2])>0
        for(m in (1:2)[testor])
        {
          cadat <- agedbasefleet[agedbasefleet$Gender==m,]
          cadat$emptytrap <- cadat$Bin*2
          cadat$emptytrap[is.na(cadat$emptytrap)] <- -1
          cadat <- cadat[cadat$emptytrap > 0,]
          ntrell <- nrow(cadat)
          cadat$obsexp <- "obs"
          cadat$trellval <- cadat$Obs
          cadat <- rbind(cadat,cadat)
          cadat$obsexp[1:ntrell] <- "exp"
          cadat$trellval[1:ntrell] <- cadat$Exp[1:ntrell]
          cadat$plotbins <- cadat$Bin
          cadat$lenbin <- cadat$Lbin_hi
          cadat <- cadat[order(cadat$lenbin),]
          #if(Lbin_method==3)
          cadat$lenbin2 <- cadat$lenbin
          cadat$lenbin <- as.factor(cadat$lenbin2)
          cadat$group <- cadat$obsexp
          cadat$group[cadat$obsexp=="exp"] <- cadat$Yr[cadat$obsexp=="exp"]
          years <- unique(cadat$Yr)
          years <- years[order(years)]
          nyears <- length(years)

          if(17 %in% plot)
          {
            plotspot <- 2
            plotspot2 <- 1
            plot.new()
            text(0.5,0.5,paste("Conditional age data for ",c("fe","")[m],"males from ", FleetNames[i],sep=""),cex=1)
            for(fy in 1:nyears)
            {
              more <- T
              if(fy - (as.integer(fy/8)*8) == 0) {more <-F}
              cadatuse <- cadat[cadat$Yr %in% years[fy],]
              resx <- cadatuse$plotbins
              resy <- cadatuse$lenbin2
              resz <- cadatuse$Obs
              plottitle <- paste(years[fy])
              plotbub <- cbind(resx,resy,resz)
              pch <- resz
              pch[pch>0] <- 1
              pch[pch<=0] <- NA
              if(plotspot %in% 2){ylab="Length bin (cm)"}
              if(plotspot %in% c(3,4,5)){ylab=""}
              plottitle <- paste("        ",plottitle," (max=",round(max(abs(resz)),digits=2),")",sep="")
              main <- list()
              main$label <- plottitle
              main$cex <- 0.8
              bub <- bubble2(plotbub,xlab="Age",ylab=ylab,col=c("black","black"),main=main,maxsize=pntscalar,
                             ylim=c(min(lbins)-3,max(lbins)+3),key.entries=c(0.0),pch=pch,scales=list(relation="same",alternating="1",tck=c(1,0)))
              print(bub,split=c(plotspot,plotspot2,6,2),more=more,panel.width=list(0.7,"npc"))
              plotspot <- plotspot +1
              if(plotspot==6){plotspot2 <- plotspot2 +1}
              if(plotspot2==3){plotspot2 <- 1}
              if(plotspot==6){plotspot <- 2}
            } # nyears
          } # plot
          if(17 %in% print)
          {
            page <- 1
            plotspot <- 2
            plotspot2 <- 1
            png(file=paste(plotdir,"17_ageatlen_flt",i,"sex",m,"page",page,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            for(fy in 1:nyears)
            {
              more <- T
              if(fy - (as.integer(fy/8)*8) == 0) {more <-F}
              cadatuse <- cadat[cadat$Yr==years[fy],]
              resx <- cadatuse$plotbins
              resy <- cadatuse$lenbin2
              resz <- cadatuse$Obs
              plottitle <- paste(years[fy],sep="")
              plotbub <- cbind(resx,resy,resz)
              pch <- resz
              pch[pch>0] <- 1
              pch[pch<=0] <- NA
              if(plotspot %in% 2){ylab="Length bin (cm)"}
              if(plotspot %in% 3:5){ylab=""}
              plottitle <- paste("        ",plottitle," (max=",round(max(abs(resz)),digits=2),")",sep="")
              main <- list()
              main$label <- plottitle
              main$cex <- 0.8
              bub <- bubble2(plotbub,xlab="Age",ylab=ylab,col=c("black","black"),main=main,maxsize=pntscalar,
                             ylim=c(min(lbins)-3,max(lbins)+3),key.entries=c(0.0),pch=pch,scales=list(relation="same",alternating="1",tck=c(1,0)))
              print(bub,split=c(plotspot,plotspot2,6,2),more=more,panel.width=list(0.7,"npc"))
              plotspot <- plotspot +1
              if(plotspot==6){plotspot2 <- plotspot2 +1}
              if(plotspot==6){plotspot <- 2}
              if((plotspot2==3) | (fy==nyears))
              {
                dev.off()
                plotspot2 <- 1
                page <- page + 1
                if(!(fy==nyears)){png(file=paste(plotdir,"17_ageatlen_flt",i,"sex",m,"page",page,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)}
              }
            } # nyears
          } # print
        } # m
      } # if fleets
    } # fleet loop
    if(verbose) print("Finished plot 17: conditional age at length data ",quote=F)
    flush.console()
  } # if datplot

  # Plot 18: length comps with fits
  if(18 %in% c(plot, print))
  {
    for(i in fleets)
    {
      if(length(lendbase$Obs[lendbase$Fleet==i])>0)
      {
        plotlens <- lendbase[lendbase$Fleet==i,]
        testor    <- length(plotlens$Gender[plotlens$Gender==1 & plotlens$Pick_gender==0])>0
        testor[2] <- length(plotlens$Gender[plotlens$Gender==1 & plotlens$Pick_gender %in% c(1,3)])>0
        testor[3] <- length(plotlens$Gender[plotlens$Gender==2])>0
        for(k in (1:3)[testor])
        {
          if(k==1){lfit <- plotlens[plotlens$Gender==1 & plotlens$Pick_gender==0,]} # combined sex
          if(k==2){lfit <- plotlens[plotlens$Gender==1 & plotlens$Pick_gender %in% c(1,3),]} # females
          if(k==3){lfit <- plotlens[plotlens$Gender==2,]} # males
          for(j in unique(lfit$Part))
          {
            lfit2 <- lfit[lfit$Part %in% j,]
            if(nseasons > 1){lfit2$Yr <- lfit2$Yr + (lfit2$Seas - 1)/nseasons + 0.5/nseasons}
            lfit2$plotyear <- as.factor(lfit2$Yr)
            lfit2$plotbins <- lfit2$Bin
            lfit2 <- lfit2[!is.na(lfit2$plotbins),]
            lfit2$plotobs <- lfit2$Obs
            lfit2$plotexp <- lfit2$Exp
            if(k==1){
              plottitle <- paste("Sample size for sexes combined discard lengths for ", FleetNames[i],sep="")
              if(j==2){plottitle <- paste("Sample size for sexes combined retained lengths for ", FleetNames[i],sep="")}
              if(j==0){plottitle <- paste("Sample size for sexes combined whole catch lengths for ", FleetNames[i],sep="")}}
            if(k==2){
              plottitle <- paste("Sample size for female discard lengths for ", FleetNames[i],sep="")
              if(j==2){plottitle <- paste("Sample size for female retained lengths for ", FleetNames[i],sep="")}
              if(j==0){plottitle <- paste("Sample size for female whole catch lengths for ", FleetNames[i],sep="")}}
            if(k==3){
              plottitle <- paste("Sample size for male discard lengths for ", FleetNames[i],sep="")
              if(j==2){plottitle <- paste("Sample size for male retained lengths for ", FleetNames[i],sep="")}
              if(j==0){plottitle <- paste("Sample size for male whole catch lengths for ", FleetNames[i],sep="")}}
            ymax <- max(lfit2$effN)
            xmax <- max(lfit2$N)
    if(samplesizeplots){
            lfitfunc <- function()
            {
              plot(lfit2$N,lfit2$effN,xlab="Observed sample size",main=plottitle,ylim=c(0,ymax),xlim=c(0,xmax),col="blue",pch=19,ylab="Effective sample size")
              abline(h=0,col="grey")
              lines(x=c(0,ymax),y=c(0,ymax),col="black")
              npoints <- length(lfit2$N)
              if(npoints > 6 & smooth & length(unique(lfit2$N))>6)
              { psmooth <- loess(lfit2$effN~lfit2$N,degree=1)
                lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
            }
            if(18 %in% plot) lfitfunc()
            if(18 %in% print){
              sex <- 1
              if(k==3){sex <- 2}
              png(file=paste(plotdir,"18_lendatfitsampsize_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              lfitfunc()
              dev.off()}
    }
            if(k==1){
              plottitle <- paste("Combined sex discard length fits for ", FleetNames[i],sep="")
              if(j==2){plottitle <- paste("Combined sex retained length fits for ", FleetNames[i],sep="")}
              if(j==0){plottitle <- paste("Combined sex whole catch length fits for ", FleetNames[i],sep="")}}
            if(k==2){
              plottitle <- paste("Female discard length fits for ", FleetNames[i],sep="")
              if(j==2){plottitle <- paste("Female retained length fits for ", FleetNames[i],sep="")}
              if(j==0){plottitle <- paste("Female whole catch length fits for ", FleetNames[i],sep="")}}
            if(k==3){
              plottitle <- paste("Male discard length fits for ", FleetNames[i],sep="")
              if(j==2){plottitle <- paste("Male retained length fits for ", FleetNames[i],sep="")}
              if(j==0){plottitle <- paste("Male whole catch length fits for ", FleetNames[i],sep="")}}

            trellis.device(theme=col.whitebg(),new = FALSE)
            trellfems <- lfit2
            ntrell <- nrow(trellfems)
            trellfems$obsexp <- "obs"
            trellfems$trellval <- trellfems$plotobs
            trellfems <- rbind(trellfems,trellfems)
            trellfems$obsexp[1:ntrell] <- "exp"
            trellfems$trellval[1:ntrell] <- trellfems$plotexp[1:ntrell]
            trellis1 <- xyplot(trellval~plotbins|plotyear,as.table=T,groups=obsexp,type=c("l","p"),pch=c(NA,1),lty=c(1,0),lwd=1.5,
                               strip=strip.custom(bg="grey"),ylab="Proportion",xlab="Length bin (cm)",col=c("red","black","red","black"),
                               cex=0.6,main=plottitle,scales=list(relation="same",alternating="1",tck=c(1,0)),data=trellfems)
            if(17 %in% plot){print(trellis1)}
            if(17 %in% print){
              sex <- 1
              if(k==3){sex <- 2}
              png(file=paste(plotdir,"18_lendatfit_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              print(print(trellis1))
              dev.off()}

            if(compresidplots){
              resx <- as.numeric(lfit2$Yr)
              resy <- as.numeric(lfit2$plotbins)
              resz <- as.numeric(lfit2$Pearson)
              if(k==1){
                plottitle <- paste("Combined sex discard Pearson residuals for ", FleetNames[i],sep="")
                if(j==2){plottitle <- paste("Combined sex retained Pearson residuals for ", FleetNames[i],sep="")}
                if(j==0){plottitle <- paste("Combined sex whole catch Pearson residuals for ", FleetNames[i],sep="")}}
              if(k==2){
                plottitle <- paste("Female discard Pearson residuals for ", FleetNames[i],sep="")
                if(j==2){plottitle <- paste("Female retained Pearson residuals for ", FleetNames[i],sep="")}
                if(j==0){plottitle <- paste("Female whole catch Pearson residuals for ", FleetNames[i],sep="")}}
              if(k==3){
                plottitle <- paste("Male discard Pearson residuals for ", FleetNames[i],sep="")
                if(j==2){plottitle <- paste("Male retained Pearson residuals for ", FleetNames[i],sep="")}
                if(j==0){plottitle <- paste("Male whole catch Pearson residuals for ", FleetNames[i],sep="")}}
              if(length(unique(resx))<minnbubble){
                resx <- c(resx,(max(resx)+1),(min(resx)-1))
                resy <- c(resy,(min(resy)),(min(resy)))
                resz <- c(resz,0,0)}
              plotbub <- cbind(resx,resy,resz)
              pch <- resz
              pch[pch==0] <- NA
              pch[pch>0] <- 16
              pch[pch<0] <- 1
              plottitle <- paste(plottitle," (max=",round(abs(max(resz)),digits=2),")",sep="")
              bub <- bubble2(plotbub,xlab="Year",ylab="Length bin (cm)",col=c("blue","blue"),main=plottitle,maxsize=pntscalar,
                           key.entries=c(0.0),pch=pch,scales=list(relation="same",alternating="1",tck=c(1,0)))
              if(18 %in% plot) print(bub)
              if(18 %in% print){
                sex <- 1
                if(k==3){sex <- 2}
                png(file=paste(plotdir,"18_lendatresids_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                print(bub)
                dev.off()}
            } # end if compresidplots
            ### this didn't work, explore later: add a histogram of the Pearson residuals
            #par(mfrow=c(2,2)) #pearsons <- mcmc(as.numeric(plotfems$Pearson))
            #pearsons[pearsons > 5] <- 5 #pearsons[pearsons < -5] <- -5
            #bins <- 30 #binwidth <- (max(pearsons) - min(pearsons))/bins
            #truehist(pearsons,h=binwidth,col="grey",xlim=c(-5,5),xlab="Pearson/Standard normal",ylab="Relative density")
            ## add a standard normal #distbins <- seq(-5,5,by=binwidth)
            #dens <- dnorm(x=distbins,mean=0,sd=1,log=FALSE) #lines(distbins,dens,col="black",lwd=1.5)
            #qq.plot(pearsons,ylab="Pearson residual",xlab="Standard normal",col=c("black")) #par(mfrow=c(1,1))
          }  # end market
        } # k loop
      } # if lengths
    } # fleet loop for lengths
    if(verbose) print("Finished plot 18: length comps with fits",quote=F)
    flush.console()
  } # end if 18 in plot or print

  # Plot 19: traditional age comps
  if(19 %in% c(plot, print))
  {
    for(i in fleets)
    {
      if(length(agedbase$Obs[agedbase$Fleet==i])>0)
      {
        plotlens <- agedbase[agedbase$Fleet==i & agedbase$Lbin_hi!=agedbase$Lbin_lo,] # trap out conditional data
        testor    <- length(plotlens$Gender[plotlens$Gender==1 & plotlens$Pick_gender==0])>0
        testor[2] <- length(plotlens$Gender[plotlens$Gender==1 & plotlens$Pick_gender %in% c(1,3)])>0
        testor[3] <- length(plotlens$Gender[plotlens$Gender==2])>0
        for(k in (1:3)[testor])
        {
          if(k==1){afit <- plotlens[plotlens$Gender==1 & as.numeric(plotlens$Pick_gender)==0,]} # combined sex
          if(k==2){afit <- plotlens[plotlens$Gender==1 & as.numeric(plotlens$Pick_gender) %in% c(1,3),]} # females
          if(k==3){afit <- plotlens[plotlens$Gender==2,]} # males
          for(j in unique(afit$Part))
          {
            afit2 <- afit[afit$Part==j,]
            if(nseasons > 1){afit2$Yr <- afit2$Yr + (afit2$Seas - 1)/nseasons + 0.5/nseasons}
            afit2$plotyear <- as.factor(afit2$Yr)
            afit2$plotbins <- afit2$Bin
            afit2 <- afit2[!is.na(afit2$plotbins),]
            afit2$plotobs <- afit2$Obs
            afit2$plotexp <- afit2$Exp

            # may optionally turn of sample size plots in #18
            if(samplesizeplots){

              if(k==1){
                plottitle <- paste("Sample size for sexes combined discard ages for ", FleetNames[i],sep="")
                if(j==2){plottitle <- paste("Sample size for sexes combined retained ages for ", FleetNames[i],sep="")}
                if(j==0){plottitle <- paste("Sample size for sexes combined whole catch ages for ", FleetNames[i],sep="")}}
              if(k==2){
                plottitle <- paste("Sample size for female discard ages for ", FleetNames[i],sep="")
                if(j==2){plottitle <- paste("Sample size for female retained ages for ", FleetNames[i],sep="")}
                if(j==0){plottitle <- paste("Sample size for female whole catch ages for ", FleetNames[i],sep="")}}
              if(k==3){
                plottitle <- paste("Sample size for male discard ages for ", FleetNames[i],sep="")
                if(j==2){plottitle <- paste("Sample size for male retained ages for ", FleetNames[i],sep="")}
                if(j==0){plottitle <- paste("Sample size for male whole catch ages for ", FleetNames[i],sep="")}}
              ymax <- max(afit2$effN)
              xmax <- max(afit2$N)
              afitssfunc <- function()
              { plot(afit2$N,afit2$effN,xlab="Observed sample size",main=plottitle,ylim=c(0,ymax),xlim=c(0,xmax),col="blue",pch=19,ylab="Effective sample size")
                abline(h=0,col="grey")
                lines(x=c(0,ymax),y=c(0,ymax),col="black")
                npoints <- length(as.numeric(afit2$N))
                if(npoints>6 & smooth & length(unique(afit2$N))>6)
                { psmooth <- loess(afit2$effN~afit2$N,degree=1)
                  lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
              }
              if(19 %in% plot){afitssfunc()}
              if(19 %in% print)
              { sex <- 1
                if(k==3){sex <- 2}
                png(file=paste(plotdir,"18agedatfitsampsize_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                afitssfunc()
                dev.off()}
            } # end if samplesizeplots
            if(k==1){
              plottitle <- paste("Combined sex discard age fits for ", FleetNames[i],sep="")
              if(j==2){plottitle <- paste("Combined sex retained age fits for ", FleetNames[i],sep="")}
              if(j==0){plottitle <- paste("Combined sex whole catch age fits for ", FleetNames[i],sep="")}}
            if(k==2){
              plottitle <- paste("Female discard age fits for ", FleetNames[i],sep="")
              if(j==2){plottitle <- paste("Female retained age fits for ", FleetNames[i],sep="")}
              if(j==0){plottitle <- paste("Female whole catch age fits for ", FleetNames[i],sep="")}}
            if(k==3){
              plottitle <- paste("Male discard age fits for ", FleetNames[i],sep="")
              if(j==2){plottitle <- paste("Male retained age fits for ", FleetNames[i],sep="")}
              if(j==0){plottitle <- paste("Male whole catch age fits for ", FleetNames[i],sep="")}}
            # trellis.device(theme=col.whitebg(),new = FALSE)
            trellfems <- afit2
            ntrell <- length(trellfems[,1])
            trellfems$obsexp <- "obs"
            trellfems$trellval <- trellfems$plotobs
            trellfems <- rbind(trellfems,trellfems)
            trellfems$obsexp[1:ntrell] <- "exp"
            trellfems$trellval[1:ntrell] <- trellfems$plotexp[1:ntrell]
            trellis1 <- xyplot(trellval~plotbins|plotyear,as.table=T,groups=obsexp,type=c("l","p"),pch=c(NA,1),lty=c(1,0),lwd=1.5,
                               strip=strip.custom(bg="grey"),ylab="Proportion",xlab="Age bin (yr)",col=c("red","black","red","black"),
                               cex=0.6,main=plottitle,scales=list(relation="same",alternating="1",tck=c(1,0)),data=trellfems)
            if(19 %in% plot){print(trellis1)}
            if(19 %in% print)
            { sex <- 1
              if(k==3){sex <- 2}
              png(file=paste(plotdir,"19_agedatfit_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              print(trellis1)
              dev.off()}

            # may optionally turn of residual plots in #18
            if(compresidplots){
                resx <- afit2$Yr
                resy <- afit2$plotbins
                resz <- afit2$Pearson
                if(k==1){
                  plottitle <- paste("Combined sex discard Pearson residuals for age comps from ", FleetNames[i],sep="")
                  if(j==2){plottitle <- paste("Combined sex retained Pearson residuals for age comps from ", FleetNames[i],sep="")}
                  if(j==0){plottitle <- paste("Combined sex whole catch Pearson residuals for age comps from ", FleetNames[i],sep="")}}
                if(k==2){
                  plottitle <- paste("Female discard Pearson residuals for age comps from ", FleetNames[i],sep="")
                  if(j==2){plottitle <- paste("Female retained Pearson residuals for age comps from ", FleetNames[i],sep="")}
                  if(j==0){plottitle <- paste("Female whole catch Pearson residuals for age comps from ", FleetNames[i],sep="")}}
                if(k==3){
                  plottitle <- paste("Male discard Pearson residuals for age comps from ", FleetNames[i],sep="")
                  if(j==2){plottitle <- paste("Male retained Pearson residuals for age comps from ", FleetNames[i],sep="")}
                  if(j==0){plottitle <- paste("Male whole catch Pearson residuals for age comps from ", FleetNames[i],sep="")}}
                if(length(unique(resx))<minnbubble)
                { resx <- c(resx,max(resx)+1,min(resx)-1)
                  resy <- c(resy,min(resy),min(resy))
                  resz <- c(resz,0,0)}
                plotbub <- cbind(resx,resy,resz)
                pch <- resz
                pch[pch==0] <- NA
                pch[pch>0] <- 16
                pch[pch<0] <- 1
                plottitle <- paste(plottitle," (max=",round(abs(max(resz)),digits=2),")",sep="")
                bub <- bubble2(plotbub,xlab="Year",ylab="Age bin (yr)",col=c("blue","blue"),main=plottitle,maxsize=pntscalar,
                               key.entries=c(0.0),pch=pch,scales=list(relation="same",alternating="1",tck=c(1,0)))
                if(18 %in% plot){print(bub)}
                if(18 %in% print)
                { sex <- 1
                  if(k==3){sex <- 2}
                  png(file=paste(plotdir,"19_agedatfitresids_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                  print(bub)
                  dev.off()}
            } # end if compresidplots
          } # market
        } # k loop
      } # if lengths exist
    } # fleet loop for lengths
    if(verbose) print("Finished traditional age comps",quote=F)

    ## Effective sample sizes for conditional age data
  if(samplesizeplots){
    for(i in fleets)
    {
      if(length(agedbase$Obs[agedbase$Fleet==i])>0)
      {
        agedbasefleet <- agedbase[agedbase$Fleet==i,]
        agedbasefleet <- agedbasefleet[(agedbasefleet$Lbin_hi==agedbasefleet$Lbin_lo),]
        testor <- length(agedbasefleet$Obs[agedbasefleet$Gender==1])>0
        testor[2] <- length(agedbasefleet$Obs[agedbasefleet$Gender==2])>0
        for(m in (1:2)[testor])
        {
          femsamps <- agedbasefleet[agedbasefleet$Gender==m,]
          femsamps$emptytrap <- femsamps$Bin*2
          femsamps$emptytrap[is.na(femsamps$emptytrap)] <- -1
          femsamps <- femsamps[femsamps$emptytrap > 0,]
          femsamps <- femsamps[!(femsamps$Obs<0.0001),] # trap nonrobust effective n's
          femsamps <- femsamps[!(femsamps$Exp>0.99),]
          femsamps$effN[femsamps$effN > maxneff] <- -1
          plottitle <- paste("N-EffN comparison for ",c("fe","")[m],"male age-at-length obs,  ", FleetNames[i],sep="")
          ymax <- max(femsamps$effN)
          xmax <- max(femsamps$N)
          alenssfunc <- function(){
            plot(femsamps$N,femsamps$effN,xlab="Observed sample size",main=plottitle,ylim=c(0,ymax),xlim=c(0,xmax),col="blue",pch=19,ylab="Effective sample size")
            abline(h=0,col="grey")
            lines(x=c(0,ymax),y=c(0,ymax),col="black")
            npoints <- length(as.numeric(femsamps$N))
            if(npoints > 6 & smooth & length(unique(femsamps$N))>6){
              psmooth <- loess(femsamps$effN~femsamps$N,degree=1)
              lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}}
          if(20 %in% plot) alenssfunc()
          if(20 %in% print){
            png(file=paste(plotdir,"20_ageatlensamplesize_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            alenssfunc()
            dev.off()}
        } # m
      } # if ages
    } # fleet loop
   } # end if samplesizeplots

    # aalyear and make year-specific key if positive
    if(aalyear[1] > 0)
    {
      for(j in 1:length(aalyear))
      {
        if(length(agedbase$Obs[agedbase$Yr==aalyear[j]])>0)
        {
          agedbase1 <- agedbase[agedbase$Yr==aalyear[j],]
          for(i in fleets)
          {
            if(length(agedbase1$Obs[agedbase1$Fleet==i])>0)
            {
              agedbasefleet <- agedbase1[agedbase1$Fleet==i & agedbase1$Lbin_hi==agedbase1$Lbin_lo,]
              testor <- length(agedbasefleet$Obs[agedbasefleet$Gender==1])>0
              testor[2] <- length(agedbasefleet$Obs[agedbasefleet$Gender==2])>0
              for(m in (1:2)[testor])
              {
                aydat <- agedbasefleet[agedbasefleet$Gender==m,]
                aydat$emptytrap <- aydat$Bin*2
                aydat$emptytrap[is.na(aydat$emptytrap)] <- -1
                aydat <- aydat[aydat$emptytrap > 0,]
                ntrell <- nrow(aydat)
                aydat$obsexp <- "obs"
                aydat$trellval <- aydat$Obs
                aydat <- rbind(aydat,aydat)
                aydat$obsexp[1:ntrell] <- "exp"
                aydat$trellval[1:ntrell] <- aydat$Exp[1:ntrell]
                aydat$plotbins <- aydat$Bin
                aydat$lenbin <- aydat$Lbin_hi
                aydat <- aydat[order(aydat$lenbin),]
                #if(Lbin_method==1)
		aydat$lenbin2 <- lbins[aydat$lenbin]
                #if(Lbin_method==3)
		aydat$lenbin2 <- aydat$lenbin
                aydat$lenbin <- as.factor(aydat$lenbin2)
                aydat$group <- aydat$obsexp
                aydat$group[aydat$obsexp=="exp"] <- aydat$Yr[aydat$obsexp=="exp"]
                ngrps <- length(unique(aydat$group))-1
                typ <- c(rep("l",ngrps),"p")
                pch <- c(rep(20,ngrps),19)
                lty <- c(rep(1,ngrps),0)
                col <- c(rep("red",ngrps),"black")
                plottitle <- paste(as.character(aalyear[j])," Age at length bin for ",c("fe","")[m],"males, ", FleetNames[i],sep="")
                trellis1 <- xyplot(trellval~plotbins| lenbin,as.table=T,groups=group,type=typ,pch=pch,lty=lty,lwd=1.5,
                                   strip=strip.custom(bg="grey"),ylab="Proportion",xlab="Age (yrs)",col=col,cex=0.6,main=plottitle,
                                   scales=list(relation="same",alternating="1",tck=c(1,0)),data=aydat)
                if(20 %in% plot){print(trellis1)}
                if(20 %in% print)
                { png(file=paste(plotdir,"20_ageatlenyearfit_flt",i,"sex",m,"year",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                  print(trellis1)
                  dev.off()}
                resx <- aydat$plotbins
                resy <- aydat$lenbin2
                resz <- aydat$Pearson
                plottitle <- paste(as.character(aalyear[j])," Pearson residuals for female A-L key, fleet",i,sep=" ")
                plotbub <- cbind(resx,resy,resz)
                pch <- resz
                pch[pch>0] <- 16
                pch[pch<0] <- 1
                pch[pch==0] <- NA
                plottitle <- paste(plottitle," (max=",round(abs(max(resz)),digits=2),")",sep="")
                bub <- bubble2(plotbub,xlab="Age",ylab="Length bin (cm)",col=c("blue","blue"),main=plottitle,maxsize=pntscalar,
                               key.entries=c(0.0),pch=pch,scales=list(relation="same",alternating="1",tck=c(1,0)))
                if(20 %in% plot){print(bub)}
                if(20 %in% print)
                { png(file=paste(plotdir,"20_ageatlenyearresids_flt",i,"sex",m,"year",j,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                  print(bub)
                  dev.off()}
              } # m
            } # fleet statement
          } # fleet loop
        } # test of year positive loop
      } # aalyear loop
    } # aalyear statement
    if(verbose) print("Finished age at length residuals",quote=F)

    print('before aalbin')
    print(aalbin)
    ## aalbin and make bin-specific key over all years if positive ###
    if(aalbin > 0)
    {
        print(aalbin)
      if(length(agedbase$Obs[agedbase$Lbin_hi %in% aalbin])>0)
      {
        agedbase2 <- agedbase[agedbase$Lbin_hi %in% aalbin,]
        print(head(agedbase2))
        for(i in fleets)
        {
          if(length(agedbase2$Obs[agedbase2$Fleet==i])>0)
          {
            print(" in aalbin plot")
            agedbasefleet <- agedbase2[agedbase2$Fleet==i,]
            agedbasefleet <- agedbasefleet[(agedbasefleet$Lbin_hi==agedbasefleet$Lbin_lo),]
            testor <- length(agedbasefleet$Obs[agedbasefleet$Gender==1])>0
            testor[2] <- length(agedbasefleet$Obs[agedbasefleet$Gender==2])>0
            for(m in (1:2)[testor])
            {
              abin <- agedbasefleet[agedbasefleet$Gender==m,]
              abin$emptytrap <- abin$Bin*2
              abin$emptytrap[is.na(abin$emptytrap)] <- -1
              abin <- abin[abin$emptytrap > 0,]
              ntrell <- nrow(abin)
              abin$obsexp <- "obs"
              abin$trellval <- abin$Obs
              abin <- rbind(abin,abin)
              abin$obsexp[1:ntrell] <- "exp"
              abin$trellval[1:ntrell] <- abin$Exp[1:ntrell]
              abin$plotbins <- abin$Bin
              abin$Yr <- abin$Yr
              abin <- abin[order(abin$Yr),]
              abin$Yr <- as.factor(abin$Yr)
              abin$group <- abin$obsexp
              abin$group[abin$obsexp=="exp"] <- abin$Yr[abin$obsexp=="exp"]
              ngrps <- length(unique(abin$group))-1
              typ <- c(rep("l",ngrps),"p")
              pch <- c(rep(NA,ngrps),1)
              lty <- c(rep(1,ngrps),0)
              col <- c(rep("red",ngrps),"black")
              plottitle <- paste("Age at length ",lbins[aalbin]," cm, for ",c("fe","")[m],"males, ", FleetNames[i],sep="")
              trellis1 <- xyplot(trellval~plotbins|Yr,as.table=T,groups=group,type=typ,pch=pch,lty=lty,lwd=1.5,
                                 strip=strip.custom(bg="grey"),ylab="Proportion",xlab="Age (yrs)",col=col,cex=0.6,
                                 main=plottitle,scales=list(relation="same",alternating="1",tck=c(1,0)),data=abin)
              if(20 %in% plot){print(trellis1)}
              if(20 %in% print)
              { png(file=paste(plotdir,"20ageatlenbinfit_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
                print(trellis1)
                dev.off()}
            } # m
          } # if fleet
        } # fleets
      } # test of year
    } # aalbin
    if(verbose) print("Finished age at length fits",quote=F)

    ## Multipanel plots of conditional aal residuals
    if(aalresids)
    {
      for(i in fleets)
      {
        if(length(agedbase$Obs[agedbase$Fleet==i])>0)
        {
          agedbasefleet <- agedbase[agedbase$Fleet==i,]
          agedbasefleet <- agedbasefleet[(agedbasefleet$Lbin_hi==agedbasefleet$Lbin_lo),]
          testor <- length(agedbasefleet$Obs[agedbasefleet$Gender==1])>0
          testor[2] <- length(agedbasefleet$Obs[agedbasefleet$Gender==2])>0
          for(m in (1:2)[testor])
          {
            ares <- agedbasefleet[agedbasefleet$Gender==m,]
            ares$emptytrap <- ares$Bin*2
            ares$emptytrap[is.na(ares$emptytrap)] <- -1
            ares <- ares[ares$emptytrap > 0,]
            ntrell <- length(ares[,1])
            ares$obsexp <- "obs"
            ares$trellval <- ares$Obs
            ares <- rbind(ares,ares)
            ares$obsexp[1:ntrell] <- "exp"
            ares$trellval[1:ntrell] <- ares$Exp[1:ntrell]
            ares$plotbins <- ares$Bin
            ares$lenbin <- ares$Lbin_hi
            ares <- ares[order(ares$lenbin),]
            ares$lenbin2 <- 0
            #if(Lbin_method==1)
		ares$lenbin2 <- lbins[ares$lenbin]
            #if(Lbin_method==3)
		ares$lenbin2 <- ares$lenbin
            ares$lenbin <- as.factor(ares$lenbin2)
            ares$group <- ares$obsexp
            ares$group[ares$obsexp=="exp"] <- ares$Yr[ares$obsexp=="exp"]
            years <- unique(ares$Yr)
            years <- years[order(years)]
            nyears <- length(years)
            if(20 %in% plot)
            {
              plotspot <- 2
              plotspot2 <- 1
              plot.new()
              text(0.5,0.5,paste("Conditional age residuals for ",c("fe","")[m],"males from ", FleetNames[i],sep=""),cex=1)
              for(fy in 1:nyears)
              {
                more <- T
                if(fy==nyears | fy %in% c(8,16,24,32,40,48,56)){more<-F}
                aresuse <- ares[ares$Yr==years[fy],]
                resx <- aresuse$plotbins
                resy <- aresuse$lenbin2
                resz <- aresuse$Pearson
                plottitle <- paste(years[fy],sep="")
                plotbub <- cbind(resx,resy,resz)
                pch <- resz
                pch[pch==0] <- NA
                pch[pch>0] <- 16
                pch[pch<0] <- 1
                if(plotspot==2){ylab="Length bin (cm)"}
                if(plotspot%in%c(3,4,5)){ylab=""}
                plottitle <- paste("        ",plottitle," (max=",round(max(abs(resz)),digits=2),")",sep="")
                main <- list()
                main$label <- plottitle
                main$cex <- 0.8
                bub <- bubble2(plotbub,xlab="Age",ylab=ylab,col=c("blue","blue"),main=main,maxsize=pntscalar,
                               ylim=c(min(lbins)-3,max(lbins)+3),key.entries=c(0.0),pch=pch,scales=list(relation="same",alternating="1",tck=c(1,0)))
                print(bub,split=c(plotspot,plotspot2,6,2),more=more,panel.width=list(0.7,"npc"))
                plotspot <- plotspot +1
                if(plotspot==6){plotspot2 <- plotspot2 +1}
                if(plotspot2==3){plotspot2 <- 1}
                if(plotspot==6){plotspot <- 2}
              }
            } # plot
            if(20 %in% print)
            {
              page <- 1
              plotspot <- 2
              plotspot2 <- 1
              png(file=paste(plotdir,"20ageatlenresids_flt",i,"sex",m,"page",page,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
              for(fy in 1:nyears)
              {
                more <- T
                if(fy==nyears | fy %in% c(8,16,24,32,40,48,56)){more<-F}
                aresuse <- ares[ares$Yr==years[fy],]
                resx <- aresuse$plotbins
                resy <- aresuse$lenbin2
                resz <- aresuse$Pearson
                plottitle <- paste(years[fy],sep="")
                plotbub <- cbind(resx,resy,resz)
                pch <- resz
                pch[pch==0] <- NA
                pch[pch>0] <- 16
                pch[pch<0] <- 1
                if(plotspot %in% 2){ylab="Length bin (cm)"}
                if(plotspot %in% c(3,4,5)){ylab=""}
                plottitle <- paste("        ",plottitle," (max=",round(max(abs(resz)),digits=2),")",sep="")
                main <- list()
                main$label <- plottitle
                main$cex <- 0.8
                bub <- bubble2(plotbub,xlab="Age",ylab=ylab,col=c("blue","blue"),main=main,maxsize=pntscalar,
                               ylim=c(min(lbins)-3,max(lbins)+3),key.entries=c(0.0),pch=pch,scales=list(relation="same",alternating="1",tck=c(1,0)))
                print(bub,split=c(plotspot,plotspot2,6,2),more=more,panel.width=list(0.7,"npc"))
                plotspot <- plotspot +1
                if(plotspot==6){plotspot2 <- plotspot2 +1}
                if(plotspot==6){plotspot <- 2}
                if((plotspot2==3) | (fy==nyears))
                {
                  dev.off()
                  plotspot2 <- 1
                  page <- page + 1
                  if(!(fy==nyears)){png(file=paste(plotdir,"20ageatlenresids_flt",i,"sex",m,"page",page,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)}
                }
              } # nyears
            } # print
          } # m
        } # if fleet
      } # fleets
    } # aalresids
    if(verbose) print("Finished plot 20: conditional age at length with fits",quote=F)
    flush.console()
  } # end if 20 in plot or print

} # end if not using newcompplots switch

if(newcompplots) # switch to allow transition to new non-trellis composition plots
{

  # Composition data plots 15-20
  lendbase   <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
  agedbase   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0,]
  latagebase <- compdbase[compdbase$Kind=="L@A" & compdbase$N > 0,]
  lendbase$effN <- as.numeric(lendbase$effN)
  agedbase$effN <- as.numeric(agedbase$effN)

  if(!datplot)
  {
    print("skipped data-only plots 15-17 because input 'datplot=F'",quote=F)
  }else{
    if(15 %in% c(plot,print))  # data only aspects
    {
        print(plotdir)
      # length comp bar plot
      SSv3_plot_comps(replist=replist,datonly=T,kind="LEN",bub=F,verbose=verbose,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,
                      maxrows=maxrows,maxcols=maxcols,fixrows=fixrows,fixcols=fixcols,
                      png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      # length comp bubble plot
      SSv3_plot_comps(replist=replist,datonly=T,kind="LEN",bub=T,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,
                      maxrows=maxrows,maxcols=maxcols,fixrows=fixrows,fixcols=fixcols,
                      png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      if(verbose) print("Finished plot 15: length comp data",quote=F)
      flush.console()
    }
    if(16 %in% c(plot,print))
    {
      # age comp bar plot
      SSv3_plot_comps(replist=replist,datonly=T,kind="AGE",bub=F,verbose=verbose,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,
                      maxrows=maxrows,maxcols=maxcols,fixrows=fixrows,fixcols=fixcols,
                      png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      # age comp bubble plot
      SSv3_plot_comps(replist=replist,datonly=T,kind="AGE",bub=T,verbose=verbose,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,
                      maxrows=maxrows,maxcols=maxcols,fixrows=fixrows,fixcols=fixcols,
                      png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      if(verbose) print("Finished plot 16: age comp data",quote=F)
      flush.console()
    }
    if(17 %in% c(plot,print))
    {
      # conditional age plot
      SSv3_plot_comps(replist=replist,datonly=T,kind="cond",bub=T,verbose=verbose,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,
                      maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,
                      fixrows=fixrows,fixcols=fixcols,
                      png=(17%in%print),GUI=(17%in%plot),plotdir=plotdir,cex.main=cex.main,...)
      if(verbose) print("Finished plot 17: conditional age at length data",quote=F)
      flush.console()
    }
  } # datplot

  # plot of length comp data with fits, sample size, etc.
  if(18 %in% c(plot,print)){
    SSv3_plot_comps(replist=replist,datonly=F,kind="LEN",bub=T,verbose=verbose,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,
                    maxrows=maxrows,maxcols=maxcols,fixrows=fixrows,fixcols=fixcols,
                    png=(18%in%print),GUI=(18%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,...)
    if(verbose) print("Finished plot 18: length comps with fits",quote=F)
    flush.console()
  }

  # plot of age comp data with fits, sample size, etc.
  if(19 %in% c(plot,print)){
    SSv3_plot_comps(replist=replist,datonly=F,kind="AGE",bub=T,verbose=verbose,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,
                    maxrows=maxrows,maxcols=maxcols,fixrows=fixrows,fixcols=fixcols,
                    png=(19%in%print),GUI=(19%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,...)
    if(verbose) print("Finished plot 19: age comps with fits",quote=F)
    flush.console()
  } # end if 19 in plot or print

  if(20 %in% c(plot,print)){
    SSv3_plot_comps(replist=replist,datonly=F,kind="cond",bub=T,verbose=verbose,
                    aalbin=aalbin,aalyear=aalyear,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,
                    maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,
                    fixrows=fixrows,fixcols=fixcols,
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

} # end if using newcompplots switch

  # Plot 21: length at age data
  if(21 %in% c(plot, print))
  {
    for(i in fleets)
    {
      if(length(latagebase$Obs[latagebase$Fleet==i])>0)
      {
        plotlens <- latagebase[latagebase$Fleet==i,]
        plotlens <- plotlens[!plotlens$Obs %in% c(NA),]
        testor <- length(plotlens$Obs[plotlens$Gender==1])>0
        testor[2] <- length(plotlens$Obs[plotlens$Gender==2])
        for(m in (1:2)[testor])
        {
          la <- plotlens[plotlens$Gender==m,] # females or males
          if(nseasons > 1){la$Yr <- la$Yr + (la$Seas - 1)/nseasons + 0.5/nseasons}
          la$plotyear <- as.factor(la$Yr)
          la$plotbins <- la$Bin
          la <- la[!is.na(la$plotbins), ]
          la$plotobs <- la$Obs
          la$plotexp <- la$Exp

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
          # trellis.device(theme=col.whitebg(),new = FALSE)
          la2 <- la
          ntrell <- length(la2[,1])
          la2$obsexp <- "obs"
          la2$trellval <- la2$plotobs
          la2 <- rbind(la2,la2)
          la2$obsexp[1:ntrell] <- "exp"
          la2$trellval[1:ntrell] <- la2$plotexp[1:ntrell]
          trellis1 <- xyplot(trellval~plotbins|plotyear,as.table=T,groups=obsexp,type=c("l","p"),pch=c(NA,1),lty=c(1,0),lwd=1.5,
                             strip=strip.custom(bg="grey"),ylab="Length (cm)",xlab="Age (yr)",col=c("red","black","red","black"),
                             cex=0.6,main=plottitle,scales=list(relation="same",alternating="1",tck=c(1,0)),data=la2)
          if(21 %in% plot) print(trellis1)
          if(21 %in% print){
            png(file=paste(plotdir,"21_lenatagefit_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            print(trellis1)
            dev.off()}
          resx <- la$Yr
          resy <- la$plotbins
          resz <- la$Pearson
          plottitle <- paste("Pearson residuals for sexes combined, ", FleetNames[i],sep="")
          if(nsexes > 1){plottitle <- paste("Pearson residuals for ",c("fe","")[m],"males, ", FleetNames[i],sep="")}
          if(length(unique(resx))<minnbubble){
            resx <- c(resx,(max(resx)+1),(min(resx)-1))
            resy <- c(resy,(min(resy)),(min(resy)))
            resz <- c(resz,0,0)}
          plotbub <- cbind(resx,resy,resz)
          pch <- resz
          pch[pch==0] <- NA
          pch[pch>0] <- 16
          pch[pch<0] <- 1
          plottitle <- paste(plottitle," (max=",round(abs(max(resz)),digits=2),")",sep="")
          bub <- bubble2(plotbub,xlab="Year",ylab="Age (yr)",col=c("blue","blue"),main=plottitle,maxsize=pntscalar,
                         key.entries=c(0.0),pch=pch,scales=list(relation="same",alternating="1",tck=c(1,0)))
          if(21 %in% plot) print(bub)
          if(21 %in% print){
            png(file=paste(plotdir,"21_lenatageresids_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
            print(bub)
            dev.off()}
        } # m
      } # if lengths
    } # fleets
    if(verbose) print("Finished plot 21: length at age data",quote=F)
    flush.console()
  } # end if 21 in plot or print

  if(22 %in% c(plot, print))
  {
   if(!is.null(equil_yield[1,1])){
   yieldfunc <- function(){
   plot(equil_yield$Depletion,equil_yield$Catch,xlab="Relative depletion",ylab="Equilibrium yield (mt)",
        type="l",lwd=2,col="blue")}
   if(22 %in% plot){yieldfunc()}
   if(22 %in% print){
    png(file=paste(plotdir,"22_yield.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
    yieldfunc()
    dev.off()}
    if(verbose) print("Finished plot 22: yield curve",quote=F)
    }
  }

  if(verbose) print("Finished all requested plots",quote=F)
  ### end of function
}
