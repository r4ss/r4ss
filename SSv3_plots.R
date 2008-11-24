SSv3_plots <- function(
    # plotting related inputs
    readrep=T, replist=NA, plot=1:19, print=0, printfolder="", fleets=NA, areas=NA, fleetcols=NA, 
    areacols=NA, verbose=T, datplot=F, Natageplot=T, minbthresh=0.25, pntscalar=2.6, 
    minnbubble=8, aalyear=-1, aalbin=-1, aalresids=F, maxneff=5000, smooth=T, samplesizeON=T, 
    bubbleON=T, pwidth=700, pheight=700, OS="Windows", 
    
    # the following inputs are a repeat of those in the SSv3_output function which will
    # be passed to that function. This could also be achieved using the "..." input.
    # if readrep=F these inputs will be overridden by the values that were used to create the replist

    dir="C:\\myfiles\\mymodels\\myrun\\", model="SS3_opt", repfile="Report.SSO", 
    ncols=200, forecast=F, warn=T, covar=F, cormax=0.95, readtargets=T,
    printstats=F, return="Yes"){    
################################################################################
#
# SSv3_plots BETA November 24, 2008.
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: To sumarize the results of an SSv3 model run.
# Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
#          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
# Returns: Plots with plot history in R GUI and/or .png files.
# General: Updated for Stock Synthesis version 3.01L September, 2008; R version 2.7.2.
# Notes:   See users guide for documentation.
# Required SS3v_output function and packages: lattice
# Credit:  Based loosely on an early version of "Scape" (A. Magnusson) and "Output viewer" (R. Methot)
#
################################################################################

# if you have never installed the required package,
# copy and paste the following install.packages command (without the "#").
# Note: this may require administrative priviledges on your computer
#
# install.packages(lattice)

  # load required package
  require(lattice)

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
  
  if(readrep | is.na(replist[[1]])){
    # if replist is not in workspace or readrep==TRUE then read report file
    if(!exists("SSv3_output")){
      print("! Warning: the function 'SSv3_output' is not in R workspace.")
      print("  See documentation at http://code.google.com/p/r4ss/wiki/Documentation") 
      print("  for instructions for sourcing the file SSv3_output.R.")
    }
    replist <- SSv3_output(
      dir=dir, model=model, repfile=repfile, ncols=ncols, forecast=forecast, warn=warn, 
      covar=covar, cormax=cormax, readtargets=readtargets, verbose=verbose,
      printstats=printstats, return="Yes")
  }else{
    # otherwise get important inputs that were used in the call to SSv3_output that produced the supplied replist
    dir      <- replist$inputs$dir      <- dir 
    model    <- replist$inputs$model    <- model 
    repfile  <- replist$inputs$repfile  <- repfile
    forecast <- replist$inputs$forecast <- forecast
    warn     <- replist$inputs$warn     <- warn
    covar    <- replist$inputs$covar    <- covar 
    verbose  <- replist$inputs$verbose  <- verbose
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
  ALK                            <- replist$ALK
  compdbase                      <- replist$composition_database
  derived_quants                 <- replist$derived_quants
  parameters                     <- replist$parameters
  FleetNames                     <- replist$FleetNames
  CoVar                          <- replist$CoVar
  stdtable                       <- replist$stdtable
  rawstd                         <- replist$rawstd
  SS_version                     <- replist$SS_version
  Run_time                       <- replist$Run_time
  Files_used                     <- replist$Files_used
  used_likelihoods               <- replist$used_likelihoods
  raw_likelihoods_by_fleet       <- replist$raw_likelihoods_by_fleet
  variance_adjustments_by_fleet  <- replist$variance_adjustments_by_fleet
  estimated_parameters           <- replist$estimated_parameters
  log_det_hessian                <- replist$log_det_hessian
  maximum_gradient_component     <- replist$maximum_gradient_component
  sigma_R_in                     <- replist$sigma_R_in
  sigma_R_out                    <- replist$sigma_R_out
  Bzero                          <- replist$Bzero
  depletion_final_year           <- replist$depletion_final_year
  fmax                           <- replist$fmax
  endyrcatch                     <- replist$endyrcatch
  endyrlandings                  <- replist$endyrlandings
  endyrspr                       <- replist$endyrspr
  endyrspr_to_proxy              <- replist$endyrspr_to_proxy
  # sprtarg and btarg are read from Forecast.SS_New alternatively they could be  
  # read from Forecast-report.SSO or taken as inputs to this function
  sprtarg                        <- replist$sprtarg
  btarg                          <- replist$btarg
  
  # derived quantities
  mainmorphs <- morph_indexing$Index[morph_indexing$Bseas==1]
  FleetNumNames <- paste(1:nfleets,FleetNames,sep="_")
  if(is.na(fleets)) fleets <- 1:nfleets
  if(is.na(areas)) areas <- 1:nareas
    
  # time series quantities used for multiple plots
  timeseries$Yr <- timeseries$Yr + (timeseries$Seas-1)/nseasons
  ts <- timeseries[timeseries$Yr <= endyr+1,]
  tsyears <- ts$Yr[ts$Seas==1]
  tsarea <- ts$Area[ts$Seas==1]
  tsspaw_bio <- ts$SpawnBio[ts$Seas==1]
  if(nsexes==1) tsspaw_bio <- tsspaw_bio/2
  dep <- tsspaw_bio/tsspaw_bio[1]

  if(verbose) print("Finished defining objects",quote=F)
  if(nareas>1){
    print(paste("! Warning: some plots are not configured for mult-area models (nareas=",nareas,")",sep=""),quote=F)
    areanames <- paste("Area",1:nareas)
  }

  #### prepare for plotting
  if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
  # make plot window (operating system specific)
  nplots <- length(intersect(1:19,plot))
  nprints <- length(intersect(1:19,print))
  if(nplots>0){
    if(OS=="Windows") windows(record=TRUE)
    if(OS=="Linux") X11()
    if(OS=="Mac") quartz()
  }
  if(printfolder!="" & nprints>0){
    plotdir <- paste(dir,printfolder,"\\",sep="")
    if(OS=="Windows") shell(paste("mkdir ",plotdir),translate=T)
    if(OS=="Linux") system(paste("mkdir -p ",plotdir))
    if(OS=="Mac") shell(paste("mkdir ",plotdir)) # don't know if this is correct or not
  }
  if(nprints>0 & verbose) print(paste("Plots specified by 'print' will be written to",plotdir))  

  # colors
  ians_blues <- c("white","grey","lightblue","skyblue","steelblue1","slateblue",topo.colors(6),"blue","blue2","blue3","blue4","black")
  ians_contour <- c("white",rep("blue",100))
  if(is.na(fleetcols)) fleetcols <- rich.colors.short(nfleets)
  if(is.na(areacols )) areacols  <- rich.colors.short(nareas)
  if(nareas==3) areacols <- rainbow(nareas)
  if(nfleets==3) fleetcols <- rainbow(nareas)

  #### plot 1
  # Static growth (mean weight, maturity, spawning output)
  if(1 %in% c(plot, print))
  {
    growdat <- replist$endgrowth
    xlab <- "Length (cm)"
    x <- biology$Mean_Size
    ylab <- "Mean weight (kg) in last year"
    ylab2 <- "Spawning output"

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
    gfunc3 <- function(){
      plot(x,biology$Spawn,xlab="Length (cm)",ylab=ylab2,type="o",col="red")
      abline(h=0,col="grey")}
    if(1 %in% plot){
      gfunc1()
      gfunc2()
      gfunc3()}
    if(1 %in% print){
      png(file=paste(plotdir,"1weightatsize.png",sep=""),width=pwidth,height=pheight)
      gfunc1()
      dev.off()
      png(file=paste(plotdir,"1maturity.png",sep=""),width=pwidth,height=pheight)
      gfunc2()
      dev.off()
      png(file=paste(plotdir,"1spawningoutput.png",sep=""),width=pwidth,height=pheight)
      gfunc3()
      dev.off()}

    # Mid year mean length at age with 95% range of lengths (by sex if applicable)
    growdatF <- growdat[growdat$Morph==mainmorphs[1],]
    growdatF$Sd_Size <- growdatF$SD_Mid
    growdatF$high <- growdatF$Len_Mid + 1.96*growdatF$Sd_Size
    growdatF$low <- growdatF$Len_Mid - 1.96*growdatF$Sd_Size
    maxy <- max(growdatF$high)
    x <- growdatF$Age
    header <- "Ending year expected growth"
    if(nseasons > 1){header <- paste(header," season 1",sep="")}
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
        growdatM <- growdat[growdat$Morph==mainmorphs[2],]
        xm <- growdatM$Age
        growdatM$Sd_Size <- growdatM$SD_Mid
        growdatM$high <- growdatM$Len_Mid + 1.96*growdatM$Sd_Size
        growdatM$low <- growdatM$Len_Mid - 1.96*growdatM$Sd_Size
        lines(xm,growdatM$Len_Mid,col="blue",lwd=2,type="l")
        lines(xm,growdatM$high,col="blue",lwd=1,lty="dashed")
        lines(xm,growdatM$low,col="blue",lwd=1,lty="dashed")
        grid()
        legend("topleft",bty="n", c("Females","Males"), lty=1, col = c("red","blue"))
      }
    }
    if(1 %in% plot) gfunc4()
    if(1 %in% print){
      png(file=paste(plotdir,"1sizeatage.png",sep=""),width=pwidth,height=pheight)
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
        png(file=paste(plotdir,"1natmort.png",sep=""),width=pwidth,height=pheight)
        mfunc()
        dev.off()}
    }
    if(verbose) print("Finished plot 1: Static growth (mean weight, maturity, spawning output)",quote=F)
  } # end if 1 in plot or print

  ### plot 2: Time-varying growth
  if(2 %in% c(plot, print))
  {
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
          png(file=paste(plotdir,"2timevarygrowthsurf",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
          persp(x,y,z,col="white",xlab="Age (yr)",ylab="",zlab="Length (cm)",expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
          dev.off()
          png(file=paste(plotdir,"2timevarygrowthcontour",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
          contour(x,y,z,nlevels=12,xlab="Age (yr)",main=main,col=ians_contour,lwd=2)
        dev.off()}
      }
    }
    if(verbose) print("Finished plot 2: Time-varying growth",quote=F)
  } # end if 2 in plot or print

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
          { png(file=paste(plotdir,"3timevarylenselsurf_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
            persp(x,y,z,col="white",xlab="Length (cm)",ylab="Year",zlab="Selectivity",expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
            dev.off()
            png(file=paste(plotdir,"3timevarylenselcontour_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
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
          { png(file=paste(plotdir,"3timevaryretsurf_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
            persp(x,y,z,col="white",xlab="Length (cm)",ylab="Year",zlab="Retention",expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
            dev.off()
            png(file=paste(plotdir,"3timevaryretcontour_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
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
            png(file=paste(plotdir,"4lenselex_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
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
              png(file=paste(plotdir,"3timevaryageselsurf_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
              persp(x,y,z,col="white",xlab="Age (yr)",ylab="Year",zlab=ylab,expand=0.5,box=T,main=main,ticktype="detailed",phi=35,theta=-10)
              dev.off()
              png(file=paste(plotdir,"3timevaryageselcontour_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
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
             {png(file=paste(plotdir,"4ageselex_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
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
              png(file=paste(plotdir,"4ageselex_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
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
      plot(ts$Yr[tsarea==1],ts$Bio_all[tsarea==1],xlab="Year",ylim=c(0,max(ts$Bio_all)),ylab="Total biomass",type="o",col=areacols[1])
      if(nareas>1){
        for(iarea in 2:nareas) lines(ts$Yr[tsarea==iarea],ts$Bio_all[tsarea==iarea],type="o",col=areacols[iarea])
        legend("topright",legend=areanames,lty=1,pch=1,col=areacols,bty="n")
      }
      abline(h=0,col="grey")
    }
    sbiofunc <- function(){
      plot(ts$Yr[tsarea==1],ts$Bio_smry[tsarea==1],xlab="Year",ylim=c(0,max(ts$Bio_smry)),ylab="Summary biomass",type="o",col=areacols[1])
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
      png(file=paste(plotdir,"5totbio.png",sep=""),width=pwidth,height=pheight)
      tbiofunc()
      dev.off()
      png(file=paste(plotdir,"5summarybio.png",sep=""),width=pwidth,height=pheight)
      sbiofunc()
      dev.off()}

    # total landings (not adapted for multi-area models)
    ls <- nrow(ts)
    totretainedmat <- as.matrix(ts[,substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
    ts$totretained <- 0
    ts$totretained[3:ls] <- rowSums(totretainedmat)[3:ls]
    landfunc <- function(){
      plot(ts$Yr[1:ls],ts$totretained[1:ls],xlab="Year",ylab="Landings (mt)",type="o",col="black")
      abline(h=0,col="grey")
      for(xx in 1:nfishfleets){lines(ts$Yr[3:ls],totretainedmat[3:ls,xx],type="l",col=fleetcols[xx])}}
    if(5 %in% plot) landfunc()
    if(5 %in% print){
      png(file=paste(plotdir,"5landings.png",sep=""),width=pwidth,height=pheight)
      landfunc()
      dev.off()}
    # total catch (not adapted for multi-area models)
    totcatchmat <- as.matrix(ts[,substr(names(ts),1,nchar("enc(B)"))=="enc(B)"])
    ts$totcatch <- 0
    ts$totcatch[3:ls] <- rowSums(totcatchmat)[3:ls]
    if(max(ts$totcatch[3:ls] - ts$totretained[3:ls] != 0))
    {
      catfunc <- function()
      {plot(ts$Yr[1:ls],ts$totcatch[1:ls],xlab="Year",ylab="Total catch (mt)",type="o",col="black")
        abline(h=0,col="grey")
      for(xx in 1:nfishfleets){lines(ts$Yr[2:ls],totcatchmat[2:ls,xx],type="l",col=fleetcols[xx])}}
      if(5 %in% plot) catfunc()
      if(5 %in% print){
        png(file=paste(plotdir,"5totcatch.png",sep=""),width=pwidth,height=pheight)
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
      plot(ts$Yr[3:ls],3:ls,xlab="Year",ylim=c(0,fmax),ylab=ylab,type="n")
        abline(h=0,col="grey")
        for(xx in 1:nfishfleets) lines(ts$Yr[3:ls],Hrates[3:ls,xx],type="o",col=fleetcols[xx])
    }
    if(5 %in% plot) Hratefunc()
    if(5 %in% print){
      png(file=paste(plotdir,"5harvestrates.png",sep=""),width=pwidth,height=pheight)
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
        plot(ts$Yr[1:ls],ts$discardall[1:ls],xlab="Year",ylab=ylab,type="o",col="black")
        abline(h=0,col="grey")
        for(xx in 1:nfishfleets) lines(ts$Yr[3:ls],discardmat[3:ls,xx],col=fleetcols[xx])
      }
      if(5 %in% plot){discfunc()}
      if(5 %in% print)
      {png(file=paste(plotdir,"5discards.png",sep=""),width=pwidth,height=pheight)
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
        {png(file=paste(plotdir,"5discardfraction.png",sep=""),width=pwidth,height=pheight)
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
      plot(x,y,xlab="Year",ylab=ylab,ylim=c(0,max(y)),type="o",col="blue")
      abline(h=0,col="grey")}
    if(6 %in% plot) recfunc()
    if(6 %in% print){
      png(file=paste(plotdir,"6recruits.png",sep=""),width=pwidth,height=pheight)
      recfunc()
      dev.off()}
    if(forecast){
      x <- timeseries$Yr
      y <- timeseries$Recruit_0
      ymax <- max(y)
      tsfore <- timeseries$Era=="FORE"
      recfunc2 <- function(){
        plot(x[!tsfore],y[!tsfore],xlab="Year",ylab=ylab,xlim=range(timeseries$Yr),ylim=c(0,max(y)),type="o",col="blue")
        abline(h=0,col="grey")
        lines(x[tsfore],y[tsfore],lwd=1,col="red",lty="dashed")
        points(x[tsfore],y[tsfore],col="red",pch=20)}
      if(6 %in% plot) recfunc2()
      if(6 %in% print){
        png(file=paste(plotdir,"6recruitswforecast.png",sep=""),width=pwidth,height=pheight)
        recfunc2()
        dev.off()}
    }
    # recruitment with asymptotic interval
    if(covar){
      recstd <- derived_quants[substring(derived_quants$LABEL,1,4)=="Recr",]
      recstd$Yr <- as.numeric(substring(recstd$LABEL,6,nchar(recstd$LABEL[1])-1))
      v <- recstd$Value
      recstd$val1 <- log(v)
      recstd$logint <- sqrt(log(1+(recstd$StdDev/v)^2))
      recstd$lower <- exp(recstd$val1 - 1.96*recstd$logint)
      recstd$upper <- exp(recstd$val1 + 1.96*recstd$logint)
      plottitle <- "~95% Asymptotic confidence interval"
      uiw <- recstd$upper - v
      liw <- v - recstd$lower
      recfunc3 <- function(maxyr){
        plotCI(x=recstd$Yr[recstd$Yr<=maxyr],y=v[recstd$Yr<=maxyr],sfrac=0.001,z=2,
          uiw=uiw[recstd$Yr<=maxyr],liw=liw[recstd$Yr<=maxyr],xlab="Year",ylo=0,
          col="black",ylab=ylab,lty=1,main=plottitle)
        abline(h=0,col="grey")}
      if(6 %in% plot) recfunc3(maxyr=endyr+1)
      if(6 %in% print){
        png(file=paste(plotdir,"6recswintervals.png",sep=""),width=pwidth,height=pheight)
        recfunc3(maxyr=endyr+1)
        dev.off()}
      if(forecast){
        if(6 %in% plot) recfunc3(maxyr=max(timeseries$Yr))
        if(6 %in% print){
          png(file=paste(plotdir,"6recswforecastintervals.png",sep=""),width=pwidth,height=pheight)
          recfunc3(maxyr=max(timeseries$Yr))
          dev.off()}
      }
    } # hessian
    if(verbose) print("Finished plot 6: recruitment",quote=F)
  } # end if 6 in plot or print

  # Plot 7: spawning biomass
  if(7 %in% c(plot, print))
  {
    ylab <- "Spawning biomass (mt)"
    ylim <- c(0,max(tsspaw_bio))
    sbfunc <- function(){
      plot(tsyears[tsarea==1],tsspaw_bio[tsarea==1],xlab="Year",ylab=ylab,ylim=ylim,type="o",col=areacols[1])
      if(nareas>1){
        for(iarea in 2:nareas) lines(tsyears[tsarea==iarea],tsspaw_bio[tsarea==iarea],type="o",col=areacols[iarea])
        legend("topright",legend=areanames,lty=1,pch=1,col=areacols,bty="n")
      }
      abline(h=0,col="grey")}
    if(7 %in% plot) sbfunc()
    if(7 %in% print){
      png(file=paste(plotdir,"7spawnbio.png",sep=""),width=pwidth,height=pheight)
      sbfunc()
      dev.off()}
    if(forecast){
      if(nsexes==1) goodforcast$Spbio <- as.numeric(goodforcast$Spbio)/2
      ymax <- max(as.numeric(tsspaw_bio),as.numeric(goodforcast$Spbio))
      xmin <- min(tsyears)-1
      yr <- as.numeric(goodforcast$Year)
      xmax <- max(yr)+1
      sbfunc2 <- function(){
        plot(tsyears,tsspaw_bio,xlab="Year",ylab=ylab,xlim=c(xmin,xmax),ylim=c(0,ymax),type="o",col="blue")
        abline(h=0,col="grey")
        lines(yr,goodforcast$Spbio,lwd=1,col="red",lty="dashed")
        points(goodforcast$Year[2:(nforecastyears)],goodforcast$Spbio[2:(nforecastyears)],col="red",pch=20)}
      if(7 %in% plot) sbfunc2()
      if(7 %in% print){
        png(file=paste(plotdir,"7spawnbiowforecast.png",sep=""),width=pwidth,height=pheight)
        sbfunc2()
        dev.off()}
    }
    if(covar){
      bioscale <- 1 #scaling factor for single sex models
      if(nsexes==1) bioscale <- 0.5 # should allow flexible input
      # with interval
      sbstd <- derived_quants[substring(derived_quants$LABEL,1,3)=="SPB",]
      sbstd$Yr <- as.numeric(substring(sbstd$LABEL,5,nchar(sbstd$LABEL[1])-1))
      v <- sbstd$Value/bioscale
      sbstd$upper <- v + 1.96*sbstd$StdDev/bioscale
      sbstd$lower <- v - 1.96*sbstd$StdDev/bioscale
      sbstd$lower[sbstd$lower < 0] <- 0
      ymax <- max(as.numeric(sbstd$upper))
      plottitle <- "~95% Asymptotic confidence interval"
      sbfunc3 <- function(){
        plot(sbstd$Yr,v,main= plottitle,xlab="Year",ylab=ylab,ylim=c(0,ymax),type="o",col="blue")
        abline(h=0,col="grey")
        lines(sbstd$Yr,sbstd$upper,lwd=1,col="blue",lty="dashed")
        lines(sbstd$Yr,sbstd$lower,lwd=1,col="blue",lty="dashed")}
      if(7 %in% plot) sbfunc3()
      if(7 %in% print){
        png(file=paste(plotdir,"7spawnbiointerval.png",sep=""),width=pwidth,height=pheight)
        sbfunc3()
        dev.off()}

    temp_switch=F #temporarily turning off the following section
    if(temp_switch)
    {
      if(forecast){
        if(nforecastyears==nforecastyearswithsd){
          sbstd <- rawstd[rawstd$name %in% c("spbio_std"),]
          sbstdfore <- rawstd[rawstd$name %in% c("depletion"),]
          sbstdfore <- sbstdfore[(seq(6,(6+nforecastyears-1),by=1)),]
          v <- as.numeric(sbstd$value)
          sbstd$upper <- v+1.96*(as.numeric(sbstd[,4]))
          sbstdfore$upper <- as.numeric(sbstdfore$value)+1.96*(as.numeric(sbstdfore[,4]))
          sbstd$lower <- v-1.96*(as.numeric(sbstd[,4]))
          sbstd$lower[sbstd$lower < 0] <- 0
          sbstdfore$lower <- as.numeric(sbstdfore$value)-1.96*(as.numeric(sbstdfore[,4]))
          sbstdfore$lower[sbstdfore$lower < 0] <- 0
          ymax <- max(as.numeric(sbstd$upper),as.numeric(sbstdfore$upper))
          plottitle <- "~95% Asymptotic confidence interval with forecast"
          xmin <- min(as.numeric(tsyears))
          yr <- as.numeric(goodforcast$Year)
          utsyears <- c(utsyears,min(yr))
          v <- c(v,goodforcast$Spbio[1])
          plotup <- c(sbstd$upper,sbstdfore$upper[1])
          plotlow <- c(sbstd$lower, sbstdfore$lower[1])
          xmax <- max(yr)+1
          sbfunc4 <- function(){
            plot(utsyears,v,main= plottitle,xlab="Year",xlim=c(xmin,xmax),ylab=ylab,ylim=c(0,ymax),type="o",col="blue")
            abline(h=0,col="grey")
            lines(utsyears,plotup,lwd=1,col="blue",lty="dashed")
            lines(utsyears,plotlow,lwd=1,col="blue",lty="dashed")
            lines(yr,goodforcast$Spbio,lwd=1,col="red",lty="dashed")
            points(yr,goodforcast$Spbio[1:(nforecastyears)],col="red",pch=20)
            lines(yr,sbstdfore$upper,lwd=1,col="red",lty="dashed")
            lines(yr,sbstdfore$lower,lwd=1,col="red",lty="dashed")}
          if(7 %in% plot) sbfunc4()
          if(7 %in% print){
            png(file=paste(plotdir,"7spawnbioforecastinterval.png",sep=""),width=pwidth,height=pheight)
            sbfunc4()
            dev.off()}
        } # forecast
      } # sexes==1
  } #temporarily turning off section on forecast
    } # hessian==T
    if(verbose) print("Finished plot 7: Basic time series",quote=F)
  } # end if 7 in plot or print


  # Plot 8: depletion
  if(8 %in% c(plot, print))
  {
    ylab <- "Spawning depletion"
    depfunc <- function(iarea){
      plottitle <- NULL
      if(nareas>1) plottitle <- paste("Spawning depletion in area",iarea)
      plot(tsyears[tsarea==iarea],dep[tsarea==iarea],xlab="Year",ylab=ylab,ylim=c(0,(max(dep))),type="o",col="blue",main=plottitle)
      abline(h=0,col="grey")
      abline(h=c(btarget,minbthresh),col="red")
      text(startyr+4,btarget+0.03,"Management target",adj=0)
      text(startyr+4,minbthresh+0.03,"Minimum stock size threshold",adj=0)

    }
    for(iarea in areas){
      if(8 %in% plot) depfunc(iarea)
      if(8 %in% print){
        png(file=paste(plotdir,"8depletion.png",sep=""),width=pwidth,height=pheight)
        depfunc(iarea)
        dev.off()}
    }


    # if depletion_basis not equal to 1, then code below needs changing
    if(hessian & depletion_basis==1){
      depstd <- rawstd[rawstd$name=="depletion",]
      depstd$upper <- depstd$value + 1.96*depstd$std_dev
      depstd$lower <- depstd$value - 1.96*depstd$std_dev
      if(depletion_basis==1){
        depstd$upper <- depstd$upper * depletion_level
        depstd$lower <- depstd$lower * depletion_level}
      ymax <- max(dep,depstd$upper)
      len <- length(tsyears)
      yr1 <- tsyears[-(1:3)]
      depfunc2 <- function(){
        plot(tsyears[tsarea==1],depstd$value,xlab="Year",ylab=ylab,ylim=c(0,ymax),type="o",col="blue")
        abline(h=0,col="grey")
        abline(h=c(btarget,minbthresh),col="red")
        text((startyr+4),(btarget+0.03),"Management target",adj=0)
        text((startyr+4),(minbthresh+0.03),"Minimum stock size threshold",adj=0)
        points(yr1,depstd$upper,pch="-",col="blue",cex=1.2)
        lines(yr1,depstd$upper,col="blue",lty="dashed")
        points(yr1,depstd$lower,pch="-",col="blue",cex=1.2)
        lines(yr1,depstd$lower,col="blue",lty="dashed")}
      if(8 %in% plot) depfunc2()
      if(8 %in% print){
        png(file=paste(plotdir,"8depletioninterval.png",sep=""),width=pwidth,height=pheight)
        depfunc2()
        dev.off()}
    }
    if(forecast){
      ymax <- max(dep,as.numeric(goodforcast$Depletion))
      xmin <- min(tsyears)-1
      yr <- as.numeric(goodforcast$Year)
      xmax <- max(yr)+1
      depfunc3 <- function(){
        plot(tsyears,dep,xlab="Year",ylab=ylab,xlim=c(xmin,xmax),ylim=c(0,ymax),type="o",col="blue")
        abline(h=0,col="grey")
        abline(h=c(btarget,minbthresh),col="red")
        text((startyr+4),(btarget+0.03),"Management target",adj=0)
        text((startyr+4),(minbthresh+0.03),"Minimum stock size threshold",adj=0)
        lines(yr,goodforcast$Depletion,lwd=1,col="red",lty="dashed")
        points(goodforcast$Year[2:nforecastyears],goodforcast$Depletion[2:nforecastyears],col="red",pch=20)}
      if(8 %in% plot) depfunc3()
      if(8 %in% print){
        png(file=paste(plotdir,"8depletionforecast.png",sep=""),width=pwidth,height=pheight)
        depfunc3()
        dev.off()}
      if(covar){
        depstdfore <- rawstd[rawstd$name=="depletion",]
        depstdfore <- depstdfore[(6+2*nforecastyears):((6+2*nforecastyears)+nforecastyears-1),]
        depstdfore$upper <- depstdfore$value + 1.96* depstdfore$std_dev
        depstdfore$lower <- depstdfore$value - 1.96* depstdfore$std_dev
        ymax <- max(c(dep,depstdfore$upper))
        xmin <- min(tsyears)-1
        xmax <- max(goodforcast$Year)+1
        len <- length(tsyears)
        yr1 <- tsyears[(len-1):len]
        yr2 <- goodforcast$Year[2:nforecastyears]
        yr3 <- goodforcast$Year
        depfunc4 <- function(){
          plot(tsyears,dep,xlab="Year",ylab=ylab,xlim=c(xmin,xmax),ylim=c(0,ymax),type="o",col="blue")
          abline(h=0,col="grey")
          abline(h=c(btarget,minbthresh),col="red")
          text((startyr+4),(btarget+0.03),"Management target",adj=0)
          text((startyr+4),(minbthresh+0.03),"Minimum stock size threshold",adj=0)
          points(yr1,depstd$upper,pch="-",col="blue",cex=1.2)
          lines(yr1,depstd$upper,col="blue",lty="dashed")
          points(yr1,depstd$lower,pch="-",col="blue",cex=1.2)
          lines(yr1,depstd$lower,col="blue",lty="dashed")
          lines(yr3,goodforcast$Depletion,lwd=1,col="red",lty="dashed")
          points(yr2,goodforcast$Depletion[2:(nforecastyears)],col="red",pch=20)
          lines(yr3,depstdfore$upper,lwd=1,col="red",lty="dashed")
          points(yr2,depstdfore$upper[2:(nforecastyears)],pch="-",col="red",cex=1.2)
          lines(yr3,depstdfore$lower,lwd=1,col="red",lty="dashed")
          points(yr2,depstdfore$lower[2:(nforecastyears)],pch="-",col="red",cex=1.2)}
        if(8 %in% plot) depfunc4()
        if(8 %in% print){
          png(file=paste(plotdir,"8depletionforecastinterval.png",sep=""),width=pwidth,height=pheight)
          depfunc4()
          dev.off()}
      } # hessian==T
    } # forecast==T
    if(verbose) print("Finished plot 8: depletion",quote=F)
  } # end if 8 in plot or print


  # Plot 9: rec devs and asymptotic error check
  if(9 %in% c(plot, print))
  {
    if(covar){
      recdev <- parameters[substring(parameters$Label,1,7)=="RecrDev",]
      if(nrow(recdev)>0){
        recdev$Yr <- as.numeric(substring(recdev$Label,9,nchar(recdev$Label[1])))
        ylab <- "Log Recruitment deviation"
        recdevfunc <- function(){
          plot(recdev$Yr,recdev$Value,xlab="Year",main="",ylab=ylab,type="b")
          abline(h=0,col="black")}
        if(9 %in% plot) recdevfunc()
        if(9 %in% print){
          png(file=paste(plotdir,"9recdevs.png",sep=""),width=pwidth,height=pheight)
          recdevfunc()
          dev.off()}
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
          png(file=paste(plotdir,"9recdevvarcheck.png",sep=""),width=pwidth,height=pheight)
          recdevfunc2()
          dev.off()}
      } # rec devs
    } # hessian = T
    if(verbose) print("Finished plot 9: rec devs and asymptotic error check",quote=F)
    flush.console()
  } # end if 9 in plot or print

  ### Plot 10: average body weight observations ###
  if(10 %in% c(plot, print))
  {
    if(!is.na(mnwgt))
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
          ymax <- max(ob + uiw)
          ymax <- max(ymax,ex)
          ptitle <- paste("Mean weight in discard for fleet",i,sep=" ")
          if(j==2) ptitle <- paste("Mean weight in retained catch for fleet",i,sep=" ")
          if(j==0) ptitle <- paste("Mean weight in whole catch for fleet",i,sep=" ")
          ylab <- "Mean individual body weight (kg)"
          bdywtfunc <- function(){
            plotCI(x=yr,y=ob,uiw=uiw,liw=ob*1.96*cv,xlab="Year",main=ptitle,ylo=0,col="red",sfrac=0.001,z=ymax,ylab=ylab,lty=1,xlim=c(xmin,xmax))
            abline(h=0,col="grey")
            points(yr,ex,col="blue",cex=2,pch="-")}
          if(10 %in% plot) bdywtfunc()
          if(10 %in% print){
            png(file=paste(plotdir,"10bodywtfit",i,".png",sep=""),width=pwidth,height=pheight)
            bdywtfunc()
            dev.off()}
        } # market
      } # fleets
    } # if mean weight data exists
    if(verbose) print("Finished plot 10: average body weight observations",quote=F)
    flush.console()
  } # end if 10 in plot or print

  ### Plot 11: SPR ###
  if(11 %in% c(plot, print))
  {
    sprfunc <- function(){
      plot(sprseries$Year,sprseries$spr,xlab="Year",ylab="SPR",ylim=c(0,max(1,max(sprseries$spr[!is.na(sprseries$spr)]))),type="o",col="blue")
      abline(h=sprtarg,col="red",lty=2)
      abline(h=0,col="grey")
      abline(h=1,col="grey")}
    if(11 %in% plot) sprfunc()
    if(11 %in% print){
      png(file=paste(plotdir,"11sprseries",i,".png",sep=""),width=pwidth,height=pheight)
      sprfunc()
      dev.off()}

    timeseries$Yr <- timeseries$Yr + (timeseries$Seas-1)/nseasons
    ts <- timeseries[timeseries$Area==1 & timeseries$Yr <= endyr+1,] #!subsetting to area 1 only. This should be generalized
    tsyears <- ts$Yr[ts$Seas==1]
    tsspaw_bio <- ts$SpawnBio[ts$Seas==1]
    if(nsexes==1) tsspaw_bio <- tsspaw_bio/2
    depletionseries <- tsspaw_bio/tsspaw_bio[1]

    reldep <- depletionseries[tsyears %in% sprseries$Year]/btarget
    relspr <- 1-sprseries$spr
    xymax <- 1.1*max(c(reldep,relspr[!is.na(relspr)]))
    phasefunc <- function(){
      plot(reldep,relspr,xlab="B/Btarget",xlim=c(0,xymax),ylim=c(0,1.0),ylab="1-SPR",type="o",col="blue")
      abline(h=0,col="grey")
      points(reldep[length(reldep)],relspr[length(relspr)],col="red",pch=19)
      abline(h=1-sprtarg,col="red",lty=2)
      abline(v=1,col="red",lty=2)}
    if(11 %in% plot) phasefunc()
    if(11 %in% print){
      png(file=paste(plotdir,"11sprphase",i,".png",sep=""),width=pwidth,height=pheight)
      phasefunc()
      dev.off()}
    if(verbose) print("Finished plot 11: SPR",quote=F)
    flush.console()
  } # end if 11 in plot or print

  ### Plot 12: spawner-recruit curve ###
  if(12 %in% c(plot, print))
  {
    recruit <- recruit[recruit$year <= (endyr+1),]
    ymax <- max(recruit$pred_recr)
    x <- recruit$spawn_bio
    xmax <- max(x)
    xlab <- "Spawning biomass (mt)"
    ylab <- "Recruitment (1,000s)"
    recruitfun <- function(){
      plot(x,recruit$with_env,xlab=xlab,ylab=ylab,type="l",col="blue",ylim=c(0,ymax),xlim=c(0,xmax))
      abline(h=0,col="grey")
      biasad <- recruit$bias_adj
      lines(x[-length(x)],biasad[-length(biasad)],col="green")
      lines(x,recruit$exp_recr,lwd=2,col="black")
      points(x,recruit$pred_recr,col="red")}
    if(12 %in% plot) recruitfun()
    if(12 %in% print){
      png(file=paste(plotdir,"12srcurve",i,".png",sep=""),width=pwidth,height=pheight)
      recruitfun()
      dev.off()}
    if(verbose) print("Finished plot 12: spawner-recruit curve",quote=F)
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
        if(npoints > 3 & smooth){
          psmooth <- loess(z~y,degree=1)
          lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}}
      if(13 %in% print){
        png(file=paste(plotdir,"13cpuefit",i,".png",sep=""),width=pwidth,height=pheight)
        plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab="Year",ylo=0,col="red",ylab="Index",main=main,lty=1)
        abline(h=0,col="grey")
        lines(x,z,lwd=2,col="blue")
        dev.off()
        png(file=paste(plotdir,"13cpuecheck",i,".png",sep=""),width=pwidth,height=pheight)
        plot(y,z,xlab=xlab,main=main,ylim=c(0,max(z)),xlim=c(0,max(y)),col="blue",pch=19,ylab="Expected index")
        abline(h=0,col="grey")
        lines(x=c(0,max(z)),y=c(0,max(z)),col="black")
        npoints <- length(z)
        if(npoints > 3 & smooth){
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
        png(file=paste(plotdir,"13logcpuefit",i,".png",sep=""),width=pwidth,height=pheight)
        plotCI(x=x,y=log(y),z=log(z),sfrac=0.001,uiw=uiw,liw=liw,xlab="Year",col="red",ylab=ylab,main=main,lty=1)
        lines(x,log(z),lwd=2,col="blue")
        dev.off()
        png(file=paste(plotdir,"13logcpuecheck",i,".png",sep=""),width=pwidth,height=pheight)
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
        natagetemp0 <- natage[natage$Area==iarea & natage$Gender==m & natage$Morph==mainmorphs[m] & natage$Seas==1 &
                            natage$Seas==1 & natage$Era!="VIRG" & natage$Yr <= (endyr+1),]
        nyrsplot <- nrow(natagetemp0)
        resx <- rep(natagetemp0$Yr, accuage+1)
        resy <- NULL
        for(i in 0:accuage) resy <- c(resy,rep(i,nyrsplot))
        resz <- NULL
        for(i in 11+0:accuage) resz <- c(resz,natagetemp0[,i])
        plotbub <- cbind(resx,resy,resz)
        if(m==1 & nsexes==1) sextitle <- ""
        if(m==1 & nsexes==2) sextitle <- "of females"
        if(m==2) sextitle="of males"
        if(nareas>1) sextitle <- paste("in area",iarea,sextitle)
        plottitle <- paste("Expected numbers ",sextitle," at age in thousands (max=",max(resz),")",sep="")

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
        ylab <- plottitle <- paste("Mean age ",sextitle," in the population (yr)",sep="")
        if(14 %in% plot){
          print(nage)
          plot(meanageyr,meanage,xlab="Year",ylim=ylim,type="o",ylab=ylab,col="black",main=plottitle)}
        if(14 %in% print){
          filepart <- paste("_sex",m,sep="")
          if(nareas > 1) filepart <- paste("_area",iarea,filepart,sep="")
          png(file=paste(plotdir,"14natage",filepart,".png",sep=""),width=pwidth,height=pheight)
          print(nage)
          dev.off()
          png(file=paste(plotdir,"14meanage",filepart,".png",sep=""),width=pwidth,height=pheight)
          plot(meanageyr,meanage,xlab="Year",ylim=ylim,type="o",ylab=ylab,col="black",main=plottitle)
          dev.off()}
      } # end gender loop
    } # end area loop
    if(verbose) print("Finished plot 14: numbers at age",quote=F)
    flush.console()
  } # end if 14 in plot or print

  # Plots of comps: data only
  lendbase   <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
  agedbase   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0,]
  latagebase <- compdbase[compdbase$Kind=="L@A" & compdbase$N > 0,]

  lendbase$effN <- as.numeric(lendbase$effN)
  agedbase$effN <- as.numeric(agedbase$effN)

  if(datplot & length(intersect(15:16,c(plot, print)))>0) # plots 15 and 16
  {
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
            if(nseasons > 1){ldat2$Yr <- ldat2$Yr + (ldat2$Seas - 1)*(1/nseasons) + (1/nseasons)/2}
            ldat2$plotyear <- as.factor(ldat2$Yr)
            ldat2$plotbins <- ldat2$Bin
            ldat2 <- ldat2[!is.na(ldat2$plotbins),]
            ldat2$plotobs <- ldat2$Obs
            bincounter <- as.integer(nlbins/5)
            usebins <- seq(1,bincounter*5,by=bincounter)
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
              png(file=paste(plotdir,"15lendatbar_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight)
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
              png(file=paste(plotdir,"15lendatbub_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight)
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
              if(nseasons > 1){adat2$Yr <- adat2$Yr + (adat2$Seas - 1)*(1/nseasons) + (1/nseasons)/2}
              adat2$plotyear <- as.factor(adat2$Yr)
              adat2$plotbins <- adat2$Bin
              adat2 <- adat2[!is.na(adat2$plotbins), ]
              adat2$plotobs <- adat2$Obs
              maxbin <- max(agebins)
              minbin <- min(agebins)
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
                                    x=list(tick.number=5,limits=c(0,(maxbin+1))),relation="same",alternating="1",tck=c(1,0)),data=adat2)
              if(16 %in% plot) print(trellis2)
              if(16 %in% print)
              { sex <- 1
                if(k==3){sex <- 2}
                png(file=paste(plotdir,"16agedatbar_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight)
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
                png(file=paste(plotdir,"16lendatbub_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight)
                print(trellis2)
                dev.off()}
            } # market
          } # k loop
        } # non-conditional check
      } # if ages
    } # fleets

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
          if(Lbin_method==3) cadat$lenbin2 <- cadat$lenbin
          cadat$lenbin <- as.factor(cadat$lenbin2)
          cadat$group <- cadat$obsexp
          cadat$group[cadat$obsexp=="exp"] <- cadat$Yr[cadat$obsexp=="exp"]
          years <- unique(cadat$Yr)
          years <- years[order(years)]
          nyears <- length(years)

          if(16 %in% plot)
          {
            plotspot <- 2
            plotspot2 <- 1
            plot.new()
            text(0.5,0.5,paste("Conditional age data for ",c("fe","")[m],"males from ", FleetNames[i],sep=""),cex=1)
            for(fy in 1:nyears)
            {
              more <- T
              if(fy==nyears | fy %in% c(8,16,24,32,40,48,56)){more<-F}
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
          if(16 %in% print)
          {
            page <- 1
            plotspot <- 2
            plotspot2 <- 1
            png(file=paste(plotdir,"16ageatlen_flt",i,"sex",m,"page",page,".png",sep=""),width=pwidth,height=pheight)
            for(fy in 1:nyears)
            {
              more <- T
              if(fy==nyears | fy %in% c(8,16,24,32,40,48,56)){more<-F}
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
                if(!(fy==nyears)){png(file=paste(plotdir,"16ageatlen_flt",i,"sex",m,"page",page,".png",sep=""),width=pwidth,height=pheight)}
              }
            } # nyears
          } # print
        } # m
      } # if fleets
    } # fleet loop
    if(verbose) print("Finished plot 16: age comp data ",quote=F)
    flush.console()
  } # datplot


  # Plot 17: length comps with fits
  if(17 %in% c(plot, print))
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
            if(nseasons > 1){lfit2$Yr <- lfit2$Yr + (lfit2$Seas - 1)*(1/nseasons) + (1/nseasons)/2}
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
    if(samplesizeON){
            lfitfunc <- function()
            {
              plot(lfit2$N,lfit2$effN,xlab="Observed sample size",main=plottitle,ylim=c(0,ymax),xlim=c(0,xmax),col="blue",pch=19,ylab="Effective sample size")
              abline(h=0,col="grey")
              lines(x=c(0,ymax),y=c(0,ymax),col="black")
              npoints <- length(lfit2$N)
              if(npoints > 3 & smooth & length(unique(lfit2$N))>3)
              { psmooth <- loess(lfit2$effN~lfit2$N,degree=1)
                lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
            }
            if(17 %in% plot) lfitfunc()
            if(17 %in% print){
              sex <- 1
              if(k==3){sex <- 2}
              png(file=paste(plotdir,"17lendatfitsampsize_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight)
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
              png(file=paste(plotdir,"17lendatfit_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight)
              print(print(trellis1))
              dev.off()}

            if(bubbleON){
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
              if(17 %in% plot) print(bub)
              if(17 %in% print){
                sex <- 1
                if(k==3){sex <- 2}
                png(file=paste(plotdir,"17lendatresids_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight)
                print(bub)
                dev.off()}
            } # end if bubbleON
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
    if(verbose) print("Finished plot 17: length comps with fits",quote=F)
    flush.console()
  } # end if 17 in plot or print

  # Plot 18: traditional age comps
  if(18 %in% c(plot, print))
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
            if(nseasons > 1){afit2$Yr <- afit2$Yr + (afit2$Seas - 1)/nseasons + 1/(2*nseasons)}
            afit2$plotyear <- as.factor(afit2$Yr)
            afit2$plotbins <- afit2$Bin
            afit2 <- afit2[!is.na(afit2$plotbins),]
            afit2$plotobs <- afit2$Obs
            afit2$plotexp <- afit2$Exp
            
            # may optionally turn of sample size plots in #18
            if(samplesizeON){ 
              
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
                if(npoints>3 & smooth & length(unique(afit2$N))>3)
                { psmooth <- loess(afit2$effN~afit2$N,degree=1)
                  lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
              }
              if(18 %in% plot){afitssfunc()}
              if(18 %in% print)
              { sex <- 1
                if(k==3){sex <- 2}
                png(file=paste(plotdir,"18agedatfitsampsize_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight)
                afitssfunc()
                dev.off()}
            } # end if samplesizeON
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
            if(18 %in% plot){print(trellis1)}
            if(18 %in% print)
            { sex <- 1
              if(k==3){sex <- 2}
              png(file=paste(plotdir,"18agedatfit_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight)
              print(trellis1)
              dev.off()}
            
            # may optionally turn of residual plots in #18
            if(bubbleON){   
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
                  png(file=paste(plotdir,"18agedatfitresids_flt",i,"sex",sex,"mkt",j,".png",sep=""),width=pwidth,height=pheight)
                  print(bub)
                  dev.off()}
            } # end if bubbleON
          } # market
        } # k loop
      } # if lengths exist
    } # fleet loop for lengths
    if(verbose) print("Finished traditional age comps",quote=F)

    ## Effective sample sizes for conditional age data
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
            if(npoints > 3 & smooth & length(unique(femsamps$N))>3){
              psmooth <- loess(femsamps$effN~femsamps$N,degree=1)
              lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}}
          if(18 %in% plot) alenssfunc()
          if(18 %in% print){
            png(file=paste(plotdir,"18ageatlensamplesize_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
            alenssfunc()
            dev.off()}
        } # m
      } # if ages
    } # fleet loop

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
                if(Lbin_method==1) aydat$lenbin2 <- lbins[aydat$lenbin]
                if(Lbin_method==3) aydat$lenbin2 <- aydat$lenbin
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
                if(18 %in% plot){print(trellis1)}
                if(18 %in% print)
                { png(file=paste(plotdir,"18ageatlenyearfit_flt",i,"sex",m,"year",j,".png",sep=""),width=pwidth,height=pheight)
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
                if(18 %in% plot){print(bub)}
                if(18 %in% print)
                { png(file=paste(plotdir,"18ageatlenyearresids_flt",i,"sex",m,"year",j,".png",sep=""),width=pwidth,height=pheight)
                  print(bub)
                  dev.off()}
              } # m
            } # fleet statement
          } # fleet loop
        } # test of year positive loop
      } # aalyear loop
    } # aalyear statement
    if(verbose) print("Finished age at length residuals",quote=F)

    ## aalbin and make bin-specific key over all years if positive ###
    if(aalbin > 0)
    {
      if(length(agedbase$Obs[agedbase$Lbin_hi %in% aalbin])>0)
      {
        agedbase2 <- agedbase[agedbase$Lbin_hi %in% aalbin,]
        for(i in fleets)
        {
          if(length(agedbase2$Obs[agedbase2$Fleet==i])>0)
          {
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
              trellis1 <- xyplot(trellval~plotbins|year,as.table=T,groups=group,type=typ,pch=pch,lty=lty,lwd=1.5,
                                 strip=strip.custom(bg="grey"),ylab="Proportion",xlab="Age (yrs)",col=col,cex=0.6,
                                 main=plottitle,scales=list(relation="same",alternating="1",tck=c(1,0)),data=abin)
              if(18 %in% plot){print(trellis1)}
              if(18 %in% print)
              { png(file=paste(plotdir,"18ageatlenbinfit_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
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
            if(Lbin_method==1) ares$lenbin2 <- lbins[ares$lenbin]
            if(Lbin_method==3) ares$lenbin2 <- ares$lenbin
            ares$lenbin <- as.factor(ares$lenbin2)
            ares$group <- ares$obsexp
            ares$group[ares$obsexp=="exp"] <- ares$Yr[ares$obsexp=="exp"]
            years <- unique(ares$Yr)
            years <- years[order(years)]
            nyears <- length(years)
            if(18 %in% plot)
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
            if(18 %in% print)
            {
              page <- 1
              plotspot <- 2
              plotspot2 <- 1
              png(file=paste(plotdir,"18ageatlenresids_flt",i,"sex",m,"page",page,".png",sep=""),width=pwidth,height=pheight)
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
                  if(!(fy==nyears)){png(file=paste(plotdir,"18ageatlenresids_flt",i,"sex",m,"page",page,".png",sep=""),width=pwidth,height=pheight)}
                }
              } # nyears
            } # print
          } # m
        } # if fleet
      } # fleets
    } # aalresids
    if(verbose) print("Finished plot 18: age comps",quote=F)
    flush.console()
  } # end if 18 in plot or print

  # Plot 19: length at age data
  if(19 %in% c(plot, print))
  {
    for(i in fleets)
    {
      if(length(latagebase$Obs[latagebase$Fleet==i])>0)
      {
        plotlens <- latagebase[latagebase$Fleet==i,]
        testor <- length(plotlens$Obs[plotlens$Gender==1])>0
        testor[2] <- length(plotlens$Obs[plotlens$Gender==2])>0
        for(m in (1:2)[testor])
        {
          la <- plotlens[plotlens$Gender==i,] # females or males
          if(nseasons > 1){la$Yr <- la$Yr + (la$Seas - 1)*(1/nseasons) + (1/nseasons)/2}
          la$plotyear <- as.factor(la$Yr)
          la$plotbins <- la$Bin
          la <- la[!is.na(la$plotbins), ]
          la$plotobs <- la$Obs
          la$plotexp <- la$Exp
          plottitle <- paste("Sample size for length-at-age for ",c("fe","")[m],"males, ", FleetNames[i],sep="")
          ymax <- max(la$effN)
          xmax <- max(la$N)
          lenatagefunc <- function(){
            plot(la$N,la$effN,xlab="Observed sample size",main=plottitle,ylim=c(0,ymax),xlim=c(0,xmax),col="blue",pch=19,ylab="Effective sample size")
            abline(h=0,col="grey")
            lines(x=c(0,ymax),y=c(0,ymax),col="black")
            npoints <- length(la$N)
            if(npoints > 3 & smooth & length(unique(la$N))>3){
              psmooth <- loess(la$effN~la$N,degree=1)
              lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}}
          #if(19 %in% plot){lenatagefunc()}
          #if(19 %in% print)
          # {png(file=paste(plotdir,"19lenatagesampsize_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
          # lenatagefunc()
          # dev.off()}
          plottitle <- paste("Length-at-age fits for ",c("fe","")[m],"males, ", FleetNames[i],sep="")
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
          if(19 %in% plot) print(trellis1)
          if(19 %in% print){
            png(file=paste(plotdir,"19lenatagefit_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
            print(trellis1)
            dev.off()}
          resx <- la$Yr
          resy <- la$plotbins
          resz <- la$Pearson
          plottitle <- paste("Pearson residuals for ",c("fe","")[m],"males, ", FleetNames[i],sep="")
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
          if(19 %in% plot) print(bub)
          if(19 %in% print){
            png(file=paste(plotdir,"19lenatageresids_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight)
            print(bub)
            dev.off()}
        } # m
      } # if lengths
    } # fleets
    if(verbose) print("Finished plot 19: length at age data",quote=F)
    flush.console()
  } # end if 19 in plot or print



  ### end of function
}
