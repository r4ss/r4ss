SSv3_plots <-
function(
    replist="ReportObject", plot=1:24, print=0, pdf=F, printfolder="", dir="default", fleets="all", areas="all",
    fleetnames="default", fleetcols="default", fleetlty=1, fleetpch=1, lwd=1, areacols="default", areanames="default",
    verbose=T, uncertainty=T, forecastplot=F, datplot=F, Natageplot=T, samplesizeplots=T, compresidplots=T,
    sprtarg=0.4, btarg=0.4, minbthresh=0.25, pntscalar=2.6, minnbubble=8, aalyear=-1, aalbin=-1, aalmaxbinrange=0,
    aalresids=F, maxneff=5000, cohortlines=c(), smooth=T, showsampsize=T, showeffN=T, showlegend=T,
    pwidth=7, pheight=7, punits="in", ptsize=12, res=300, cex.main=1,selexlines=1:5,
    rows=1, cols=1, maxrows=6, maxcols=6, maxrows2=2, maxcols2=4, tagrows=3, tagcols=3, fixdims=T, new=T,...)

{
################################################################################
#
# SSv3_plots
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: To sumarize the results of an SSv3 model run.
# Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
#	   Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
#	   and other contributors to http://code.google.com/p/r4ss/
# Returns: Plots with plot history in R GUI and/or .png files.
# General: Updated for Stock Synthesis version 3.04; R version 2.8.1
# Notes:   See users guide for documentation.
# Required SS3v_output function and plotrix package
# Credit:  Based loosely on an early version of "Scape" (A. Magnusson) and "Output viewer" (R. Methot)
#
################################################################################

  codedate <- "December 21, 2009"

  if(verbose){
    print(paste("R function updated:",codedate),quote=F)
    print("Check for new code and report problems at http://code.google.com/p/r4ss/",quote=F)
  }

  flush.console()

#################################################################################
## embedded functions: bubble3, plotCI, rich.colors.short, matchfun2
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
	  plot(x,y,type='n',xlim=xlim,main=main,cex.main=cex.main,xlab=xlab,ylab=ylab,axes=F,cex.main=cex.main)
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
		       }else{		 out <- objsubset[(line1+adjust1):(line2+adjust2), ]}
    return(out)
  }

  # label table is a step toward internationalization of the code
  # in the future, this could be read from a file, or we could have multiple columns
  # in the table to choose from
  labeltable = as.data.frame(matrix(c(
    1,	 "Length (cm)",
    2,	 "Age (yr)",
    3,	 "Year",
    4,	 "Maturity",
    5,	 "Selectivity",
    6,	 "Landings (mt)",
    7,	 "Total catch (mt)",
    8,	 "Predicted Discards (mt)",
    9,	 "Discard fraction by weight",
    10,	  "Retention",
    11,	  "Index",
    12,	  "Expected index",
    13,	  "True age (yr)",
    14,	  "SD of observed age (yr)",
    15,	  "Observed sample size",
    16,	  "Effective sample size",
    17,	  "Proportion",
    18,	  "cm",
    19,	  "Frequency",
    20,	  "Weight (lbs)"
    ),20,2,byrow=T))
  lab = as.character(labeltable[,2])

  # get quantities from the big list
  if(replist[[1]]=="ReportObject"){
    return("The input 'replist' should refer to an R object created by the function 'SSv3_output'.")
  }

  nfleets			 <- replist$nfleets
  nfishfleets			 <- replist$nfishfleets
  nsexes			 <- replist$nsexes
  ngpatterns			 <- replist$ngpatterns
  lbins				 <- replist$lbins
  nlbins			 <- replist$nlbins
  lbinspop			 <- replist$lbinspop
  nlbinspop			 <- replist$nlbinspop
  agebins			 <- replist$agebins
  nagebins			 <- replist$nagebins
  accuage			 <- replist$accuage
  nareas			 <- replist$nareas
  startyr			 <- replist$startyr
  endyr				 <- replist$endyr
  nseasons			 <- replist$nseasons
  seasfracs			 <- replist$seasfracs
  nforecastyears		 <- replist$nforecastyears
  morph_indexing		 <- replist$morph_indexing
  biology			 <- replist$biology
  endgrowth			 <- replist$endgrowth
  growthseries			 <- replist$growthseries
  sizeselex			 <- replist$sizeselex
  retention			 <- replist$retention
  ageselex			 <- replist$ageselex
  timeseries			 <- replist$timeseries
  F_method			 <- replist$F_method
  depletion_method		 <- replist$depletion_method
  depletion_basis		 <- replist$depletion_basis
  discard			 <- replist$discard
  discard_type			 <- replist$discard_type
  DF_discard			 <- replist$DF_discard
  mnwgt				 <- replist$mnwgt
  DF_mnwgt			 <- replist$DF_mnwgt
  sprseries			 <- replist$sprseries
  recruit			 <- replist$recruit
  cpue				 <- replist$cpue
  natage			 <- replist$natage
  movement			 <- replist$movement
  ALK				 <- replist$ALK
  AAK				 <- replist$AAK
  compdbase			 <- replist$composition_database
  derived_quants		 <- replist$derived_quants
  parameters			 <- replist$parameters
  FleetNames			 <- replist$FleetNames
  covar				 <- replist$covar
  stdtable			 <- replist$stdtable
  #rawstd			  <- replist$rawstd
  SS_version			 <- replist$SS_version
  Run_time			 <- replist$Run_time
  Files_used			 <- replist$Files_used
  used_likelihoods		 <- replist$used_likelihoods
  raw_likelihoods_by_fleet	 <- replist$raw_likelihoods_by_fleet
  variance_adjustments_by_fleet	 <- replist$variance_adjustments_by_fleet
  N_estimated_parameters	 <- replist$N_estimated_parameters
  estimated_non_rec_devparameters <- replist$estimated_non_rec_devparameters
  log_det_hessian		 <- replist$log_det_hessian
  maximum_gradient_component	 <- replist$maximum_gradient_component
  sigma_R_in			 <- replist$sigma_R_in
  rmse_table			 <- replist$rmse_table
  SBzero			 <- replist$SBzero
  current_depletion		 <- replist$current_depletion
  last_years_sprmetric		 <- replist$last_years_sprmetric
  inputs			 <- replist$inputs
  managementratiolabels		 <- replist$managementratiolabels
  equil_yield			 <- replist$equil_yield

  # check for internal consistency
  if(uncertainty==T & inputs$covar==F){
    print("To use uncertainty=T, you need to have covar=T in the input to the SSv3_output function",quote=F)
    return()
  }
  if(forecastplot==T & inputs$forecast==F){
    print("To use forecastplot=T, you need to have forecast=T in the input to the SSv3_output function",quote=F)
    return()
  }
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
  SBlabelflag <- TRUE
  timeseries$Yr <- timeseries$Yr + (timeseries$Seas-1)/nseasons
  ts <- timeseries[timeseries$Yr <= endyr+1,]
  tsyears <- ts$Yr[ts$Seas==1]
  tsarea <- ts$Area[ts$Seas==1]

  tsspaw_bio <- ts$SpawnBio[ts$Seas==1]
  if(nsexes==1) tsspaw_bio <- tsspaw_bio/2
  dep <- tsspaw_bio/tsspaw_bio[1]
  if(nareas > 1){
    for(a in 2:nareas){
      asb <- tsspaw_bio[tsarea==a]
      dep[tsarea==a] <- asb/asb[1]
    }
  }

  if(verbose) print("Finished defining objects",quote=F)
  if(nareas>1){
    print(paste("! Warning: some plots are not configured for mult-area models (nareas=",nareas,")",sep=""),quote=F)
    if(areanames[1]=="default") areanames <- paste("area",1:nareas)
  }

  #### prepare for plotting
  # make plot window (operating system specific)
  nplots <- length(intersect(1:25,plot))
  nprints <- length(intersect(1:25,print))

  if(length(grep('linux',version$os)) > 0) OS <- "Linux"
  if(length(grep('mingw',version$os)) > 0) OS <- "Windows"
  # need appropriate line to support Mac operating systems

  if(nprints>0 & pdf){
    print("can't have pdf=T and print!=0: use print only or pdf & plot inputs",quote=F)
    return()
  }
  if(nplots>0 & !pdf & new){
    if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    if(OS=="Windows") windows(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
    if(OS=="Linux") X11(width=pwidth,height=pheight,pointsize=ptsize)
    if(OS=="Mac") quartz(width=pwidth,height=pheight,pointsize=ptsize)
    plotdir <- NULL
  }
  if(nplots>0 & !new){
    if(verbose) print("Adding plots to existing plot window. Plot history not erased.",quote=F)
  }
  if(nprints>0){
    if(dir=="default") dir <- inputs$dir
    dir.create(dir,showWarnings=F)
    plotdir <- paste(dir,printfolder,"/",sep="")
    dir.create(plotdir,showWarnings=F)
    if(nprints>0 & verbose) print(paste("Plots specified by 'print' will be written to",plotdir),quote=F)
  }
  if(pdf){
    pdffile <- paste(inputs$dir,"/SSplots_",format(Sys.time(),'%d-%b-%Y_%H.%M' ),".pdf",sep="")
    pdf(file=pdffile,width=pwidth,height=pheight)
    if(verbose) print(paste("PDF file with plots will be: ",pdffile,sep=""),quote=F)
  }
  par(mfcol=c(rows,cols))

  if(pdf){
    mar0 <- par()$mar # current margins
    par(mar=rep(0,4))
    plot(0,type='n',xlab='',ylab='',axes=F,xlim=c(0,1),ylim=c(0,1))
    y <- 0.9
    ystep <- -.05
    text(0,y,"Plots created using the 'r4ss' package in R",pos=4)
    y <- y+ystep
    text(0,y,paste("Stock Synthesis version:",substr(SS_version,1,9)),pos=4)
    y <- y+ystep
    text(0,y,Run_time,pos=4)
    y <- y+ystep
    Files2 <- strsplit(Files_used," ")[[1]]
    text(0,y,paste("Control file:",Files2[2]),pos=4)
    y <- y+ystep
    text(0,y,paste("Data file:",Files2[4]),pos=4)
    par(mar=mar0) # replace margins
  }
  # colors and line types
  ians_blues <- c("white","grey","lightblue","skyblue","steelblue1","slateblue",topo.colors(6),"blue","blue2","blue3","blue4","black")
  ians_contour <- c("white",rep("blue",100))
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

  #### plot 1
  # Static growth (mean weight, maturity, fecundity, spawning output)
  if(1 %in% c(plot, print) | length(cohortlines)>0)
  {
    growdat <- replist$endgrowth
    xlab <- lab[1]
    x <- biology$Mean_Size
    ylab <- "Mean weight (kg) in last year"
    ylab2 <- "Spawning output"

    # determine fecundity type
    FecType <- 4
    if("Eg/gm_slope_wt_Fem" %in% parameters$Label) FecType <- 1
    if("Eg/kg_slope_wt_Fem" %in% parameters$Label) FecType <- 1 # name from SSv3.1
    if("Eggs_exp_len_Fem" %in% parameters$Label) FecType <- 2
    if("Eggs_exp_wt_Fem" %in% parameters$Label) FecType <- 3

    # define labels and x-variable
    if(FecType==1){
      fec_ylab <- "Eggs per kg"
      fec_xlab <- "Female weight (kg)"
      par1name <- "Eg/gm_inter_Fem"
      par2name <- "Eg/gm_slope_wt_Fem"
      FecX <- biology$Wt_len_F
    }
    if(substr(SS_version,1,7)=="SS-V3.1"){ # fix for name change in SSv3.1
      par1name <- "Eg/kg_inter_Fem"
      par2name <- "Eg/kg_slope_wt_Fem"
    }
    if(FecType==2){
      fec_ylab <- "Eggs per kg"
      fec_xlab <- "Female length (cm)"
      par1name <- "Eggs_scalar_Fem"
      par2name <- "Eggs_exp_len_Fem"
      FecX <- biology$Mean_Size
    }
    if(FecType==3){
      fec_ylab <- "Eggs per kg"
      fec_xlab <- "Female weight (kg)"
      par1name <- "Eggs_scalar_Fem"
      par2name <- "Eggs_exp_wt_Fem"
      FecX <- biology$Wt_len_F
    }

    par1 <- parameters$Value[parameters$Label==par1name]
    par2 <- parameters$Value[parameters$Label==par2name]
    if(par2!=0) SBlabelflag <- FALSE

    if(FecType==1) FecY <- par1 + par2*FecX
    if(FecType==2) FecY <- par1*FecX^par2
    if(FecType==3) FecY <- par1*FecX^par2

    # Mid year mean length at age with 95% range of lengths (by sex if applicable)
    growdatF <- growdat[growdat$Gender==1 & growdat$Morph==min(growdat$Morph[growdat$Gender==1]),]
    growdatF$Sd_Size <- growdatF$SD_Mid
    growdatF$high <- growdatF$Len_Mid + 1.96*growdatF$Sd_Size
    growdatF$low <- growdatF$Len_Mid - 1.96*growdatF$Sd_Size
    if(nsexes > 1){
      growdatM <- growdat[growdat$Gender==2 & growdat$Morph==min(growdat$Morph[growdat$Gender==2]),]
      xm <- growdatM$Age
      growdatM$Sd_Size <- growdatM$SD_Mid
      growdatM$high <- growdatM$Len_Mid + 1.96*growdatM$Sd_Size
      growdatM$low <- growdatM$Len_Mid - 1.96*growdatM$Sd_Size
    }

    gfunc1 <- function(){ # weight
      plot(x,biology$Wt_len_F,xlab=xlab,ylab=ylab,type="o",col="red")
      abline(h=0,col="grey")
      if(nsexes > 1){
	lines(x,biology$Wt_len_M,col="blue",type="o")
	legend("topleft",bty="n", c("Females","Males"), lty=1, col = c("red","blue"))}
    }
    gfunc2 <- function(){ # maturity
      if(min(biology$Mat_len)<1){ # if length based
        plot(x,biology$Mat_len,xlab=lab[1],ylab=lab[4],type="o",col="red")
      }else{ # else is age based
        plot(growdatF$Age, growdatF$Age_Mat,
             xlab=lab[2],ylab=lab[4],type="o",col="red")
      }
      abline(h=0,col="grey")
    }
    gfunc4 <- function(){ # fecundity
      ymax <- 1.1*max(FecY)
      plot(FecX, FecY,xlab=fec_xlab,ylab=fec_ylab,ylim=c(0,ymax),col="blue",pch=19)
      lines(FecX,rep(par1,length(FecX)),col="red")
      text(mean(range(FecX)),par1-0.05*ymax,"Egg output proportional to spawning biomass")
    }
    gfunc3 <- function(){ # spawning output
      plot(x,biology$Spawn,xlab=lab[1],ylab=ylab2,type="o",col="red")
      abline(h=0,col="grey")
    }
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

    maxy <- max(growdatF$high)
    if(nsexes > 1){maxy <- max(maxy,growdatM$high)}
    x <- growdatF$Age
    main <- "Ending year expected growth"
    # if(nseasons > 1){main <- paste(main," season 1",sep="")}
    ylab <- "Length (cm, middle of the year)"
    gfunc5 <- function()
    {
      plot(x,growdatF$Len_Mid,col="red",lwd=2,ylim=c(0,maxy),type="l",ylab=ylab,xlab=lab[2],main=main,cex.main=cex.main)
      abline(h=0,col="grey")
      lines(x,growdatF$high,col="red",lwd=1,lty="dashed")
      lines(x,growdatF$low,col="red",lwd=1,lty="dashed")
      if(nsexes > 1)
      {
	lines(xm,growdatM$Len_Mid,col="blue",lwd=2,type="l")
	lines(xm,growdatM$high,col="blue",lwd=1,lty="dashed")
	lines(xm,growdatM$low,col="blue",lwd=1,lty="dashed")
	grid()
	legend("topleft",bty="n", c("Females","Males"), lty=1, col = c("red","blue"))
      }
    }
    if(1 %in% plot) gfunc5()
    if(1 %in% print){
      png(file=paste(plotdir,"01_sizeatage.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      gfunc5()
      dev.off()}

    # Natural mortality (if time or sex varying)
    M <- growdatF$M
    if(min(M)!=max(M))
    {
      ymax <- max(M)
      ylab <- "Natural mortality"
      mfunc <- function()
      {
	plot(growdatF$Age,M,col="red",lwd=2,ylim=c(0,ymax),type="o",ylab=ylab,xlab=lab[2])
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
    if(verbose & 1 %in% c(plot,print)) print("Finished plot 1: Static growth (mean weight, maturity, spawning output)",quote=F)
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
	y <- growdatuse$Yr
	z <- growdatuse[,-(1:4)]
	z <- as.matrix(z)
	time <- FALSE
	for(t in 1:ncol(z)) if(max(z[,t])!=min(z[,t])) time <- T
	if(time)
	{
	  if(length(cohortlines)>0) print("Growth is time-varying, which is not yet implemented in cohortlines added to length comps.",quote=F)
	  z <- t(z)
	  if(i==1){main <- "Female time-varying growth"}
	  if(nsexes==1){main <- "Time-varying growth"}
	  if(i==2){main <- "Male time-varying growth"}
	  if(nseasons > 1){main <- paste(main," season 1",sep="")}
	  if(2 %in% plot){
	    persp(x,y,z,col="white",xlab=lab[2],ylab="",zlab=lab[1],expand=0.5,box=T,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
	    contour(x,y,z,nlevels=12,xlab=lab[2],main=main,cex.main=cex.main,col=ians_contour,lwd=2)}
	  if(2 %in% print){
	    png(file=paste(plotdir,"02_timevarygrowthsurf",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	    persp(x,y,z,col="white",xlab=lab[2],ylab="",zlab=lab[1],expand=0.5,box=T,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
	    dev.off()
	    png(file=paste(plotdir,"02_timevarygrowthcontour",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	    contour(x,y,z,nlevels=12,xlab=lab[2],main=main,cex.main=cex.main,col=ians_contour,lwd=2)
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
      intmort <- sizeselex[sizeselex$Factor=="Mort" & sizeselex$gender==m,]
      intkeep <- sizeselex[sizeselex$Factor=="Keep" & sizeselex$gender==m,]
      intdead <- sizeselex[sizeselex$Factor=="Dead" & sizeselex$gender==m,]
      intselex <- sizeselex[sizeselex$Factor=="Lsel" & sizeselex$gender==m,]
      for(i in fleets)
      {
	plotselex <- intselex[intselex$Fleet==i,]
	time <- FALSE
	for(t in 5 + 1:nlbinspop) if(length(unique(plotselex[,t]))>1){time <- TRUE}
	if(time)
	{
	  x <- lbinspop
	  y <- plotselex$year
	  z <- plotselex[,-(1:5)]
	  z <- matrix(as.numeric(as.matrix(z)),ncol=ncol(z))
	  z <- t(z)
	  main <- paste(sextitle1,"varying selectivity for ", FleetNames[i],sep="")
	  if(3 %in% plot)
	  { persp(x,y,z,col="white",xlab=lab[1],ylab=lab[3],zlab=lab[5],expand=0.5,box=T,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
	    contour(x,y,z,nlevels=5,xlab=lab[1],ylab=lab[3],main=main,cex.main=cex.main,col=ians_blues,lwd=2)}
	  if(3 %in% print)
	  { png(file=paste(plotdir,"03_timevarylenselsurf_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	    persp(x,y,z,col="white",xlab=lab[1],ylab=lab[3],zlab=lab[5],expand=0.5,box=T,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
	    dev.off()
	    png(file=paste(plotdir,"03_timevarylenselcontour_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	    contour(x,y,z,nlevels=5,xlab=lab[1],ylab=lab[3],main=main,cex.main=cex.main,col=ians_blues,lwd=2)
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
	  { persp(x,y,z,col="white",xlab=lab[1],ylab=lab[3],zlab=lab[10],expand=0.5,box=T,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
	    contour(x,y,z,nlevels=5,xlab=lab[1],ylab=lab[3],main=main,cex.main=cex.main,col=ians_blues,lwd=2)}
	  if(3 %in% print)
	  { png(file=paste(plotdir,"03_timevaryretsurf_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	    persp(x,y,z,col="white",xlab=lab[1],ylab=lab[3],zlab=lab[10],expand=0.5,box=T,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
	    dev.off()
	    png(file=paste(plotdir,"03_timevaryretcontour_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	    contour(x,y,z,nlevels=5,xlab=lab[1],ylab=lab[3],main=main,cex.main=cex.main,col=ians_blues,lwd=2)
	    dev.off()}
	}
	plotselex <- plotselex[plotselex$year==endyr & plotselex$gender==m,-(1:5)]
	ylab <- lab[5]
	bins <- as.numeric(names(plotselex))
	vals <- as.numeric(paste(plotselex))
	main <- paste(sextitle2," year selectivity for ", FleetNames[i],sep="")
	selfunc <- function()
	{
          # determine whether retention was used
	  intret2 <- intret[intret$Fleet==i,]
	  retchecktemp <- as.vector(unlist(intret2[1,]))
	  retcheck <- as.numeric(retchecktemp[6:length(retchecktemp)])
	  if(is.na(sum(retcheck))) retcheckuse <- 0
	  if(!is.na(sum(retcheck))) retcheckuse <- max(retcheck)-min(retcheck)
          # make plot
	  plot(bins,vals,xlab=lab[1],ylim=c(0,1),main=main,cex.main=cex.main,ylab="",type='n')
	  abline(h=0,col="grey")
	  abline(h=1,col="grey")
	  if(1%in%selexlines) lines(bins,vals,type="o",col="blue",cex=1.1)
	  if(retcheckuse > 0){ # if retention, then add additional lines & legend
	    useret <- intret[intret$Fleet==i,]
	    usekeep <- intkeep[intkeep$Fleet==i,]
	    usemort <- intmort[intmort$Fleet==i,]
	    usedead <- intdead[intdead$Fleet==i,]
	    plotret <- useret[useret$year==max(as.numeric(useret$year)),]
	    plotkeep <- usekeep[usekeep$year==max(as.numeric(usekeep$year)),]
	    plotmort <- usemort[usemort$year==max(as.numeric(usemort$year)),]
	    plotdead <- usedead[usedead$year==max(as.numeric(usedead$year)),]
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
	    legend("bottomright",inset=c(0.05,0.05),bty="n",
		   c(lab[5],lab[10],"Discard mortality","Keep = Sel*Ret","Dead = Sel*(Ret+(1-Ret)*Mort)")[selexlines],
		   lty=1,col=c("blue","red","orange","purple","green3")[selexlines],
		   pch=c(1,3,4,2,5)[selexlines], pt.cex=c(1.1,.9,.9,.9,.9)[selexlines])
	  }
	  mtext(ylab,side=2,line=3)
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
    ylab <- lab[5]
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
	      persp(x,y,z,col="white",xlab=lab[2],ylab=lab[3],zlab=ylab,expand=0.5,box=T,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
	      contour(x,y,z,nlevels=5,xlab=lab[2],main=main,cex.main=cex.main,col=ians_blues,lwd=2)}
	    if(3 %in% print){
	      png(file=paste(plotdir,"03_timevaryageselsurf_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	      persp(x,y,z,col="white",xlab=lab[2],ylab=lab[3],zlab=ylab,expand=0.5,box=T,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
	      dev.off()
	      png(file=paste(plotdir,"03_timevaryageselcontour_flt",i,"sex",m,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	      contour(x,y,z,nlevels=5,xlab=lab[2],main=main,cex.main=cex.main,col=ians_blues,lwd=2)
	      dev.off()}
	    plotageselex2 <- plotageselex[plotageselex$year %in% c(max(as.numeric(plotageselex$year))),]
	    plotageselex2 <- plotageselex2[,-(1:7)]
	    main <- paste(sextitle2," year selectivity for ", FleetNames[i],sep="")
	    endselfunc <- function()
	    {
              plot((as.numeric(names(plotageselex2))),(as.numeric(paste(c(plotageselex2)))),xlab=lab[2],ylim=c(0,1),main=main,cex.main=cex.main,ylab=ylab,type="o",col="blue",cex=1.1)
	      abline(h=0,col="grey")
            }
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
	      plot((as.numeric(names(plotageselex))),vals,xlab=lab[2],ylim=c(0,1),main=main,cex.main=cex.main,ylab=ylab,type="o",col="blue",cex=1.1)
	      abline(h=0,col="grey")
	    }
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
    biofunc <- function(totalORsummary=1){
      plot1 <- ts$Area==1 & ts$Era!="VIRG" # T/F for in area & not virgin value
      plot2 <- ts$Area==1 & ts$Era=="VIRG" # T/F for in area & is virgin value

      # switch for total or summary
      if(totalORsummary==1) yvals <- ts$Bio_all
      if(totalORsummary==2) yvals <- ts$Bio_smry
      if(totalORsummary==1) ylab <- "Total biomass (mt)"
      if(totalORsummary==2) ylab <- "Summary biomass (mt)"

      plot(ts$Yr[plot1],yvals[plot1],
	   xlab=lab[3],ylim=c(0,max(yvals[plot1])),
	   ylab=ylab,type="o",col=areacols[1])
      points(ts$Yr[plot2],yvals[plot2],col=areacols[1],pch=19)
      if(nareas>1){
	for(iarea in 2:nareas){
	  plot1 <- ts$Area==iarea & ts$Era!="VIRG" # T/F for in area & not virgin value
	  plot2 <- ts$Area==iarea & ts$Era=="VIRG" # T/F for in area & is virgin value
	  lines(ts$Yr[plot1],yvals[plot1],type="o",col=areacols[iarea])
	  points(ts$Yr[plot2],yvals[plot2],col=areacols[iarea],pch=19)
	}
	legend("topright",legend=areanames,lty=1,pch=1,col=areacols,bty="n")
      }
      abline(h=0,col="grey")
    }

    if(5 %in% plot){
      biofunc(totalORsummary=1)
      biofunc(totalORsummary=2)
    }
    if(5 %in% print){
      png(file=paste(plotdir,"05_totbio.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      biofunc(totalORsummary=1)
      dev.off()
      png(file=paste(plotdir,"05_summarybio.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      biofunc(totalORsummary=2)
      dev.off()
    }

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
      if (is.na(ylim[1])) ylim <- c(0,1.1*max(y))
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

    # harvest rates
    if(F_method==1){
	Fstring <- "Hrate:_"
	ylabF <- "Harvest rate/Year"
    }else{ # for either continuous F or hybrid F (methods 2 and 3)
	Fstring <- "F:_"
	ylabF <- "Continuous F"
    }

    ### total landings (retained) & catch (encountered)
    goodrows <- ts$Area==1 & ts$Era %in% c("INIT","TIME")
    catchyrs <- ts$Yr[goodrows] # T/F indicator of the lines for which we want to plot catch
    retmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
    totcatchmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar("enc(B)"))=="enc(B)"])
    Hratemat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar(Fstring))==Fstring])

    if(nareas > 1){
      for(iarea in 2:nareas){
	arearows <- ts$Area==iarea & ts$Era %in% c("INIT","TIME")
	retmat <- retmat + as.matrix(ts[arearows, substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
	totcatchmat <- totcatchmat + as.matrix(ts[arearows, substr(names(ts),1,nchar("enc(B)"))=="enc(B)"])
	Hratemat  <- Hratemat  + as.matrix(ts[arearows, substr(names(ts),1,nchar(Fstring))==Fstring])
      }
    }
    # ghost is a fleet with no catch (or a survey for these purposes)
    ghost <- rep(TRUE,nfleets)
    ghost[(1:nfishfleets)[colSums(totcatchmat)>0]] <- FALSE
    discmat <- totcatchmat - retmat

    discfracmat <- discmat/totcatchmat
    discfracmat[totcatchmat==0] <- 0

    # generic function to plot catch, landings, discards or harvest rates
    linefunc <- function(ymat,ylab,addtotal=T){
      if(addtotal & nfishfleets>1){
	ytotal <- rowSums(ymat)
	ymax <- max(ytotal)
      }else{
	ytotal <- rep(NA,nrow(ymat))
	ymax <- max(ymat)
      }
      legendloc <- 'topleft'
      if(ymax>0.6 & ymax<1.0){
	ymax <- 1 # if discards are big, plot full range from 0 to 1
	legendloc <- 'bottomright'
      }

      plot(catchyrs, ytotal, ylim=c(0,ymax), xlab=lab[3], ylab=ylab, type="o")
      abline(h=0,col="grey")
      abline(h=1,col="grey")
      for(f in 1:nfishfleets){
	if(max(ymat[,f])>0){
	  lines(catchyrs, ymat[,f], type="o", col=fleetcols[f],
		lty=fleetlty[f], lwd=lwd, pch=fleetpch[f])
	}
      }
      if(showlegend){
	if(nfishfleets>1 & addtotal){
	  legend(legendloc, lty=fleetlty[!ghost], lwd=lwd, pch=c(1,fleetpch[!ghost]),
		 col=c('black',fleetcols[!ghost]), legend=c('Total',fleetnames[!ghost]), bty='n')
	}else{
	  legend(legendloc, lty=1, lwd=lwd, pch=fleetpch[!ghost], col=fleetcols[!ghost], legend=fleetnames[!ghost], bty='n')
	}
      }
    } # end linefunc

    # function for stacked polygons
    stackfunc <- function(ymat,ylab){
      ## call to embedded, modified function
      stackpoly(x=catchyrs, y=ymat, border='black',
		xlab=lab[3], ylab=ylab, col=fleetcols)
      if(showlegend) legend('topleft', fill=fleetcols[!ghost], legend=fleetnames[!ghost], bty='n')
    } # end stackfunc

    if(5 %in% plot){
      linefunc(ymat=retmat, ylab=lab[6], addtotal=T)
      if(nfishfleets>1) stackfunc(ymat=retmat, ylab=lab[6])
      # only make these plots if there are discards
      if(max(discmat)>0){
	linefunc(ymat=totcatchmat, ylab=lab[7], addtotal=T)
	if(nfishfleets>1) stackfunc(ymat=totcatchmat, ylab=lab[7])
	linefunc(ymat=discmat,ylab=lab[8], addtotal=T)
	if(nfishfleets>1) stackfunc(ymat=discmat,ylab=lab[8])
	linefunc(ymat=discfracmat,ylab=lab[9], addtotal=F)
      }
      linefunc(ymat=Hratemat, ylab=ylabF, addtotal=F)
    }

    if(5 %in% print){
      png(file=paste(plotdir,"05_landings.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      linefunc(ymat=retmat, ylab=lab[6], addtotal=T)
      dev.off()
      png(file=paste(plotdir,"05_harvestrates.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
      linefunc(ymat=Hratemat, ylab=ylabF, addtotal=F)
      dev.off()
      if(nfishfleets>1){
	png(file=paste(plotdir,"05_landings_stacked.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	stackfunc(ymat=retmat, ylab=lab[6])
	dev.off()
      }
      # only make the remaining plots if there are discards
      if(max(discmat)>0)
      {
	png(file=paste(plotdir,"05_totcatch.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	linefunc(ymat=totcatchmat, ylab=lab[7], addtotal=T)
	dev.off()
	png(file=paste(plotdir,"05_discards.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	linefunc(ymat=discmat,ylab=lab[8], addtotal=T)
	dev.off()
	png(file=paste(plotdir,"05_discardfraction.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	linefunc(ymat=discfracmat,ylab=lab[9], addtotal=F)
	dev.off()
	if(nfishfleets>1){
	  png(file=paste(plotdir,"05_totcatch_stacked.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	  stackfunc(ymat=totcatchmat, ylab=lab[7])
	  dev.off()
	  png(file=paste(plotdir,"05_discards_stacked.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	  stackfunc(ymat=discmat,ylab=lab[8])
	  dev.off()
	}
      }
    }
    if(verbose) print("Finished plot 5: Basic time series",quote=F)
  } # end if 5 in plot or print

  # Plot 6: recruitment (not adapted for multi-area models)
  if(6 %in% c(plot, print))
   {
    x <- ts$Yr[!duplicated(ts$Yr)]
    y <- ts$Recruit_0
    if(nareas > 1){
     y <- ts$Recruit_0[ts$Area == 1]
     for(a in 2:nareas){ y <- c(y,ts$Recruit_0[ts$Area == a])
     }}
    ylab <- "Age-0 recruits (1,000s)"
    recfunc <- function(){
      if(nareas == 1){
       plot(x[3:length(x)],y[3:length(y)],xlab=lab[3],ylab=ylab,xlim=c(x[1]-1,x[length(x)]+1),ylim=c(0,max(y)),type="o",col="blue")
       points(x[1],y[1],col="blue",pch=19)
       points(x[2],y[2],col="blue")
      }
      if(nareas > 1){
       arealen <- (length(y)/nareas)
       ytemp <- y[3:arealen]
       plot(x[3:length(x)],ytemp,xlab=lab[3],ylab=ylab,xlim=c(x[1]-1,x[length(x)]+1),ylim=c(0,max(y)),type="o",col="blue")
       points(x[1],y[1],col="blue",pch=19)
       points(x[2],y[2],col="blue")
       for(a in 2:nareas)
	{
       ytemp <- y[(((a-1)*(arealen))+1):((a*arealen))]
       lines(x[3:length(x)],ytemp[3:length(ytemp)],col="blue",type="o")
       points(x[1],ytemp[1],col="blue",pch=19)
       points(x[2],ytemp[2],col="blue")
       }
       }

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
	plot(x[x<=termyr+1],y[x<=termyr+1],xlab=lab[3],ylab=ylab,xlim=range(timeseries$Yr),ylim=c(0,max(y)),type="o",col="blue")
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
      if(!"Recr_Virgin" %in% derived_quants$LABEL){
	print("Skipping recruitment with uncertainty plot because 'Recr_Virgin' not in derived quantites.",quote=F)
	print("  Try changing 'min yr for Spbio_sdreport' in starter file to -1.",quote=F)
      }else{
	recstd <- matchfun2("Recr_Virgin",0,"SPRratio",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=derived_quants,objsubset=derived_quants,substr1=TRUE,substr2=TRUE)
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
	  uiw=uiw[recstd$Yr<=maxyr],liw=liw[recstd$Yr<=maxyr],xlab=lab[3],ylo=0,
	  col="black",ylab=ylab,lty=1,main=plottitle)
	abline(h=0,col="grey")}
	if(6 %in% plot) recfunc3(maxyr=endyr+1)
	if(6 %in% print){
	png(file=paste(plotdir,"06_recswintervals.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	recfunc3(maxyr=endyr+1)
	dev.off()}

	if(forecastplot){
	recfunc4 <- function(maxyr){
	plotCI(x=recstd$Yr,y=v,sfrac=0.001,z=2,uiw=uiw,liw=liw,xlab=lab[3],ylo=0,
	  col="black",ylab=ylab,lty=1,main=plottitle)
	points(x=recstd$Yr[recstd$Yr>(endyr+1)],y=v[recstd$Yr>(endyr+1)],col="red",pch=19)
	abline(h=0,col="grey")}
	if(6 %in% plot) recfunc4(maxyr)
	if(6 %in% print){
	  png(file=paste(plotdir,"06_recswforecastintervals.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	  recfunc4(maxyr)
	  dev.off()}
	}
      } # end if no Recr_Virgin
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
      ylab <- "Spawning biomass (mt)"
      if(!SBlabelflag==TRUE){ylab <- "Spawning output (eggs)"}
      if(forecastplot) xlim=range(tsplotyr) else xlim=range(tsplotyr[noforecast])
      plot(0,xlab=lab[3], ylab=ylab,xlim=xlim, ylim=c(0,max(tsplotSB)), type="n")
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
      if(!"SPB_Virgin" %in% derived_quants$LABEL){
	print("Skipping spawning biomass with uncertainty plot because 'SPB_Virgin' not in derived quantites.",quote=F)
	print("  Try changing 'min yr for Spbio_sdreport' in starter file to -1.",quote=F)
      }else{
	xlab=lab[3]
	ylab <- "Spawning biomass (mt)"
	if(!SBlabelflag==TRUE){ylab <- "Spawning output (eggs)"}
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
	  plot(plotsbstdyr[-1],plotsbstv[-1],xlim=range(plotsbstdyr),main= plottitle,xlab=lab[3],ylab=ylab,ylim=c(0,ymax),type="o",col="blue")
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
	  dev.off()
	}

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
	      plot(plotsbstdyr[-1],plotsbstv[-1],xlim=range(plotsbstdyr,plotsbstdyr2),main= plottitle,xlab=lab[3],ylab=ylab,ylim=c(0,ymax),type="o",col="blue")
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
	  } # if year range
	} # forecastplot
      } # end if missing "SPB_virgin"
    } # if uncertainty==T
    if(verbose) print("Finished plot 7: Spawning biomass",quote=F)
  } # end if 7 in plot or print

  # Plot 8: depletion
  if(8 %in% c(plot, print))
  {
    if(nseasons > 1) print("Skipped Plot 8 (depletion) because it is not yet configured for multi-season models",quote=F)
    if(nseasons==1){ ## Temporary filter until calculations can be cleaned up
     ylab <- "Spawning depletion"
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
     depfunc <- function(iarea){
      plottitle <- NULL
      if(nareas>1) plottitle <- paste("Spawning depletion in",areanames[iarea])
      tsplotyr <- tsyears[tsarea==iarea]
      tsplotdep <- dep[tsarea==iarea]
      plot(tsplotyr[2:length(tsplotyr)],tsplotdep[2:length(tsplotdep)],xlab=lab[3],ylab=ylab,ylim=c(0,(max(dep))),type="o",col="blue",main=plottitle)
      points(tsplotyr[1],tsplotdep[1],col="blue",pch=19)
      abline(h=0,col="grey")
      addtarg()
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
	plot(depstd$Yr[depstd$period=="time"],depstd$Value[depstd$period=="time"],xlab=lab[3],ylab=ylab,ylim=c(0,ymax),type="o",col="blue")
	abline(h=0,col="grey")
	addtarg()
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
      if(nareas>1) plottitle <- paste("Spawning depletion in",areanames[iarea])
      plot(tsplotyr[2:length(tsplotyr)],tsplotdep[2:length(tsplotdep)],xlab=lab[3],
	      ylab=ylab,xlim=c(xmin,xmax),ylim=c(0,ymax),type="o",col="blue",main=plottitle)
      points(tsplotyr[1],tsplotdep[1],col="blue",pch=19)
      abline(h=0,col="grey")
      addtarg()
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
       if(nareas>1) plottitle <- paste("Spawning depletion in ",areanames[iarea])
       plot(tsplotyr[2:length(tsplotyr)],tsplotdep[2:length(tsplotdep)],xlab=lab[3],
	    ylab=ylab,xlim=c(xmin,xmax),ylim=c(0,ymax),type="o",col="blue",main=plottitle)
       points(tsplotyr[1],tsplotdep[1],col="blue",pch=19)
       abline(h=0,col="grey")
       addtarg()
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
  if(9 %in% c(plot, print)){
    if(substr(SS_version,1,8)=="SS-V3.03"){
      # temporary complaint--will remove later
      print('Skipped plot 9, recdevs: upgrade to a newer version of SS!',quote=F)
    }else{
      recdevEarly <- parameters[substring(parameters$Label,1,13) %in% c("Early_RecrDev"),]
      early_initage <- parameters[substring(parameters$Label,1,13) %in% c("Early_InitAge"),]
      main_initage <- parameters[substring(parameters$Label,1,12) %in% c("Main_InitAge"),]
      recdev <- parameters[substring(parameters$Label,1,12) %in% c("Main_RecrDev"),]
      recdevFore <- parameters[substring(parameters$Label,1,8)=="ForeRecr",]
      recdevLate <- parameters[substring(parameters$Label,1,12)=="Late_RecrDev",]


      if(nrow(recdev)==0 || max(recdev$Value)==0){
	if(verbose) print("Skipped plot 9: Rec devs and asymptotic error check - no rec devs estimated",quote=F)
      }else{
	if(nrow(recdev)>0){
	  recdev$Yr <- as.numeric(substring(recdev$Label,14))
	  if(nrow(recdevEarly)>0){
	    recdevEarly$Yr <- as.numeric(substring(recdevEarly$Label,15))
	  }
	  if(nrow(early_initage)>0){
	    early_initage$Yr <- startyr - as.numeric(substring(early_initage$Label,15))
	    recdevEarly <- rbind(early_initage,recdevEarly)
	  }
	  if(nrow(main_initage)>0){
	    main_initage$Yr <- startyr - as.numeric(substring(main_initage$Label,14))
	    recdev <- rbind(main_initage,recdev)
	  }
	  if(nrow(recdevFore)>0)
	    recdevFore$Yr <- as.numeric(substring(recdevFore$Label,10))
	  if(nrow(recdevLate)>0)
	    recdevLate$Yr <- as.numeric(substring(recdevLate$Label,14))
	  if(nrow(recdevFore)>0 & nrow(recdevLate)>0)
	    recdevFore <- rbind(recdevLate,recdevFore)

	  Yr <- c(recdevEarly$Yr,recdev$Yr,recdevFore$Yr)
	  xlim <- range(Yr,na.rm=T)
	  ylim <- range(recdevEarly$Value,recdev$Value,recdevFore$Value,na.rm=T)
	  ylab <- "Log recruitment deviation"

	  recdevfunc <- function(){
	    plot(recdev$Yr,recdev$Value,xlab=lab[3],main="",cex.main=cex.main,ylab=ylab,type="b",xlim=xlim,ylim=ylim)
	    # should probably change color between early/main not before/after startyr as now
	    if(nrow(recdevEarly)>0)
	      lines(recdevEarly$Yr,recdevEarly$Value,type='b',col='blue')
	    if(nrow(recdevFore)>0)
	      lines(recdevFore$Yr,recdevFore$Value,type='b',col='blue')
	    abline(h=0,col="black")
	  }
	  if(9 %in% plot) recdevfunc()
	  if(9 %in% print){
	    png(file=paste(plotdir,"09_recdevs.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	    recdevfunc()
	    dev.off()}
	  if(uncertainty){
	    ymax <- 1.1*max(recdev$Parm_StDev,recdevEarly$Parm_StDev,recdevFore$Parm_StDev,sigma_R_in,na.rm=T)
	    recdevfunc2 <- function(){
	      # std. dev. of recdevs
	      par(mar=par("mar")[c(1:3,2)])
	      plot(recdev$Yr,recdev$Parm_StDev,xlab=lab[3],
		   main="Recruitment deviation variance check",cex.main=cex.main,
		   ylab="Asymptotic standard error estimate",xlim=xlim,ylim=c(0,ymax),type="b")
	      if(nrow(recdevEarly)>0)
		lines(recdevEarly$Yr,recdevEarly$Parm_StDev,type="b",col="blue")
	      if(nrow(recdevFore)>0)
		lines(recdevFore$Yr,recdevFore$Parm_StDev,type="b",col="blue")
	      abline(h=0,col="grey")
	      abline(h=sigma_R_in,col="red")

	      # bias correction (2nd axis, scaled by ymax)
	      lines(recruit$year,ymax*recruit$biasadj,col="green3")
	      abline(h=ymax*1,col="green3",lty=3)
	      ypts <- pretty(0:1)
	      axis(side=4,at=ymax*ypts,label=ypts)
	      mtext("Bias adjustment fraction",side=4,line=3)
	    }
	    recdevfunc3 <- function(){
	      # std. dev. of recdevs
	      par(mar=par("mar")[c(1:3,2)])
	      plot(recdev$Yr,1-(recdev$Parm_StDev/sigma_R_in)^2,xlab=lab[3],
		   main="Bias adjustment check",cex.main=cex.main,
		   ylab="Bias adjustment fraction, 1 - stddev^2 / sigmaR^2",xlim=xlim,ylim=c(0,1.05),type="b")
	      if(nrow(recdevEarly)>0)
		lines(recdevEarly$Yr,1-(recdevEarly$Parm_StDev/sigma_R_in)^2,type="b",col="blue")
	      if(nrow(recdevFore)>0)
		lines(recdevFore$Yr,1-(recdevFore$Parm_StDev/sigma_R_in)^2,type="b",col="blue")
	      abline(h=0,col="grey")
	      abline(h=1,col="grey")

	      # bias correction (2nd axis, scaled by ymax)
	      lines(recruit$year,recruit$biasadj,col="green3")
	    }
	    recdevfunc4 <- function(){
	      # recdevs with uncertainty intervals
              alldevs <- rbind(recdevEarly,recdev,recdevFore)
              colvec <- c(rep('blue',nrow(recdevEarly)),
                          rep('black',nrow(recdev)),
                          rep('blue',nrow(recdevFore)))
              ## alldevs$Parm_StDev[is.na(alldevs$Parm_StDev)] <- 0
              val <- alldevs$Value
              std <- alldevs$Parm_StDev
              recdev_hi <- val + 1.96*std
              recdev_lo <- val - 1.96*std
              Yr <- alldevs$Yr
              ylim <- range(recdev_hi,recdev_lo,na.rm=T)
              plot(Yr,Yr,type='n',xlab="Year",
                   ylab='Log recruitment deviation',ylim=ylim)
              abline(h=0,col="grey")
              arrows(Yr,recdev_lo,Yr,recdev_hi,length=0.03,code=3,angle=90,lwd=1.2,col=colvec)
              lines(Yr,val,lty=3)
              points(Yr,val,pch=16,col=colvec)
	    }
	    if(9 %in% plot){
              recdevfunc4()
	      recdevfunc2()
	      recdevfunc3()
	    }
	    if(9 %in% print){
	      png(file=paste(plotdir,"09_recdevwithbars.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	      recdevfunc4()
	      dev.off()
	      png(file=paste(plotdir,"09_recdevvarcheck.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	      recdevfunc2()
	      dev.off()
	      png(file=paste(plotdir,"09_recdevbiasadj.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	      recdevfunc3()
	      dev.off()
	    }
	  } # rec devs
	} # end if uncertainty==T
	if(verbose) print("Finished plot 9: Rec devs and asymptotic error check",quote=F)
	flush.console()
      } # end if max(recdev)>0
    } # end if using current version of SS
  } # end if 9 in plot or print

  ### Plot 10: discard fractions (if present) ###
  if(10 %in% c(plot, print)){
    ### discard fractions ###
    if(length(discard)>1){
      for(fleetname in unique(discard$Fleet)){
	usedisc <- discard[discard$Fleet==fleetname,]
	yr <- as.numeric(usedisc$Yr)
	ob <- as.numeric(usedisc$Obs)
	cv <- as.numeric(usedisc$CV)
	liw <- -ob*cv*qt(0.025,DF_discard) # quantile of t-distribution
	uiw <- ob*cv*qt(0.975,DF_discard) # quantile of t-distribution
	liw[(ob-liw)<0] <- ob[(ob-liw)<0] # no negative limits
	xlim <- c((min(yr)-3),(max(yr)+3))
	if(discard_type=="as_biomass"){
	  title <- paste("Total discard for",fleetname)
	  ylab <- "Total discard (mt)"
	}
	if(discard_type=="as_fraction"){
	  title <- paste("Discard fraction for",fleetname)
	  ylab <- "Discard fraction"
	}
	dfracfunc <- function(){
	  plotCI(x=yr,y=ob,z=0,uiw=uiw,liw=liw,ylab=ylab,xlab=lab[3],main=title,ylo=0,yhi=1,col="red",sfrac=0.001,lty=1,xlim=xlim)
	  abline(h=0,col="grey")
	  points(yr,usedisc$Exp,col="blue",pch="-",cex=2)
	}
	if(10 %in% plot) dfracfunc()
	if(10 %in% print) {
	  png(file=paste(dir,"10discfracfit",fleetname,".png",sep=""),width=pwidth,height=pheight)
	  dfracfunc()
	  dev.off()
	}
      } # discard series
    } # if discards

    ### average body weight observations ###
    if(!is.na(mnwgt)[1]){
      for(fleetname in unique(mnwgt$Fleet)){
	usemnwgt <- mnwgt[mnwgt$Fleet==fleetname & mnwgt$Obs>0,]
	usemnwgt$Mkt <- usemnwgt$Mkt
	for(j in unique(mnwgt$Mkt)){
	  yr <- usemnwgt$Yr[usemnwgt$Mkt==j]
	  ob <- usemnwgt$Obs[usemnwgt$Mkt==j]
	  cv <- usemnwgt$CV[usemnwgt$Mkt==j]
	  ex <- usemnwgt$Exp[usemnwgt$Mkt==j]
	  xmin <- min(yr)-3
	  xmax <- max(yr)+3
	  liw <- -ob*cv*qt(0.025,DF_mnwgt) # quantile of t-distribution
	  uiw <- ob*cv*qt(0.975,DF_mnwgt) # quantile of t-distribution
	  liw[(ob-liw)<0] <- ob[(ob-liw)<0] # no negative limits
	  ymax <- max(ob + uiw)
	  ymax <- max(ymax,ex)
	  titlepart <- "discard"
	  if(j==2) titlepart <- "retained catch"
	  if(j==0) titlepart <- "whole catch"
	  ptitle <- paste("Mean weight in",titlepart,"for fleet",fleetname,sep=" ")
	  ylab <- "Mean individual body weight (kg)"
	  bdywtfunc <- function(){
	    plotCI(x=yr,y=ob,uiw=uiw,liw=liw,xlab=lab[3],main=ptitle,ylo=0,col="red",sfrac=0.001,z=ymax,ylab=ylab,lty=1,xlim=c(xmin,xmax))
	    abline(h=0,col="grey")
	    points(yr,ex,col="blue",cex=2,pch="-")}
	  if(10 %in% plot) bdywtfunc()
	  if(10 %in% print){
	    png(file=paste(plotdir,"10_bodywtfit",fleetname,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	    bdywtfunc()
	    dev.off()}
	} # market
      } # fleets
    } # if mean weight data exists
    if(verbose) print("Finished plot 10: Average body weight observations",quote=F)
    flush.console()
  } # end if 10 in plot or print

  ### Plot 11: SPR and fishing intensity plots ###
  if(11 %in% c(plot, print)){
    sprfunc <- function(){
      plot(sprseries$Year,sprseries$spr,xlab=lab[3],ylab="SPR",ylim=c(0,max(1,max(sprseries$spr[!is.na(sprseries$spr)]))),type="o",col="blue")
      if(sprtarg>0) abline(h=sprtarg,col="red",lty=2)
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
	plot(sprseries$Year,(1-sprseries$spr),xlab=lab[3],ylab="1-SPR",ylim=c(0,1),type="o",col="blue")
	if(sprtarg>0) abline(h=(1-sprtarg),col="red",lty=2)
	abline(h=0,col="grey")
	abline(h=1,col="grey")}
      if(11 %in% plot) sprfunc()
      if(11 %in% print){
	png(file=paste(plotdir,"11_1minussprseries",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	sprfunc()
	dev.off()}

      if(uncertainty & sprtarg>0){
	sprratiostd <- derived_quants[substring(derived_quants$LABEL,1,8)=="SPRratio",]
	sprratiostd$Yr <- as.numeric(substring(sprratiostd$LABEL,10))
	sprratiostd$period <- "fore"
	sprratiostd$period[sprratiostd$Yr<=(endyr)] <- "time"
	sprratiostd$upper <- sprratiostd$Value + 1.96*sprratiostd$StdDev
	sprratiostd$lower <- sprratiostd$Value - 1.96*sprratiostd$StdDev
	ylab <- managementratiolabels[1,2]
	ylim=c(0,max(1,sprratiostd$upper[sprratiostd$period=="time"]))
	sprfunc2 <- function(){
	  plot(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$Value[sprratiostd$period=="time"],xlab=lab[3],ylim=ylim,ylab=ylab,type="o",col="blue")
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
      if(btarg<=0 | sprtarg<=0){
	print("skipped SPR phase plot (in group 11) because btarg or sprtarg <= 0",quote=F)
      }else{
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
      }
      flush.console()
    } # end temporary multi-season disable of section
  } # end if 11 in plot or print

  ### Plot 12: spawner-recruit curve ###
  if(12 %in% c(plot, print)){
    recruit <- recruit[recruit$era %in% c("Main","Fixed","Late"),]
    ymax <- max(recruit$pred_recr)
    x <- recruit$spawn_bio
    xmax <- max(x)
    xlab <- "Spawning biomass (mt)"
    ylab <- "Recruitment (1,000s)"
    recruitfun <- function(){
      plot(x[order(x)],recruit$with_env[order(x)],xlab=xlab,ylab=ylab,type="l",col="blue",ylim=c(0,ymax),xlim=c(0,xmax))
      abline(h=0,col="grey")
      # lines(x[order(x)],recruit$adjusted[order(x)],col="green")
      lines(x,recruit$adjusted,col="green")
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
	  plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab=lab[3],ylo=0,col="red",ylab=lab[11],main=main,cex.main=cex.main,lty=1)
	  abline(h=0,col="grey")
	  lines(x,z,lwd=2,col="blue")
	  plot(y,z,xlab=xlab,main=main,cex.main=cex.main,ylim=c(0,max(z)),xlim=c(0,max(y)),col="blue",pch=19,ylab=lab[12])
	  abline(h=0,col="grey")
	  lines(x=c(0,max(z)),y=c(0,max(z)),col="black")
	  npoints <- length(z)
	  if(npoints > 6 & smooth){
	    psmooth <- loess(z~y,degree=1)
	    lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}}
	if(13 %in% print){
	  png(file=paste(plotdir,"13_cpuefit",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	  plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab=lab[3],ylo=0,col="red",ylab=lab[11],main=main,cex.main=cex.main,lty=1)
	  abline(h=0,col="grey")
	  lines(x,z,lwd=2,col="blue")
	  dev.off()
	  png(file=paste(plotdir,"13cpuecheck",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	  plot(y,z,xlab=xlab,main=main,cex.main=cex.main,ylim=c(0,max(z)),xlim=c(0,max(y)),col="blue",pch=19,ylab=lab[12])
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
	  plotCI(x=x,y=log(y),z=log(z),sfrac=0.001,uiw=uiw,liw=liw,xlab=lab[3],col="red",ylab=ylab,main=main,cex.main=cex.main,lty=1)
	  lines(x,log(z),lwd=2,col="blue")
	  plot(log(y),log(z),xlab=xlab,main=main,cex.main=cex.main,ylim=c(log(min(z)),log(max(z))),xlim=c(log(min(y)),log(max(y))),col="blue",pch=19,ylab=ylab2)
	  lines(x=c(log(min(z)),log(max(z))),y=c(log(min(z)),log(max(z))),col="black")
	  if(npoints & smooth){
	    psmooth <- loess(log(z)~log(y),degree=1)
	    lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
	}
	if(13 %in% print){
	  png(file=paste(plotdir,"13_logcpuefit",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	  plotCI(x=x,y=log(y),z=log(z),sfrac=0.001,uiw=uiw,liw=liw,xlab=lab[3],col="red",ylab=ylab,main=main,cex.main=cex.main,lty=1)
	  lines(x,log(z),lwd=2,col="blue")
	  dev.off()
	  png(file=paste(plotdir,"13_logcpuecheck",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	  plot(log(y),log(z),xlab=xlab,main=main,cex.main=cex.main,ylim=c(log(min(z)),log(max(z))),xlim=c(log(min(y)),log(max(y))),col="blue",pch=19,ylab=ylab2)
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
  if(14 %in% c(plot, print)){
    if(is.null(natage)){
      print("Skipped plot 14 because NUMBERS_AT_AGE unavailable in report file",quote=F)
      print("	change starter file setting for 'detailed age-structured reports'",quote=F)
    }else{
      bseas <- unique(natage$BirthSeas)
      if(length(bseas)>1) print("Numbers at age plots are for only the first birth season",quote=F)
      if(ngpatterns>0) print("Numbers at age plots are for growth pattern 1 only",quote=F)
      if(nseasons>1) print("Numbers at age plots are for season 1 only",quote=F)
      for(iarea in areas){
	for(m in 1:nsexes){
	  # warning! implementation of birthseasons may not be correct in this section
	  # data frame to combine values across factors
	  natagetemp_all <- natage[natage$Area==iarea &
				   natage$Gender==m &
				   natage$Seas==1 &
				   natage$Era!="VIRG" &
				   natage$Yr <= (endyr+1) &
				   natage$BirthSeas==min(bseas) &
				   natage$Bio_Pattern==1,]
	  # create data frame with 0 values to fill across submorphs
	  morphlist <- unique(natagetemp_all$SubMorph)
	  natagetemp0 <- natagetemp_all[natagetemp_all$SubMorph==morphlist[1],]
	  for(iage in 0:accuage) natagetemp0[,11 + iage] <- 0

	  # combining across submorphs
	  for(imorph in 1:length(morphlist)){
	    natagetemp_morph <- natagetemp_all[natagetemp_all$SubMorph==morphlist[imorph],]
	    natagetemp0[,11+0:accuage] <- natagetemp0[,11+0:accuage] + natagetemp_morph[,11+0:accuage]
	  } # end morph loop

	  nyrsplot <- nrow(natagetemp0)
	  resx <- rep(natagetemp0$Yr, accuage+1)
	  resy <- NULL
	  for(i in 0:accuage) resy <- c(resy,rep(i,nyrsplot))
	  resz <- NULL
	  for(i in 11+0:accuage) resz <- c(resz,natagetemp0[,i])

	  # assign unique name to data frame for area, sex
	  assign(paste("natagetemp0area",iarea,"sex",m,sep=""),natagetemp0)

	  if(m==1 & nsexes==1) sextitle <- ""
	  if(m==1 & nsexes==2) sextitle <- " of females"
	  if(m==2) sextitle=" of males"
	  if(nareas>1) sextitle <- paste(sextitle," in ",areanames[iarea],sep="")
	  plottitle <- paste("Expected numbers at age",sextitle," in thousands (max=",max(resz),")",sep="")

	  # calculations related to mean age
	  natagetemp1 <- as.matrix(natagetemp0[,-(1:10)])
	  ages <- 0:accuage
	  natagetemp2 <- as.data.frame(natagetemp1)
	  natagetemp2$sum <- as.vector(apply(natagetemp1,1,sum))

	  # remove rows with 0 fish (i.e. no growth pattern in this area)
	  natagetemp0 <- natagetemp0[natagetemp2$sum > 0, ]
	  natagetemp1 <- natagetemp1[natagetemp2$sum > 0, ]
	  natagetemp2 <- natagetemp2[natagetemp2$sum > 0, ]
	  prodmat <- t(natagetemp1)*ages
	  prodsum <- as.vector(apply(prodmat,2,sum))
	  natagetemp2$sumprod <- prodsum
	  natagetemp2$meanage <- natagetemp2$sumprod/natagetemp2$sum - (natagetemp0$BirthSeas-1)/nseasons
	  natageyrs <- sort(unique(natagetemp0$Yr))
	  meanage <- 0*natageyrs
	  for(i in 1:length(natageyrs)){ # averaging over values within a year (depending on birth season)
	    meanage[i] <- sum(natagetemp2$meanage[natagetemp0$Yr==natageyrs[i]]*natagetemp2$sum[natagetemp0$Yr==natageyrs[i]])/sum(natagetemp2$sum[natagetemp0$Yr==natageyrs[i]])}
	  if(m==1 & nsexes==2) meanagef <- meanage # save value for females in 2 sex models

	  ylim <- c(0,max(meanage))

	  ylab <- "Mean age (yr)"
	  plottitle1 <- "Mean age in the population"
	  tempfun <- function(){
	    # bubble plot with line
	    bubble3(x=resx, y=resy, z=resz,
		    xlab=lab[3],ylab=lab[2],col=c("black","black"),main=plottitle,maxsize=(pntscalar+1.0),
		    las=1,cex.main=cex.main,allopen=1)
	    lines(natageyrs,meanage,col="red",lwd=3)
	  }
	  tempfun2 <- function(){
	    # mean length for males and femails
	    plot(natageyrs,meanage,col="blue",lty=1,pch=4,xlab=lab[3],ylim=ylim,type="o",ylab=ylab,main=plottitle1,cex.main=cex.main)
	    points(natageyrs,meanagef,col="red",lty=2,pch=1,type="o")
	    legend("bottomleft",bty="n", c("Females","Males"), lty=c(2,1), pch=c(1,4), col = c("red","blue"))
	  }
	  if(14 %in% plot){
	    tempfun()
	    if(m==2 & nsexes==2) tempfun2()
	  }
	  if(14 %in% print){
	    filepartsex <- paste("_sex",m,sep="")
	    filepartarea <- ""
	    if(nareas > 1) filepartarea <- paste("_",areanames[iarea],sep="")
	    png(file=paste(plotdir,"14_natage",filepartarea,filepartsex,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	    tempfun()
	    dev.off()
	    # make 2-sex plot after looping over both sexes
	    if(m==2 & nsexes==2){
	      png(file=paste(plotdir,"14_meanage",filepartarea,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	      tempfun2()
	      dev.off()
	    }
	  } # end printing of plot 14
	} # end gender loop
      } # end area loop
      if(nsexes>1){
	for(iarea in areas){
	  plottitle2 <- paste("Sex ratio of numbers at age (males/females)",sep="")
	  if(nareas > 1) plottitle2 <- paste(plottitle2," for ",areanames[iarea],sep="")

	  natagef <- get(paste("natagetemp0area",iarea,"sex",1,sep=""))
	  natagem <- get(paste("natagetemp0area",iarea,"sex",2,sep=""))
	  natageratio <- as.matrix(natagem[,-(1:10)]/natagef[,-(1:10)])
	  if(diff(range(natageratio))!=0){
	    tempfun <- function(...){
	      contour(natageyrs,0:accuage,natageratio,xaxs="i",yaxs="i",xlab=lab[3],ylab=lab[2],
		      main=plottitle2,cex.main=cex.main,...)
	    }
	    if(14 %in% plot){
	      tempfun(labcex=1)
	    }
	    if(14 %in% print){
	      filepart <- ""
	      if(nareas > 1) filepart <- paste("_",areanames[iarea],filepart,sep="")
	      png(file=paste(plotdir,"14_natageratio",filepart,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	      tempfun(labcex=0.4)
	      dev.off()}
	  }else{
	    print("skipped sex ratio contour plot because ratio=1 for all ages and years",quote=F)
	  }
	} # end area loop
      } # end if nsexes>1

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
	tempfun <- function(){
	  plot(xvals,ploty,ylim=ylim,type="o",col="black",xlab=lab[13],ylab=lab[14])
	  if(n_age_error_keys > 1){
	    for(i in 2:n_age_error_keys){
	      lines(xvals,sd_vectors[i,],type="o",col=rich.colors.short(n_age_error_keys)[i])
	    } # close for n keys loop
	  } # close if more than one key statement
	  abline(h=0,col="grey") # grey line at 0
	}
	if(14 %in% plot){
	  tempfun()
	} # end if 14 in plot
	if(14 %in% print){
	  png(file=paste(plotdir,"14_ageerrorkeys.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	  tempfun()
	  dev.off()
	} # close if 14 in print
      } # end if AAK
      if(verbose) print("Finished plot 14: Numbers at age",quote=F)
      flush.console()
    }
  } # close if 14 in plot or print

  ###############################################################
  # two embedded functions for composition plots (plots 15 through 20)
  #
  #   make_multifig	  generalized function for multi-figure plots
  #   SSv3_plot_comps	  dedicated for SS composition plots
  #
  ###############################################################

  make_multifig <- function(ptsx, ptsy, yr, linesx=0, linesy=0,
			    sampsize=0, effN=0, showsampsize=T, showeffN=T, sampsizeround=1,
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
      #		 and with easier controls over some things
      # Written: Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
      # Returns: a plot
      # General:
      # Notes:
      # Required packages: none
      #
      ################################################################################

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
	    plot(x,y,type="n",xlim=xlim,main=main,cex.main=cex.main,xlab=xlab,ylab=ylab,axes=F,cex.main=cex.main)
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
      #	  number of columns and maximum number of bars within a in panel
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
	  plot(0,type="l",axes=F,xlab="",ylab="",xlim=xrange_big,ylim=yrange_big,
	       xaxs="i",yaxs=ifelse(bars,"i","r"))
	  abline(h=0,col="grey") # grey line at 0
	  if(linepos==1) lines(linesx_i,linesy_i,col=linescol,lwd=lwd,lty=lty) # lines first
	  if(diff(range(size))!=0){ # if size input is provided then use bubble function
	    bubble3(x=ptsx_i,y=ptsy_i,z=z_i,col=c(ptscol,ptscol2),
		    maxsize=maxsize,minnbubble=minnbubble,allopen=allopen,add=T) # bubble plot
	  }else{
	    if(!bars) points(ptsx_i,ptsy_i,pch=pch,col=ptscol)	# points
	    if( bars) points(ptsx_i,ptsy_i,type="h",lwd=barwidth,col=ptscol,lend=1)  # histogram-style bars
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
	      if(length(legtext_i)==1){
		if(legtext_i=="yr"){ text_i <- yr_i }	 # values in "yr" input
		if(legtext_i=="sampsize" & showsampsize){	      # sample sizes
		  vals <- unique(sampsize[yr==yr_i])
		  if(length(vals)>1){
		    print(paste("Warning: sampsize values are not all equal--choosing the first value:",vals[1]),quote=F)
		    print(paste("	  yr=",yr_i,", and all sampsize values:",paste(vals,collapse=","),sep=""),quote=F)
		    vals <- vals[1]
		  }
		  text_i <- paste("N=",round(vals,sampsizeround),sep="")
		}
		if(legtext_i=="effN" & showeffN){				      # effective sample sizes
		  vals <- unique(effN[yr==yr_i])
		  if(length(vals)>1){
		    print(paste("Warning: effN values are not all equal--choosing the first value:",vals[1]),quote=F)
		    print(paste("	  all effN values:",paste(vals,collapse=",")),quote=F)
		    vals <- vals[1]
		  }
		  text_i <- paste("effN=",round(vals,sampsizeround),sep="")
		}
	      }
	      #if(length(legtext_i)==npanels) text_i <- legtext_i[ipanel]      # one input value per panel
	      if(length(legtext_i)==nvals)   text_i <- legtext_i[yr==yr_i][1] # one input value per element
	      if(length(legtext_i)==1)	     text_i <- text_i		      # yr, sampsize, or effN

	      if(legx[1]=="default"){
		# default is left side for first plot, right thereafter
		textx <- ifelse(i==1, usr[1], usr[2])
	      }else{ textx <- legx[i] }
	      if(legy[1]=="default"){
		texty <- usr[4]		# default is top for all plots
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
	  if(mfg[2]==1) axis(side=2,at=axis2,las=horiz_lab)	   # axis on left side panels
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
      par(mfcol=c(rows,cols),mar=c(5,5,4,2)+.1,oma=rep(0,4))

      # return information on what was plotted
      return(list(npages=npages, npanels=npanels, ipage=ipage))
    } # end embedded function: make_multifig

  SSv3_plot_comps <- function(
			      kind="LEN", aalyear=-1, aalbin=-1, GUI=T, png=F, plotdir=NA, fleets="all",
			      datonly=F, Natageplot=T, samplesizeplots=T, compresidplots=T, bub=F, showsampsize=T, showeffN=T,
			      minnbubble=8, pntscalar=2.6, pwidth=7, pheight=7, punits="in", ptsize=12, res=300, cex.main=1,
			      linepos=1, fitbar=F,maxsize=3,do.sqrt=TRUE,smooth=TRUE,cohortlines=c(),
			      agelab=lab[2], lenlab=lab[1],proplab=lab[17],yearlab=lab[3], lenunits=lab[18],
			      osslab=lab[15],esslab=lab[16],printmkt=T,printsex=T,
			      maxrows=6,maxcols=6,maxrows2=2,maxcols2=4,fixdims=T,maxneff=5000,returntitles=T,verbose=T,...)
    {
      ################################################################################
      #
      # SSv3_plot_comps BETA December 4, 2009
      # This function comes with no warranty or guarantee of accuracy
      #
      # Purpose: test subset of SSv3_plots to show compositional data with or without fits
      # Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
      #		 Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
      # Returns: Plots with plot history in R GUI and/or .png files.
      # General: Updated for Stock Synthesis version 3.04 September, 2009; R version 2.8.1
      # Notes:	 See users guide for documentation: http://code.google.com/p/r4ss/wiki/Documentation
      # Required SS3v_output function
      #
      ################################################################################
      if(!exists("make_multifig"))
	print("you are missing the function 'make_mulitifig', which should have been embedded within this file SSv3_plots.R")

      titles <- NULL
      if(png) if(is.na(plotdir)) return("plotdir must be specified to write png files.")

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
	kindlab=lab[1]
	if(datonly){
	  filenamestart <- "15_lendat_"
	  titledata <- "length comp data, "
	}else{
	  filenamestart <- "18_lenfit_"
	  titledata <- "length comps, "
	}
      }
      if(kind=="SIZE"){
	dbase_kind <- sizedbase
	kindlab=lab[20]
	if(datonly){
	  filenamestart <- "15_sizedat_"
	  titledata <- "size comp data, "
	}else{
	  filenamestart <- "18_sizefit_"
	  titledata <- "size comps, "
	}
      }
      if(kind=="AGE"){
	dbase_kind <- agedbase
	kindlab=lab[2]
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
	kindlab=lab[2]
	if(datonly){
	  filenamestart <- "17_condAALdat_"
	  titledata <- "conditional age at length data, "
	}else{
	  filenamestart <- "20_condAALfit_"
	  titledata <- "conditional age at length, "
	}
      }

      if(kind=="GSTAGE"){
	dbase_kind <- ghostagedbase
	kindlab=lab[2]
	if(datonly){
	  filenamestart <- "16_gstagedat_"
	  titledata <- "gst age comp data, "
	}else{
	  filenamestart <- "19_gstagefit_"
	  titledata <- "gst age comps, "
	}
      }
      if(!(kind%in%c("LEN","SIZE","AGE","cond","GSTAGE"))) return("Input 'kind' to SSv3_plot_comps should be 'LEN' or 'AGE'.")
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
			  if(kind!="GSTAGE"){
			      make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
					    sampsize=dbase$N,effN=dbase$effN,showsampsize=showsampsize,showeffN=showeffN,
					    bars=bars,linepos=(1-datonly)*linepos,
					    nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
					    main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=proplab,
					    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,ipage=ipage,...)
			  }
			  if(kind=="GSTAGE"){
			      make_multifig(ptsx=dbase$Bin,ptsy=dbase$Obs,yr=dbase$Yr,linesx=dbase$Bin,linesy=dbase$Exp,
					    sampsize=dbase$N,effN=dbase$effN,showsampsize=F,showeffN=F,
					    bars=bars,linepos=(1-datonly)*linepos,
					    nlegends=3,legtext=list(dbase$YrSeasName,"sampsize","effN"),
					    main=ptitle,cex.main=cex.main,xlab=kindlab,ylab=proplab,
					    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,ipage=ipage,...)
			  }
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

		      tempfun <- function(){
			  bubble3(x=dbase$Yr, y=dbase$Bin, z=z, xlab=yearlab,ylab=kindlab,col=col,
				  las=1,main=ptitle,cex.main=cex.main,maxsize=pntscalar,allopen=allopen,minnbubble=minnbubble)
			  # add lines for growth of individual cohorts if requested
			  if(length(cohortlines)>0){
			      for(icohort in 1:length(cohortlines)){
				  print(paste("Adding line for",cohortlines[icohort],"cohort"),quote=F)
				  if(k %in% c(1,2)) lines(growdatF$Age+cohortlines[icohort],growdatF$Len_Mid, col="red")  #females
				  if(k %in% c(1,3)) lines(growdatM$Age+cohortlines[icohort],growdatM$Len_Mid, col="blue") #males
			      }
			  }
		      }

		      if(GUI) tempfun()
		      if(png){ # set up plotting to png file if required
			  filename <- paste(plotdir,filenamestart,filetype,filename_fltsexmkt,".png",sep="")
			  png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
			  tempfun()
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
			  make_multifig(ptsx=dbase$Bin,ptsy=dbase$Lbin_mid,yr=dbase$Yr,size=z,
					sampsize=dbase$N,showsampsize=showsampsize,showeffN=F,
					nlegends=1,legtext=list(dbase$YrSeasName),
					bars=F,linepos=0,main=ptitle,cex.main=cex.main,
					xlab=lab[2],ylab=lab[1],ymin0=F,maxrows=maxrows2,maxcols=maxcols2,
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
		  #			and Pearson residuals of A-L key for specific years
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
			      lenbinlegend[ydbase$Lbin_range>0] <- paste(ydbase$Lbin_lo,"-",ydbase$Lbin_hi,lenunits,sep="")
			      tempfun <- function(ipage,...){ # temporary function to aid repeating the big function call
				  make_multifig(ptsx=ydbase$Bin,ptsy=ydbase$Obs,yr=ydbase$Lbin_lo,
						linesx=ydbase$Bin,linesy=ydbase$Exp,
						sampsize=ydbase$N,effN=ydbase$effN,showsampsize=showsampsize,showeffN=showeffN,
						nlegends=3,legtext=list(lenbinlegend,"sampsize","effN"),
						bars=F,linepos=linepos,main=ptitle,cex.main=cex.main,
						xlab=lab[2],ylab=proplab,maxrows=maxrows,maxcols=maxcols,
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
				  bubble3(x=ydbase$Bin,y=ydbase$Lbin_lo,z=z,xlab=lab[2],ylab=lab[1],col=rep("blue",2),
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
		  #		 for specific length bins
		  if(aalbin[1] > 0)
		  {
		      badbins <- setdiff(aalbin, dbase$Lbin_hi)
		      goodbins <- intersect(aalbin, dbase$Lbin_hi)
		      if(length(badbins)>0){
			  print(paste("Error! the following inputs for 'aalbin' do not match the Lbin_hi values for the conditional age at length data:",badbins),quote=F)
			  print(paste("	      the following inputs for 'aalbin' are fine:",goodbins),quote=F)
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
						    sampsize=abindbase$N,effN=abindbase$effN,showsampsize=showsampsize,showeffN=showeffN,
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
		  if(samplesizeplots & !datonly & kind!="GSTAGE")
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
				   col="blue",pch=19,ylab=esslab,xaxs="i",yaxs="i")
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

  ### process composition data
  if(length(intersect(c(15:21,24),c(plot,print)))>0){
    # configure seasons
    if(nseasons>1) compdbase$YrSeasName <- paste(floor(compdbase$Yr),"s",compdbase$Seas,sep="") else compdbase$YrSeasName <- compdbase$Yr

    # deal with Lbins
    compdbase$Lbin_range <- compdbase$Lbin_hi - compdbase$Lbin_lo
    compdbase$Lbin_mid <- 0.5*(compdbase$Lbin_lo + compdbase$Lbin_hi)

    # divide into objects by kind
    lendbase	   <- compdbase[compdbase$Kind=="LEN" & compdbase$N > 0,]
    sizedbase	   <- compdbase[compdbase$Kind=="SIZE" & compdbase$N > 0,]
    agedbase	   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0
				& !is.na(compdbase$Lbin_range) & compdbase$Lbin_range > aalmaxbinrange,]
    condbase	   <- compdbase[compdbase$Kind=="AGE" & compdbase$N > 0
				& !is.na(compdbase$Lbin_range) & compdbase$Lbin_range <= aalmaxbinrange,]
    ghostagedbase  <- compdbase[compdbase$Kind=="AGE" & compdbase$N < 0
				& !is.na(compdbase$Lbin_range) & compdbase$Lbin_range > aalmaxbinrange,]
    tagdbase1	   <- compdbase[compdbase$Kind=="TAG1",]
    tagdbase2	   <- compdbase[compdbase$Kind=="TAG2",]
    # consider range of bins for conditional age at length data
    print(paste("CompReport file separated by this code as follows (rows = num. comps * num. bins):"),quote=F)
    print(paste("  ",nrow(lendbase),"rows of length comp data,"),quote=F)
    print(paste("  ",nrow(sizedbase),"rows of generalized size comp data,"),quote=F)
    print(paste("  ",nrow(agedbase),"rows of age comp data,"),quote=F)
    print(paste("  ",nrow(condbase),"rows of conditional age-at-length data, and"),quote=F)
    print(paste("  ",nrow(ghostagedbase),"rows of ghost fleet age comp data"),quote=F)
    print(paste("  ",nrow(tagdbase1),"rows of 'TAG1' comp data"),quote=F)
    print(paste("  ",nrow(tagdbase2),"rows of 'TAG2' comp data"),quote=F)
    Lbin_ranges <- as.data.frame(table(agedbase$Lbin_range))
    names(Lbin_ranges)[1] <- "Lbin_hi-Lbin_lo"
    if(length(unique(agedbase$Lbin_range)) > 1){
      print("Warning!: different ranges of Lbin_lo to Lbin_hi found in age comps.",quote=F)
      print(Lbin_ranges)
      print("  consider increasing 'aalmaxbinrange' to designate",quote=F)
      print("  some of these data as conditional age-at-length",quote=F)
    }
    # convert bin indices to true lengths

    # don't remember why these are not converted to numeric in SSv3_output, nor why they aren't done in 1 step to compdbase
    lendbase$effN  <- as.numeric(lendbase$effN)
    sizedbase$effN <- as.numeric(sizedbase$effN)
    agedbase$effN  <- as.numeric(agedbase$effN)
    condbase$effN  <- as.numeric(condbase$effN)
  }
  # now make use of embedded SSv3_plot_comps function to make composition plots
  if(!datplot)
    {
      if(length(intersect(15:17,c(plot,print)))>0)
	print("skipped data-only plots 15-17 (comp data without fit) because input 'datplot=F'",quote=F)
    }else{
      if(15 %in% c(plot,print))	 # data only aspects
	{
	  # length comp bar plot
	  SSv3_plot_comps(datonly=T,kind="LEN",bub=F,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  # length comp bubble plot
	  SSv3_plot_comps(datonly=T,kind="LEN",bub=T,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,cohortlines=cohortlines,...)
	  # size comp bar plot
	  SSv3_plot_comps(datonly=T,kind="SIZE",bub=F,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  # size comp bubble plot
	  SSv3_plot_comps(datonly=T,kind="SIZE",bub=T,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,cohortlines=cohortlines,...)
	  if(verbose) print("Finished plot 15: length and size comp data",quote=F)
	  flush.console()
	}
      if(16 %in% c(plot,print))
	{
	  # age comp bar plot
	  SSv3_plot_comps(datonly=T,kind="AGE",bub=F,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  # age comp bubble plot
	  SSv3_plot_comps(datonly=T,kind="AGE",bub=T,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  # ghost age comp bar plot
	  SSv3_plot_comps(datonly=T,kind="GSTAGE",bub=F,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=F,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  # ghost age comp bubble plot
	  SSv3_plot_comps(datonly=T,kind="GSTAGE",bub=T,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=F,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  if(verbose) print("Finished plot 16: age comp data",quote=F)
	  flush.console()
	}
      if(17 %in% c(plot,print))
	{
	  # conditional age plot
	  SSv3_plot_comps(datonly=T,kind="cond",bub=T,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,
			  fixdims=fixdims,
			  png=(17%in%print),GUI=(17%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  if(verbose) print("Finished plot 17: conditional age at length data",quote=F)
	  flush.console()
	}
    } # end if datplot

  # plot 18: length comp data with fits, sample size, etc.
  if(18 %in% c(plot,print)){
    SSv3_plot_comps(datonly=F,kind="LEN",bub=T,verbose=verbose,fleets=fleets,
		    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
		    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
		    png=(18%in%print),GUI=(18%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,...)
    SSv3_plot_comps(datonly=F,kind="SIZE",bub=T,verbose=verbose,fleets=fleets,
		    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
		    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
		    png=(18%in%print),GUI=(18%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,...)
    if(verbose) print("Finished plot 18: length and size comps with fits",quote=F)
    flush.console()
  }

  # plot 19: age comp data with fits, sample size, etc.
  if(19 %in% c(plot,print)){
    SSv3_plot_comps(datonly=F,kind="AGE",bub=T,verbose=verbose,fleets=fleets,
		    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
		    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
		    png=(19%in%print),GUI=(19%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,...)
    SSv3_plot_comps(datonly=F,kind="GSTAGE",bub=T,verbose=verbose,fleets=fleets,
		    samplesizeplots=F,showsampsize=F,showeffN=F,
		    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
		    png=(19%in%print),GUI=(19%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,...)
    if(verbose) print("Finished plot 19: age comps with fits",quote=F)
    flush.console()
  } # end if 19 in plot or print

  # plot 20: conditional age at length plot with fits, sample size, etc.
  if(20 %in% c(plot,print)){
if(3==4){
    SSv3_plot_comps(datonly=F,kind="cond",bub=T,verbose=verbose,fleets=fleets,
		    aalbin=aalbin,aalyear=aalyear,
		    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
		    maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,
		    png=(20%in%print),GUI=(20%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,...)

    if(verbose) print("Finished plot 20a: conditional age at length with fits",quote=F)
  }
    # more plot 20: Andre's new conditional age-at-length plots
    if(nrow(condbase)==0){
      if(verbose) print("Skipped plot 20b: mean age and std. dev. in conditional AAL: no data of this type",quote=F)
    }else{
      Lens <-sort(unique(condbase$Lbin_lo))
      Yrs <- sort(unique(condbase$Yr))
      par(mfrow=c(2,2))
      for (Gender in 1:2){
	for (Yr in Yrs){
	  y <- condbase[condbase$Yr==Yr & condbase$Gender==Gender,]
	  Size <- NULL; Size2 <- NULL
	  Obs <- NULL; Obs2 <- NULL
	  Pred <- NULL;	 Pred2 <- NULL
	  Upp <- NULL; Low <- NULL; Upp2 <- NULL; Low2 <- NULL
	  for (Ilen in Lens){
	    z <- y[y$Lbin_lo == Ilen & y$Lbin_hi == Ilen,]
	    if (length(z[,1]) > 0){
	      weightsPred <- z$Exp/sum(z$Exp)
	      weightsObs <- z$Obs/sum(z$Obs)
	      ObsV <- sum(z$Bin*weightsObs)
	      ObsV2 <- sum(z$Bin*z$Bin*weightsObs)
	      PredV <- sum(z$Bin*weightsPred)
	      PredV2 <- sum(z$Bin*z$Bin*weightsPred)
	      # Overdispersion on N
	      # NN <- z$N[1]*0.01 # Andre did this for reasons unknown
	      NN <- z$N[1]
	      if (max(z$Obs) > 1.0e-4){
		Size <- c(Size,Ilen)
		Obs <- c(Obs,ObsV)
		Pred <- c(Pred,PredV)
		varn <-sqrt(PredV2-PredV*PredV)/sqrt(NN)
		Pred2 <- c(Pred2,varn)
		varn <-sqrt(ObsV2-ObsV*ObsV)/sqrt(NN)
		Obs2 <- c(Obs2,varn)
		Low <- c(Low,ObsV-1.64*varn)
		Upp <- c(Upp,ObsV+1.64*varn)
		if (NN > 1){
		  Size2 <- c(Size2,Ilen)
		  Low2 <- c(Low2,varn*sqrt((NN-1)/qchisq(0.95,NN)))
		  Upp2 <- c(Upp2,varn*sqrt((NN-1)/qchisq(0.05,NN)))
		}
	      }
	    }
	  }
	  if (length(Obs) > 0){
	    ymax <- max(Pred,Obs,Upp)*1.1
	    plot(Size,Obs,xlab="Size (cm)",ylab=lab[2],pch=16,xlim=c(min(Lens),max(Lens)),ylim=c(0,ymax),yaxs="i")
	    lines(Size,Pred)
	    lines(Size,Low,lty=3)
	    lines(Size,Upp,lty=3)
	    title(paste("Year = ",Yr,"; Gender = ",Gender))

	    ymax <- max(Obs2,Pred2)*1.1
	    plot(Size,Obs2,xlab="Size (cm)",ylab="Stdev (Age) (yr)",pch=16,xlim=c(min(Lens),max(Lens)),ylim=c(0,ymax),yaxs="i")
	    lines(Size,Pred2)
	    lines(Size2,Low2,lty=3)
	    lines(Size2,Upp2,lty=3)
	  }
	} # end loop over years
      } # end loop over genders
      if(verbose) print("Finished plot 20b: mean age and std. dev. in conditional AAL",quote=F)
      if(verbose) print("  This is a new plot, currently in beta mode.",quote=F)
      if(verbose) print("  Left plots are mean AAL by size-class (obs. and pred.)",quote=F)
      if(verbose) print("  with 90% CIs based on adding 1.64 SE of mean to the data",quote=F)
      if(verbose) print("  Right plots in each pair are SE of mean AAL (obs. and pred.)",quote=F)
      if(verbose) print("  with 90% CIs based on the chi-square distribution.",quote=F)
      flush.console()
    }
  } # end if 20 in plot or print

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
		  #plot(la$N,la$effN,xlab=lab[15],main=plottitle,ylim=c(0,ymax),xlim=c(0,xmax),col="blue",pch=19,ylab=lab[16])
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
				  sampsize=la$N,effN=0,showsampsize=showsampsize,showeffN=F,
				  nlegends=3,legtext=list(la$YrSeasName,"sampsize","effN"),
				  bars=F,linepos=1,
				  main=plottitle,cex.main=cex.main,xlab=lab[2],ylab=lab[1],
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
		    bubble3(x=la$Yr,y=la$Bin,z=la$Pearson,xlab=lab[3],ylab=lab[2],col=rep("blue",2),
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

  # restore default single panel settings
  par(mfcol=c(rows,cols),mar=c(5,5,4,2)+.1,oma=rep(0,4))

  # Yield curve
  if(22 %in% c(plot, print)){
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

    if(nareas==1){
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
	old_warn <- options()$warn	# previous setting
	options(warn=-1)		# turn off "zero-length arrow" warning
	s <- seq(length(sprod_good)-1)
	arrows(Bio_all_good[s],sprod_good[s],Bio_all_good[s+1],sprod_good[s+1],length=0.06,angle=20,col="black",lwd=1.2)
	options(warn=old_warn)	#returning to old value

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
  if(23 %in% c(plot, print)){
    if(!datplot){
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
	  plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab=lab[3],ylo=0,col="red",ylab=lab[11],main=main,cex.main=cex.main,lty=1)
	  abline(h=0,col="grey")}
	if(23 %in% print){
	  png(file=paste(plotdir,"23_cpue_dataonly",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	  plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab=lab[3],ylo=0,col="red",ylab=lab[11],main=main,cex.main=cex.main,lty=1)
	  abline(h=0,col="grey")
	  dev.off()}
      } # nfleets
      if(verbose) print("Finished plot 23: CPUE data plots",quote=F)
      flush.console()
    } # end if datplot
  } # end if 23 in plot or print

  ### Plot 24: Tag plots ###
  if(24 %in% c(plot, print)){
    if(nrow(tagdbase2)==0){
      print("skipped plots 24 (tags) because there's no tag-related data'",quote=F)
    }else{
      # calculations needed for printing to multiple PNG files
      grouprange <- unique(tagdbase2$Rep)
      ngroups <- length(unique(tagdbase2$Rep))
      npages <- ceiling(ngroups/(tagrows*tagcols))

      tagfun1 <- function(ipage=0){
	# obs & exp recaps by tag group
	par(mfcol=c(tagrows,tagcols),mar=c(2.5,2.5,2,1),cex.main=cex.main,oma=c(2,2,2,0))
	if(npages > 1 & ipage!=0) grouprange <- intersect(grouprange, 1:(tagrows*tagcols) + tagrows*tagcols*(ipage-1))
	for(igroup in grouprange){
	  tagtemp <- tagdbase2[tagdbase2$Rep==igroup,]
	  ylim=c(0,max(5,cbind(tagtemp$Obs,tagtemp$Exp)*1.05))
	  plot(0,type="n",xlab="",ylab="",ylim=ylim,main=paste("TG ",igroup,sep=""),
	       xaxs="i",yaxs="i",xlim=c(min(tagtemp$Yr)-0.5,max(tagtemp$Yr)+0.5))
	  for (iy in 1:length(tagtemp$Yr)){
	    xx <- c(tagtemp$Yr[iy]-0.5,tagtemp$Yr[iy]-0.5,tagtemp$Yr[iy]+0.5,tagtemp$Yr[iy]+0.5)
	    yy <- c(0,tagtemp$Obs[iy],tagtemp$Obs[iy],0)
	    polygon(xx,yy,col="grey80")
	  }
	  points(tagtemp$Yr,tagtemp$Exp,type="o",lty=1,pch=16)

	  # add labels in left and lower outer margins once per page
	  mfg <- par("mfg")
	  if(mfg[1]==1 & mfg[2]==1){
	    mtext(lab[3],side=1,line=0,outer=T)
	    mtext(lab[19],side=2,line=0,outer=T)
	    mtext("Fit to tag recaptures by tag group",side=3,line=0,outer=T,cex=cex.main,font=2)
	  }
	}

	# restore default single panel settings
	par(mfcol=c(rows,cols),mar=c(5,5,4,2)+.1,oma=rep(0,4))
      }

      print("Calculated tagging related quantities...",quote=F)
      # reconfiguring tagdbase2
      # why? to exclude the first year for each group?
      XRep <- -1
      x <- NULL
      for (irow in 1:length(tagdbase2[,1])){
	if (tagdbase2$Rep[irow] != XRep){
	  XRep <- tagdbase2$Rep[irow]
	}else{
	  x <- rbind(x,tagdbase2[irow,])
	}
      }
      # alternatively, don't reconfigure by using:
      #x <- tagdbase

      #obs vs exp tag recaptures by year aggregated across group
      tagobs <- aggregate(x$Obs,by=list(x$Yr,x$Rep),FUN=sum,na.rm=T)
      tagexp <- aggregate(x$Exp,by=list(x$Yr,x$Rep),FUN=sum,na.rm=T)
      Recaps <- data.frame(Yr=tagobs[,1],Group=tagobs[,2],Obs=tagobs[,3],Exp=tagexp[,3])

      xlim <- range(Recaps[,1])
      xx2 <- aggregate(Recaps[,3],by=list(Recaps$Yr),FUN=sum,na.rm=T)
      xx3 <- aggregate(Recaps[,4],by=list(Recaps$Yr),FUN=sum,na.rm=T)
      RecAg <- data.frame(Yr=xx2[,1],Obs=xx2[,2],Exp=xx3[,2])

      tagfun2 <- function(){
	#obs vs exp tag recaptures by year aggregated across group
	plot(0,xlim=xlim+c(-0.5,0.5),ylim=c(0,max(RecAg[,2],RecAg[,3])*1.05),type="n",xaxs="i",yaxs="i",
	     xlab=lab[3],ylab=lab[19],main="Tag recaptures aggregated across tag groups",cex.main=cex.main)
	for (iy in 1:nrow(RecAg)){
	  xx <- c(RecAg[iy,1]-0.5,RecAg[iy,1]-0.5,RecAg[iy,1]+0.5,RecAg[iy,1]+0.5)
	  yy <- c(0,RecAg[iy,2],RecAg[iy,2],0)
	  polygon(xx,yy,col="grey80")
	}
	lines(RecAg[,1],RecAg[,3],type="o",pch=16,lty=1,lwd=2)
      }

      Recaps$Pearson <- (Recaps$Obs-Recaps$Exp)/sqrt(Recaps$Exp)
      tagfun3 <- function(){
	# bubble plot of observed recapture data
	plottitle <- "Observed tag recaptures by year and tag group"
	bubble3(x=Recaps$Yr,y=Recaps$Group,z=Recaps$Obs,xlab=lab[3],ylab="Tag Group",col=rep("blue",2),
		las=1,main=plottitle,cex.main=cex.main,maxsize=pntscalar,allopen=F,minnbubble=minnbubble)
      }
      tagfun4 <- function(){
	# bubble plot of residuals
	plottitle <- "Residuals for tag recaptures: (obs-exp)/sqrt(exp)"
	bubble3(x=Recaps$Yr,y=Recaps$Group,z=Recaps$Pearson,xlab=lab[3],ylab="Tag Group",col=rep("blue",2),
		las=1,main=plottitle,cex.main=cex.main,maxsize=pntscalar,allopen=F,minnbubble=minnbubble)
      }
      tagfun5 <- function(){
	# line plot by year and group
	plottitle <- "Observed tag recaptures by year and tag group"
	plot(0,type='n',xlim=range(Recaps$Yr),ylim=range(Recaps$Group),xlab=lab[3],ylab="Tag Group",
	     main=plottitle,cex.main=cex.main)
	rescale <- 5/max(Recaps$Obs,Recaps$Exp)
	for(igroup in sort(unique(Recaps$Group))){
	  lines(Recaps$Yr[Recaps$Group==igroup],igroup+0*Recaps$Obs[Recaps$Group==igroup],col='grey',lty=3)
	  points(Recaps$Yr[Recaps$Group==igroup],igroup+rescale*Recaps$Obs[Recaps$Group==igroup],type='o',pch=16,cex=.5)
	  lines(Recaps$Yr[Recaps$Group==igroup],igroup+rescale*Recaps$Exp[Recaps$Group==igroup],col='red',lty='51',lwd=2)
	}
      }
      if(24 %in% plot){
	tagfun1()
	tagfun2()
	tagfun3()
	tagfun4()
      }
      if(24 %in% print){
	filenamestart <- "24_tags_by_group"
	for(ipage in 1:npages){
	  if(npages>1) pagetext <- paste("_page",ipage,sep="") else pagetext <- ""
	  filename <- paste(plotdir,filenamestart,pagetext,".png",sep="")
	  png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	  tagfun1(ipage=ipage,...)
	  dev.off() # close device if png
	}
	filename <- paste(plotdir,"24_tags_aggregated.png",sep="")
	png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	tagfun2()
	dev.off()
	filename <- paste(plotdir,"24_tags_data_bubbleplot.png",sep="")
	png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	tagfun3()
	dev.off()
	filename <- paste(plotdir,"24_tags_residuals.png",sep="")
	png(file=filename,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	tagfun4()
	dev.off()
      }
      if(verbose) print("Finished plot 24: tags",quote=F)
      flush.console()
    } # end if tag data present
  } # end if 24 in plot or print
  if(pdf) dev.off() # close PDF file if it was open
  if(verbose) print("Finished all requested plots",quote=F)
  ### end of SSv3_plots function
}

