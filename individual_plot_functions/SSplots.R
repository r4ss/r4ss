SSplots <-
function(
    replist="ReportObject", plot=1:24, print=0, pdf=FALSE, printfolder="", dir="default", fleets="all", areas="all",
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
# SSv3_plots
# This function comes with no warranty or guarantee of accuracy
#
# Purpose: To sumarize the results of an SSv3 model run.
# Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
#	   Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
#	   and other contributors to http://code.google.com/p/r4ss/
# Returns: Plots with plot history in R GUI and/or .png files.
# General: Updated for Stock Synthesis version 3.10; R version 2.8.1
# Notes:   See users guide for documentation.
# Required SS3v_output function and plotrix package
# Credit:  Based loosely on an early version of "Scape" (A. Magnusson) and "Output viewer" (R. Methot)
#
################################################################################

  codedate <- "June 4, 2010"

  if(verbose){
    print(paste("R function updated:",codedate),quote=FALSE)
    print("Check for new code and report problems at http://code.google.com/p/r4ss/",quote=FALSE)
  }

  flush.console()

  # label table is a step toward internationalization of the code
  # in the future, this could be read from a file, or we could have multiple columns
  # in the table to choose from
  labeltable = as.data.frame(matrix(c(
    1,	 "Length (cm)",
    2,	 "Age (yr)",
    3,	 "Year",
    4,	 "Maturity",
    5,	 "Selectivity",
    6,	 "Landings",
    7,	 "Total catch",
    8,	 "Predicted Discards", # should add units
    9,	 "Discard fraction", # need to add by weight or by length
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
    20,	  "Weight (lbs)",
    21,   "(mt)",
    22,   "(numbers x1000)"
    ),22,2,byrow=TRUE))
  lab = as.character(labeltable[,2])
  if(catchasnumbers){
    lab[6] <- paste(lab[6],lab[22])
    lab[7] <- paste(lab[7],lab[22])
  }else{
    lab[6] <- paste(lab[6],lab[21])
    lab[7] <- paste(lab[7],lab[21])
  }

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
  if(uncertainty==TRUE & inputs$covar==FALSE){
    print("To use uncertainty=T, you need to have covar=T in the input to the SSv3_output function",quote=FALSE)
    return()
  }
  if(forecastplot==TRUE & inputs$forecast==FALSE){
    print("To use forecastplot=T, you need to have forecast=T in the input to the SSv3_output function",quote=FALSE)
    return()
  }
  if(forecastplot==TRUE & max(timeseries$Yr > ex2$endyr+1)==0){
    print("Changeing 'forecastplot' input to FALSE because all years up to endyr+1 are included by default",quote=FALSE)
    forecastplot <- FALSE
  }

  # derived quantities
  mainmorphs <- morph_indexing$Index[morph_indexing$Bseas==1 & morph_indexing$Sub_Morph_Dist==max(morph_indexing$Sub_Morph_Dist)]
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

  if(verbose) print("Finished defining objects",quote=FALSE)
  if(nareas>1){
    print(paste("! Warning: some plots are not configured for mult-area models (nareas=",nareas,")",sep=""),quote=FALSE)
    if(areanames[1]=="default") areanames <- paste("area",1:nareas)
  }

  #### prepare for plotting
  # make plot window (operating system specific)
  nplots <- length(intersect(1:25,plot))
  nprints <- length(intersect(1:25,print))

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
    plotdir <- NULL
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
    pdffile <- paste(inputs$dir,"/SSplots_",format(Sys.time(),'%d-%b-%Y_%H.%M' ),".pdf",sep="")
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

  #### plots 1 (now contains 2 as well)
  # Static growth (mean weight, maturity, fecundity, spawning output)
  # and Time-varying growth
  if(1 %in% c(plot, print) | length(cohortlines)>0)
  {
    SBlabelflag <-
      SSplotBiology(replist=replist,
                    plot=(1 %in% plot),print=(1 %in% print),
                    cex.main=cex.main)
    if(verbose) print("Finished plot 1: Biology (weight, maturity, spawning output, growth)",quote=FALSE)
  }

  ### plots 3 and 4 selectivity and retention
  # Length selex and retention
  if(length(intersect(c(3,4), c(plot,print)))>0)
  {
    SSplotSelex(replist=replist, selexlines=selexlines,
                plot=(3 %in% plot), print=(3 %in% print),
                cex.main=cex.main)
    if(verbose) print("Finished plots 3 and 4: selectivity plots",quote=FALSE)
  }


  ### plot 5: Basic time series (contains what used to be plot 7)
  # stats and dimensions
  if(5 %in% c(plot, print))
  {
    for(isubplot in 1:10){ # which of 10 subplots to make
      for(doforecast in unique(c(FALSE,forecastplot))){ # add forecast or not
        for(douncertainty in unique(c(FALSE,uncertainty))){ # add uncertainty or not
          SSplotTimeseries(replist=replist,
                           subplots=isubplot,
                           forecast=doforecast,
                           uncertainty=douncertainty,
                           plot=(5 %in% plot),
                           print=(5 %in% print),
                           SBlabelflag=SBlabelflag,
                           verbose=verbose)
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

  # Plot 7: recruitment
  if(7 %in% c(plot, print)){
    SSplotRecdevs(replist=replist,
                  plot=(7 %in% plot),
                  print=(7 %in% print),
                  forecastplot=forecastplot,
                  uncertainty=uncertainty)
  } # end if 7 in plot or print


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
    for(i in unique(cpue$Fleet)){
      cpueuse <- cpue[cpue$Obs > 0 & cpue$Fleet==i,]
      x <- cpueuse$Yr
      y <- cpueuse$Obs
      z <- cpueuse$Exp
      uiw <- qlnorm(.975,meanlog=log(y),sdlog=cpueuse$SE) - y
      liw <- y - qlnorm(.025,meanlog=log(y),sdlog=cpueuse$SE)
      npoints <- length(z)
      main=paste("Index ", i,sep="")
      xlab <- "Observed index"
      cpuefun1 <- function(){
	plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab=lab[3],ylo=0,col="red",ylab=lab[11],main=main,cex.main=cex.main,lty=1)
	abline(h=0,col="grey")
	lines(x,z,lwd=2,col="blue")
      }
      cpuefun2 <- function(){
	plot(y,z,xlab=xlab,main=main,cex.main=cex.main,ylim=c(0,max(z)),xlim=c(0,max(y)),col="blue",pch=19,ylab=lab[12])
	abline(h=0,col="grey")
	lines(x=c(0,max(z)),y=c(0,max(z)))
	if(smooth && npoints > 6 && diff(range(y))>0){
	  psmooth <- loess(z~y,degree=1)
	  lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
      }
      if(13 %in% plot){
	cpuefun1()
	cpuefun2()
      }
      if(13 %in% print){
	png(file=paste(plotdir,"13_cpuefit",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	cpuefun1()
	dev.off()
	png(file=paste(plotdir,"13cpuecheck",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	cpuefun2()
	dev.off()
      }

      # same plots again in log space
      ylab <- "Log index"
      main <- paste("Log index ", i,sep="")
      xlab <- "Log observed index"
      ylab2 <- "Log expected index"
      uiw <- qnorm(.975,mean=log(y),sd=cpueuse$SE) - log(y)
      liw <- log(y) - qnorm(.025,mean=log(y),sd=cpueuse$SE)
      cpuefun3 <- function(){
	plotCI(x=x,y=log(y),z=log(z),sfrac=0.001,uiw=uiw,liw=liw,xlab=lab[3],col="red",ylab=ylab,main=main,cex.main=cex.main,lty=1)
	lines(x,log(z),lwd=2,col="blue")
      }
      cpuefun4 <- function(){
	plot(log(y),log(z),xlab=xlab,main=main,cex.main=cex.main,col="blue",pch=19,ylab=ylab2)
	lines(x=range(log(z)),y=range(log(z)))
	if(smooth && npoints > 6 && diff(range(y))>0){
	  psmooth <- loess(log(z)~log(y),degree=1)
	  lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
      }
      if(13 %in% plot){
	cpuefun3()
	cpuefun4()
      }
      if(13 %in% print){
	png(file=paste(plotdir,"13_logcpuefit",i,".png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
	cpuefun3()
	dev.off()
	cpuefun4()
	dev.off()
      }
    } # nfleets
    if(verbose) print("Finished plot 13: CPUE plots",quote=FALSE)
    flush.console()
  } # end if 13 in plot or print

  ### Plot 14: numbers at age ###
  if(14 %in% c(plot, print)){
    if(is.null(natage)){
      print("Skipped plot 14 because NUMBERS_AT_AGE unavailable in report file",quote=FALSE)
      print("	change starter file setting for 'detailed age-structured reports'",quote=FALSE)
    }else{
      bseas <- unique(natage$BirthSeas)
      if(length(bseas)>1) print("Numbers at age plots are for only the first birth season",quote=FALSE)
      if(ngpatterns>0) print("Numbers at age plots are for growth pattern 1 only",quote=FALSE)
      if(nseasons>1) print("Numbers at age plots are for season 1 only",quote=FALSE)
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
	  if(diff(range(natageratio,finite=TRUE))!=0){
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
	    print("skipped sex ratio contour plot because ratio=1 for all ages and years",quote=FALSE)
	  }
	} # end area loop
      } # end if nsexes>1

      # plot of equilibrium age composition by gender and area
      tempfun <- function(){
        equilage <- natage[natage$Era=="VIRG",]
        plot(0,type='n',xlim=c(0,accuage),ylim=c(0,1.05*max(equilage[,-(1:10)])),xaxs='i',yaxs='i',
             xlab='Age',ylab='Numbers at age at equilibrium')
        legendlty <- NULL
        legendcol <- NULL
        legendlegend <- NULL

        for(iarea in areas){
          for(m in 1:nsexes){
            equilagetemp <- equilage[equilage$Area==iarea & equilage$Gender==m & equilage$SubMorph==mainmorphs[m],]
            if(nrow(equilagetemp)>1){
              print('in plot of equilibrium age composition by gender and area',quote=FALSE)
              print('multiple morphs or seasons not supporting, using first row from choices below',quote=FALSE)
              print(equilagetemp[,1:10])
            }
            equilagetemp <- equilagetemp[1,-(1:10)]
            lines(0:accuage,equilagetemp,lty=m,lwd=3,col=areacols[iarea])
            legendlty <- c(legendlty,m)
            legendcol <- c(legendcol,areacols[iarea])

            if(m==1 & nsexes==1) sextitle <- ""
            if(m==1 & nsexes==2) sextitle <- "Females"
            if(m==2) sextitle="Males"
            if(nareas>1) sextitle <- paste(sextitle," in ",areanames[iarea],sep="")
            legendlegend <- c(legendlegend,sextitle)
          }
        }
        if(length(legendlegend)>1) legend('topright',legend=legendlegend,col=legendcol,lty=legendlty,lwd=3)
      }

      if(14 %in% plot){
        tempfun()
      } # end if 14 in plot
      if(14 %in% print){
        png(file=paste(plotdir,"14_equilagecomp.png",sep=""),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
        tempfun()
        dev.off()
      } # close if 14 in print

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
      if(verbose) print("Finished plot 14: Numbers at age",quote=FALSE)
      flush.console()
    }
  } # close if 14 in plot or print


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
    compdbase$Kind[compdbase$Kind=="L@A" & compdbase$Ageerr < 0] <- "W@A"
    ladbase	   <- compdbase[compdbase$Kind=="L@A" & !is.na(compdbase$N),]
    wadbase	   <- compdbase[compdbase$Kind=="W@A" & !is.na(compdbase$N),]
    tagdbase1	   <- compdbase[compdbase$Kind=="TAG1",]
    tagdbase2	   <- compdbase[compdbase$Kind=="TAG2",]
    # consider range of bins for conditional age at length data
    print(paste("CompReport file separated by this code as follows (rows = num. comps * num. bins):"),quote=FALSE)
    print(paste("  ",nrow(lendbase),"rows of length comp data,"),quote=FALSE)
    print(paste("  ",nrow(sizedbase),"rows of generalized size comp data,"),quote=FALSE)
    print(paste("  ",nrow(agedbase),"rows of age comp data,"),quote=FALSE)
    print(paste("  ",nrow(condbase),"rows of conditional age-at-length data, and"),quote=FALSE)
    print(paste("  ",nrow(ghostagedbase),"rows of ghost fleet age comp data"),quote=FALSE)
    print(paste("  ",nrow(ladbase),"rows of mean length at age data"),quote=FALSE)
    print(paste("  ",nrow(wadbase),"rows of mean weight at age data"),quote=FALSE)
    print(paste("  ",nrow(tagdbase1),"rows of 'TAG1' comp data"),quote=FALSE)
    print(paste("  ",nrow(tagdbase2),"rows of 'TAG2' comp data"),quote=FALSE)
    Lbin_ranges <- as.data.frame(table(agedbase$Lbin_range))
    names(Lbin_ranges)[1] <- "Lbin_hi-Lbin_lo"
    if(length(unique(agedbase$Lbin_range)) > 1){
      print("Warning!: different ranges of Lbin_lo to Lbin_hi found in age comps.",quote=FALSE)
      print(Lbin_ranges)
      print("  consider increasing 'aalmaxbinrange' to designate",quote=FALSE)
      print("  some of these data as conditional age-at-length",quote=FALSE)
    }
    # convert bin indices to true lengths

    ## # don't remember why these are not converted to numeric in SSv3_output, nor why they aren't done in 1 step to compdbase
    ## lendbase$effN  <- as.numeric(lendbase$effN)
    ## sizedbase$effN <- as.numeric(sizedbase$effN)
    ## agedbase$effN  <- as.numeric(agedbase$effN)
    ## condbase$effN  <- as.numeric(condbase$effN)
  }
  # now make use of SSplotcomps function to make composition plots
  if(!datplot)
    {
      if(length(intersect(15:17,c(plot,print)))>0)
	print("skipped data-only plots 15-17 (comp data without fit) because input 'datplot=F'",quote=FALSE)
    }else{
      if(15 %in% c(plot,print))	 # data only aspects
	{
	  # length comp bar plot
	  SSv3_plot_comps(datonly=TRUE,kind="LEN",bub=F,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  # length comp bubble plot
	  SSv3_plot_comps(datonly=TRUE,kind="LEN",bub=TRUE,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,cohortlines=cohortlines,...)
	  # size comp bar plot
	  SSv3_plot_comps(datonly=TRUE,kind="SIZE",bub=F,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  # size comp bubble plot
	  SSv3_plot_comps(datonly=TRUE,kind="SIZE",bub=TRUE,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(15%in%print),GUI=(15%in%plot),plotdir=plotdir,cex.main=cex.main,cohortlines=cohortlines,...)
	  if(verbose) print("Finished plot 15: length and size comp data",quote=FALSE)
	  flush.console()
	}
      if(16 %in% c(plot,print))
	{
	  # age comp bar plot
	  SSv3_plot_comps(datonly=TRUE,kind="AGE",bub=F,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  # age comp bubble plot
	  SSv3_plot_comps(datonly=TRUE,kind="AGE",bub=TRUE,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  # ghost age comp bar plot
	  SSv3_plot_comps(datonly=TRUE,kind="GSTAGE",bub=F,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=F,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  # ghost age comp bubble plot
	  SSv3_plot_comps(datonly=TRUE,kind="GSTAGE",bub=TRUE,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=F,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
			  png=(16%in%print),GUI=(16%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  if(verbose) print("Finished plot 16: age comp data",quote=FALSE)
	  flush.console()
	}
      if(17 %in% c(plot,print))
	{
	  # conditional age plot
	  SSv3_plot_comps(datonly=TRUE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
			  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=F,
			  maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,
			  fixdims=fixdims,
			  png=(17%in%print),GUI=(17%in%plot),plotdir=plotdir,cex.main=cex.main,...)
	  if(verbose) print("Finished plot 17: conditional age at length data",quote=FALSE)
	  flush.console()
	}
    } # end if datplot

  # plot 18: length comp data with fits, sample size, etc.
  if(18 %in% c(plot,print)){
    SSv3_plot_comps(datonly=F,kind="LEN",bub=TRUE,verbose=verbose,fleets=fleets,
		    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
		    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
		    png=(18%in%print),GUI=(18%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,...)
    SSv3_plot_comps(datonly=F,kind="SIZE",bub=TRUE,verbose=verbose,fleets=fleets,
		    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
		    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
		    png=(18%in%print),GUI=(18%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,...)
    if(verbose) print("Finished plot 18: length and size comps with fits",quote=FALSE)
    flush.console()
  }

  # plot 19: age comp data with fits, sample size, etc.
  if(19 %in% c(plot,print)){
    SSv3_plot_comps(datonly=F,kind="AGE",bub=TRUE,verbose=verbose,fleets=fleets,
		    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
		    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
		    png=(19%in%print),GUI=(19%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,...)
    SSv3_plot_comps(datonly=F,kind="GSTAGE",bub=TRUE,verbose=verbose,fleets=fleets,
		    samplesizeplots=F,showsampsize=F,showeffN=F,
		    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
		    png=(19%in%print),GUI=(19%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,...)
    if(verbose) print("Finished plot 19: age comps with fits",quote=FALSE)
    flush.console()
  } # end if 19 in plot or print

  # plot 20: conditional age at length plot with fits, sample size, etc.
  if(20 %in% c(plot,print)){
  if(aalresids==TRUE){
    SSv3_plot_comps(datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
		    aalbin=aalbin,aalyear=aalyear,
		    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
		    maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,
		    png=(20%in%print),GUI=(20%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,...)
    if(verbose) print("Finished plot 20a: conditional age at length with fits",quote=FALSE)
    }
  # more plot 20: Andre's new conditional age-at-length plots
    if(nrow(condbase)==0){
      if(verbose) print("Skipped plot 20b: mean age and std. dev. in conditional AAL: no data of this type",quote=FALSE)
    }else{
      Lens <-sort(unique(condbase$Lbin_lo))
      Yrs <- sort(unique(condbase$Yr))
      par(mfrow=c(3,2))
      for(fleets in 1:nfleets){
      for (Gender in 1:nsexes){
	for (Yr in Yrs){
         y <- condbase[condbase$Yr==Yr & condbase$Gender==Gender & condbase$Fleet==fleets,]
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
     } # end fleet loop
      if(verbose) print("Finished plot 20b: mean age and std. dev. in conditional AAL",quote=FALSE)
      if(verbose) print("  This is a new plot, currently in beta mode.",quote=FALSE)
      if(verbose) print("  Left plots are mean AAL by size-class (obs. and pred.)",quote=FALSE)
      if(verbose) print("  with 90% CIs based on adding 1.64 SE of mean to the data",quote=FALSE)
      if(verbose) print("  Right plots in each pair are SE of mean AAL (obs. and pred.)",quote=FALSE)
      if(verbose) print("  with 90% CIs based on the chi-square distribution.",quote=FALSE)
      flush.console()
    }
  } # end if 20 in plot or print

  # plot 21: mean length at age and mean weight at age
  if(21 %in% c(plot,print)){
    SSv3_plot_comps(datonly=FALSE,kind="L@A",bub=TRUE,verbose=verbose,fleets=fleets,
		    samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
		    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
		    png=(21%in%print),GUI=(21%in%plot),smooth=smooth,plotdir=plotdir,
		    maxneff=maxneff,cex.main=cex.main,...)
    SSv3_plot_comps(datonly=FALSE,kind="W@A",bub=TRUE,verbose=verbose,fleets=fleets,
		    samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
		    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,
		    png=(21%in%print),GUI=(21%in%plot),smooth=smooth,plotdir=plotdir,
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
    if(!is.null(equil_yield[1,1]) && !is.na(equil_yield[1,1])){
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
      if(verbose) print("Finished plot 22: yield curve",quote=FALSE)
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
	xlim <- c(0,max(Bio_all_good,na.rm=TRUE))
	ylim <- c(min(0,sprod_good,na.rm=TRUE),max(sprod_good,na.rm=TRUE))
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
      if(verbose) print("Finished plot 22: Surplus production",quote=FALSE)
    }
    if(nareas>1) print("Surplus production plot not implemented for multi-area models",quote=FALSE)
  } # close plot section 22

  ### Plot 23: CPUE data-only plots ###
  if(23 %in% c(plot, print)){
    if(!datplot){
      print("skipped plots 23 (CPUE without fit) because input 'datplot=F'",quote=FALSE)
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
      if(verbose) print("Finished plot 23: CPUE data plots",quote=FALSE)
      flush.console()
    } # end if datplot
  } # end if 23 in plot or print

  ### Plot 24: Tag plots ###
  if(24 %in% c(plot, print)){
    if(nrow(tagdbase2)==0){
      print("skipped plots 24 (tags) because there's no tag-related data'",quote=FALSE)
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
	    mtext(lab[3],side=1,line=0,outer=TRUE)
	    mtext(lab[19],side=2,line=0,outer=TRUE)
	    mtext("Fit to tag recaptures by tag group",side=3,line=0,outer=TRUE,cex=cex.main,font=2)
	  }
	}

	# restore default single panel settings
	par(mfcol=c(rows,cols),mar=c(5,5,4,2)+.1,oma=rep(0,4))
      }

      print("Calculated tagging related quantities...",quote=FALSE)
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
      tagobs <- aggregate(x$Obs,by=list(x$Yr,x$Rep),FUN=sum,na.rm=TRUE)
      tagexp <- aggregate(x$Exp,by=list(x$Yr,x$Rep),FUN=sum,na.rm=TRUE)
      Recaps <- data.frame(Yr=tagobs[,1],Group=tagobs[,2],Obs=tagobs[,3],Exp=tagexp[,3])

      xlim <- range(Recaps[,1])
      xx2 <- aggregate(Recaps[,3],by=list(Recaps$Yr),FUN=sum,na.rm=TRUE)
      xx3 <- aggregate(Recaps[,4],by=list(Recaps$Yr),FUN=sum,na.rm=TRUE)
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
		las=1,main=plottitle,cex.main=cex.main,maxsize=pntscalar,allopen=FALSE,minnbubble=minnbubble)
      }
      tagfun4 <- function(){
	# bubble plot of residuals
	plottitle <- "Residuals for tag recaptures: (obs-exp)/sqrt(exp)"
	bubble3(x=Recaps$Yr,y=Recaps$Group,z=Recaps$Pearson,xlab=lab[3],ylab="Tag Group",col=rep("blue",2),
		las=1,main=plottitle,cex.main=cex.main,maxsize=pntscalar,allopen=FALSE,minnbubble=minnbubble)
      }
      tagfun5 <- function(){
	# line plot by year and group
	plottitle <- "Observed tag recaptures by year and tag group"
	plot(0,type="n",xlim=range(Recaps$Yr),ylim=range(Recaps$Group),xlab=lab[3],ylab="Tag Group",
	     main=plottitle,cex.main=cex.main)
	rescale <- 5/max(Recaps$Obs,Recaps$Exp)
	for(igroup in sort(unique(Recaps$Group))){
	  lines(Recaps$Yr[Recaps$Group==igroup],igroup+0*Recaps$Obs[Recaps$Group==igroup],col="grey",lty=3)
	  points(Recaps$Yr[Recaps$Group==igroup],igroup+rescale*Recaps$Obs[Recaps$Group==igroup],type="o",pch=16,cex=.5)
	  lines(Recaps$Yr[Recaps$Group==igroup],igroup+rescale*Recaps$Exp[Recaps$Group==igroup],col="red",lty="51",lwd=2)
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
      if(verbose) print("Finished plot 24: tags",quote=FALSE)
      flush.console()
    } # end if tag data present
  } # end if 24 in plot or print
  if(pdf) dev.off() # close PDF file if it was open
  if(verbose) print("Finished all requested plots",quote=FALSE)
  ### end of SSplots function
}

