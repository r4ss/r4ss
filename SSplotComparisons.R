SSplotComparisons <-
  function(summaryoutput,subplots=1:13,
           plot=TRUE,print=FALSE,
           models="all",
           endyrvec=NULL,
           indexfleets="default",
           indexUncertainty=FALSE,
           indexSEvec="default",
           indexPlotEach=FALSE,         #TRUE plots the observed index for each model with colors, or FALSE just plots observed once in black dots
           labels=c("Year",             #1
             "Spawning biomass (mt)",   #2
             "Spawning depletion",      #3
             "Age-0 recruits (1,000s)", #4
             "Recruitment deviations",  #5
             "Index",                   #6
             "Log index",               #7
             "Density"),                #8
           col="default", shadecol="default",
           pch="default", lty=1, lwd=2,
           xlim="default", xaxs="r", yaxs="r",
           type="o", uncertainty=TRUE, shadealpha=0.1,
           legend=TRUE, legendlabels="default", legendloc="topright",
           btarg=0.4, minbthresh=0.25,
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           plotdir=NULL,
           densitynames=c("SPB_Virgin","SPB_2011","Bratio_2011","SR_R0","TotYield_MSY"),
           densityxlabs=c("B0 (mt)","Spawning Biomass in 2011 (mt)","depletion in 2011","log(R0)","MSY (mt)"),
           densityscalex=1,
           densityscaley=1,
           fix0=TRUE,
           new=TRUE,
           verbose=TRUE,
           mcmcVec="default")
{
  # subfunction to write png files
  pngfun <- function(file) png(file=paste(plotdir,file,sep="/"),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
  if(print & is.null(plotdir)) stop("to print PNG files, you must supply a directory as 'plotdir'")
  
  # subfunction to add legend
  legendfun <- function() legend(legendloc, legend=legendlabels, col=col, lty=lty, lwd=lwd, pch=pch, bty="n")

  rc <- function(n,alpha=1){
    # a subset of rich.colors by Arni Magnusson from the gregmisc package
    # a.k.a. rich.colors.short, but put directly in this function
    # to try to diagnose problem with transparency on one computer
    x <- seq(0, 1, length = n)
    r <- 1/(1 + exp(20 - 35 * x))
    g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
    b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
    rgb.m <- matrix(c(r, g, b), ncol = 3)
    rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha=alpha))
  }
  
  # get stuff from summary output
  n             <- summaryoutput$n
  nsexes        <- summaryoutput$nsexes
  pars          <- summaryoutput$pars
  parsSD        <- summaryoutput$parsSD
  parphases     <- summaryoutput$parphases
  quants        <- summaryoutput$quants
  quantsSD      <- summaryoutput$quantsSD
  SpawnBio      <- summaryoutput$SpawnBio
  SpawnBioLower <- summaryoutput$SpawnBioLower
  SpawnBioUpper <- summaryoutput$SpawnBioUpper
  Bratio        <- summaryoutput$Bratio
  BratioLower   <- summaryoutput$BratioLower
  BratioUpper   <- summaryoutput$BratioUpper
  SPRratio      <- summaryoutput$SPRratio
  SPRratioLower <- summaryoutput$SPRratioLower
  SPRratioUpper <- summaryoutput$SPRratioUpper
  recruits      <- summaryoutput$recruits
  recruitsLower <- summaryoutput$recruitsLower
  recruitsUpper <- summaryoutput$recruitsUpper
  recdevs       <- summaryoutput$recdevs
  recdevsLower  <- summaryoutput$recdevsLower
  recdevsUpper  <- summaryoutput$recdevsUpper
  indices       <- summaryoutput$indices
  mcmc          <- summaryoutput$mcmc               #a list of dataframes, 1 for each model with mcmc output
  lowerCI       <- summaryoutput$lowerCI
  upperCI       <- summaryoutput$upperCI
  
  # fix biomass for single-sex models
  if(any(nsexes==1)){
    cat("dividing SpawnBio by 2 for single-sex models:",(1:n)[nsexes==1],"\n")
    for(i in (1:n)[nsexes==1]){
      SpawnBio[,i]    <- SpawnBio[,i]/2
      SpawnBioLower[,i]  <- SpawnBioLower[,i]/2
      SpawnBioUpper[,i]  <- SpawnBioUpper[,i]/2
    }
  }
  
  if(models[1]=="all") models <- 1:n
  nlines <- length(models)
  
  if(col[1]=="default" & nlines>3) col <- rc(nlines+1)[-1]
  if(col[1]=="default" & nlines<3) col <- rc(nlines)
  if(col[1]=="default" & nlines==3) col <- c("blue","red","green3")
  if(shadecol[1]=="default" & nlines>3) shadecol <- rc(nlines+1,alpha=shadealpha)[-1]
  if(shadecol[1]=="default" & nlines<3) shadecol <- rc(nlines,alpha=shadealpha)
  if(shadecol[1]=="default" & nlines==3) shadecol <- rgb(red=c(0,1,0),green=c(0,0,0.8),blue=c(1,0,0),alpha=shadealpha)

  if(pch[1]=="default") pch <- 1:nlines
  if(lty[1]=="default") lty <- 1:nlines

  if(length(col) < nlines) col <- rep(col,nlines)
  if(length(pch) < nlines) pch <- rep(pch,nlines)
  if(length(lty) < nlines) lty <- rep(lty,nlines)
  if(length(lwd) < nlines) lwd <- rep(lwd,nlines)
  
  if(legendlabels[1]=="default") legendlabels <- paste("model",1:nlines)

  # determine operating system and open new window if requested
  if(grepl("linux",version$os)) OS <- "Linux"
  if(grepl("mingw",version$os)) OS <- "Windows"
  # need appropriate line to support Mac operating systems

  if(plot & new){
    if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    if(OS=="Windows") windows(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
    if(OS=="Linux") X11(width=pwidth,height=pheight,pointsize=ptsize)
    if(OS=="Mac") quartz(width=pwidth,height=pheight,pointsize=ptsize)
  }

  if(mcmcVec[1]=="default") mcmcVec <- rep(FALSE,nlines)
  # reset values to NA
  cols <- (1:n)[mcmcVec]
  SpawnBioLower[,cols] <- SpawnBioUpper[,cols] <- SpawnBio[,cols] <- NA
  BratioLower[,cols] <- BratioUpper[,cols] <- Bratio[,cols] <- NA
  SPRratioLower[,cols] <- SPRratioUpper[,cols] <- SPRratio[,cols] <- NA
  recruitsLower[,cols] <- recruitsUpper[,cols] <- recruits[,cols] <- NA
  recdevsLower[,cols] <- recdevsUpper[,cols] <- recdevs[,cols] <- NA



  # get MCMC results if requested
  for(iline in (1:nlines)[mcmcVec]){
    imodel <- models[iline]
    
    ### get MCMC for SpawnBio
    tmp <- grep("SPB",names(mcmc[[imodel]]))   #try it to see what you get
    if(length(tmp) > 0) {   #there are some mcmc values to use
      mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
      mcmclabs <- names(mcmc.tmp)
      lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
      med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
      upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
      if(nsexes[iline] == 1) {
        lower <- lower/2
        upper <- upper/2
        med <- med/2
      }
      SpawnBio[,imodel] <- med[match(SpawnBio$Label,mcmclabs)]
      SpawnBioLower[,imodel] <- lower[match(SpawnBioLower$Label,mcmclabs)]
      SpawnBioUpper[,imodel] <- upper[match(SpawnBioUpper$Label,mcmclabs)]
    }

    ### get MCMC for Bratio
    for(iline in (1:nlines)[mcmcVec]){
      imodel <- models[iline]
      # get values from mcmc to replace
      tmp <- grep("Bratio",names(mcmc[[imodel]]))   #try it to see what you get
      if(length(tmp) > 0) {   #there are some mcmc values to use
        mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
        mcmclabs <- names(mcmc.tmp)
        lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
        med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
        upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
        Bratio[,imodel] <- med[match(Bratio$Label,mcmclabs)]
        BratioLower[,imodel] <- lower[match(BratioLower$Label,mcmclabs)]
        BratioUpper[,imodel] <- upper[match(BratioUpper$Label,mcmclabs)]
      }
    }

    ### get MCMC for SPRratio
    for(iline in (1:nlines)[mcmcVec]){
      imodel <- models[iline]
      # get values from mcmc to replace
      tmp <- grep("SPRratio",names(mcmc[[imodel]]))   #try it to see what you get
      if(length(tmp) > 0) {   #there are some mcmc values to use
        mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
        mcmclabs <- names(mcmc.tmp)
        lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
        med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
        upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
        SPRratio[,imodel] <- med[match(SPRratio$Label,mcmclabs)]
        SPRratioLower[,imodel] <- lower[match(SPRratioLower$Label,mcmclabs)]
        SPRratioUpper[,imodel] <- upper[match(SPRratioUpper$Label,mcmclabs)]
      }
    }
  
    ### get MCMC for recruits
    # get values from mcmc to replace
    for(iline in (1:nlines)[mcmcVec]){
      imodel <- models[iline]
      tmp <- grep("^Recr_",names(mcmc[[imodel]]))   #try it to see what you get
      tmp2 <- grep("Recr_Unfished",names(mcmc[[imodel]]))
      tmp <- setdiff(tmp,tmp2)
      if(length(tmp) > 0) { #there are some mcmc values to use
        mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
        mcmclabs <- names(mcmc.tmp)
        lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
        med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
        upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
        recruits[,imodel] <- med[match(recruits$Label,mcmclabs)]
        recruitsLower[,imodel] <- lower[match(recruitsLower$Label,mcmclabs)]
        recruitsUpper[,imodel] <- upper[match(recruitsUpper$Label,mcmclabs)]
      }
    }

    ### get MCMC for recdevs
    for(iline in (1:nlines)[mcmcVec]){
      imodel <- models[iline]
      # get values from mcmc to replace
      tmp <- unique(c(grep("_RecrDev_",names(mcmc[[imodel]])),
                      grep("_InitAge_",names(mcmc[[imodel]])),
                      grep("ForeRecr_",names(mcmc[[imodel]]))))
      if(length(tmp) > 0) { #there are some mcmc values to use
        mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
        mcmclabs <- names(mcmc.tmp)
        lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
        med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
        upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
        recdevs[,imodel] <- med[match(recdevs$Label,mcmclabs)]
        recdevsLower[,imodel] <- lower[match(recdevsLower$Label,mcmclabs)]
        recdevsUpper[,imodel] <- upper[match(recdevsUpper$Label,mcmclabs)]
      }
    }
  }

  if(length(endyrvec)==1) endyrvec <- rep(endyrvec,nlines)
  if(!is.null(endyrvec)){
    for(iline in 1:nlines){
      endyr <- endyrvec[iline]
      imodel <- models[iline]
      SpawnBio[SpawnBio$Yr > endyr, imodel] <- NA
      SpawnBioLower[SpawnBio$Yr > endyr, imodel] <- NA
      SpawnBioUpper[SpawnBio$Yr > endyr, imodel] <- NA
      Bratio[Bratio$Yr > endyr, imodel] <- NA
      BratioLower[Bratio$Yr > endyr, imodel] <- NA
      BratioUpper[Bratio$Yr > endyr, imodel] <- NA
      SPRratio[SPRratio$Yr >= endyr, imodel] <- NA
      SPRratioLower[SPRratio$Yr >= endyr, imodel] <- NA
      SPRratioUpper[SPRratio$Yr >= endyr, imodel] <- NA
      recruits[recruits$Yr > endyr, imodel] <- NA
      recruitsLower[recruits$Yr > endyr, imodel] <- NA
      recruitsUpper[recruits$Yr > endyr, imodel] <- NA
      recdevs[recdevs$Yr > endyr, imodel] <- NA
      recdevsLower[recdevs$Yr > endyr, imodel] <- NA
      recdevsUpper[recdevs$Yr > endyr, imodel] <- NA
    }
  }
  
  
  addpoly <- function(yrvec, lower, upper){ # add shaded uncertainty intervals behind line
    lower[lower<0] <- 0 # max of value or 0
    for(iline in 1:nlines){
      imodel <- models[iline]
      good <- !is.na(lower[,imodel]) & !is.na(upper[,imodel])
      polygon(x=c(yrvec[good],rev(yrvec[good])),
              y=c(lower[good,imodel],rev(upper[good,imodel])),
              border=NA,col=shadecol[iline])
      lines(yrvec[good],lower[good,imodel],lty=3,col=col[iline])
      lines(yrvec[good],upper[good,imodel],lty=3,col=col[iline])
    }
  }

  equ <- -(1:2)
  
  plotSpawnBio <- function(uncertainty=TRUE){ # plot spawning biomass
    # get axis limits
    if(xlim[1]=="default"){
      xlim <- range(SpawnBio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    ylim <- range(0, SpawnBio[,models], na.rm=TRUE)
    if(uncertainty) ylim <- range(ylim, SpawnBioUpper[,models], na.rm=TRUE)

    # do some scaling of y-axis
    ylab <- labels[2]
    yunits <- 1
    if(ylim[2] > 1e3 & ylim[2] < 1e6){
      yunits <- 1e3
      ylab <- gsub("mt","x1000 mt",ylab)
    }
    if(ylim[2] > 1e6){
      yunits <- 1e6
      ylab <- gsub("mt","million mt",ylab)
    }
    plot(0,type="n",xlim=xlim,ylim=ylim,xlab=labels[1],ylab=ylab,xaxs=xaxs,yaxs=yaxs,axes=FALSE)
    if(uncertainty){
      # add shading for undertainty
      addpoly(yrvec=SpawnBio$Yr[-(1:2)], lower=SpawnBioLower[-(1:2),], upper=SpawnBioUpper[-(1:2),])
      xEqu <- SpawnBio$Yr[2] - (1:nlines)/nlines # equilibrium spawning biomass year by model
    }else{
      xEqu <- rep(SpawnBio$Yr[2], nlines)  # equilibrium spawning biomass year by model
    }
    # add arrows for equilibrium values
    matplot(SpawnBio$Yr[-(1:2)], SpawnBio[-(1:2), models],
            col=col,pch=pch,lty=lty,lwd=lwd,type=type,add=TRUE)
    if(uncertainty) arrows(x0=xEqu, y0=as.numeric(SpawnBioLower[1,models]),
                           x1=xEqu, y1=as.numeric(SpawnBioUpper[1,models]),
                           length=0.01, angle=90, code=3, col=col)
    # add points at equilibrium values
    points(x=xEqu, SpawnBio[1, models], col=col, pch=pch, cex=1.2, lwd=lwd)
    abline(h=0,col="grey")
    if(legend) legendfun()

    # add axes
    axis(1)
    yticks <- pretty(ylim)
    axis(2,at=yticks,lab=format(yticks/yunits),las=1)
    box()
  }

  plotBratio <- function(uncertainty=TRUE){ # plot biomass ratio (may be identical to previous plot)
    # get axis limits
    if(xlim[1]=="default"){
      xlim <- range(Bratio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    ylim <- range(0, Bratio[,models], na.rm=TRUE)
    if(uncertainty) ylim <- range(ylim, BratioUpper[,models], na.rm=TRUE)

    # make plot
    plot(0,type="n",xlim=xlim,ylim=ylim,xlab=labels[1],ylab=labels[3],xaxs=xaxs,yaxs=yaxs,las=1)
    if(uncertainty) addpoly(Bratio$Yr, lower=BratioLower, upper=BratioUpper)
    matplot(Bratio$Yr,Bratio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,add=TRUE)
    abline(h=0,col="grey")
    abline(h=1,col="grey",lty=2)

    if(btarg>0){
      abline(h=btarg,col="red",lty=2)
      text(min(Bratio$Yr)+4,btarg+0.03,"Management target",adj=0)
    }
    if(minbthresh>0){
      abline(h=minbthresh,col="red",lty=2)
      text(min(Bratio$Yr)+4,minbthresh+0.03,"Minimum stock size threshold",adj=0)
    }

    if(legend) legendfun()
  }

  plotSPRratio <- function(uncertainty=TRUE){ # plot biomass ratio (may be identical to previous plot)
    # get axis limits
    if(xlim[1]=="default"){
      xlim <- range(SPRratio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    ylim <- range(0, SPRratio[,models], na.rm=TRUE)
    if(uncertainty) ylim <- range(ylim, SPRratioUpper[,models], na.rm=TRUE)

    # make plot
    plot(0,type="n",xlim=xlim,ylim=ylim,xlab=labels[1],ylab="(1-SPR)/(1-SPR_40%)" ,xaxs=xaxs,yaxs=yaxs,las=1)
    if(uncertainty) addpoly(SPRratio$Yr, lower=SPRratioLower, upper=SPRratioUpper)
    matplot(SPRratio$Yr,SPRratio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,add=TRUE)
    abline(h=0,col="grey")
    abline(h=1,col="grey",lty=2)

    if(btarg>0){
      abline(h=0,col="grey")
      abline(h=1,col="red",lty=2)
      text(SPRratio$Yr[1]+4,(1+0.02),"Management target",adj=0)
    }

    if(legend) legendfun()
  }
  
  plotRecruits <- function(uncertainty=TRUE){ # plot recruitment
    # determine y-limits
    ylim <- range(0,recruits[,models],na.rm=TRUE)
    if(uncertainty) ylim <- range(ylim, recruits[,models], recruitsUpper[,models], na.rm=TRUE)

    # do some automatic scaling of the units
    ylab <- labels[4]
    yunits <- 1
    if(ylim[2] > 1e3 & ylim[2] < 1e6){
      yunits <- 1e3
      ylab <- gsub("1,000s","millions",ylab)
    }
    if(ylim[2] > 1e6){
      yunits <- 1e6
      ylab <- gsub("1,000s","billions",ylab)
    }

    if(xlim[1]=="default"){
      xlim <- range(recruits$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }

    # plot lines showing recruitment
    matplot(recruits$Yr[-(1:2)],recruits[-(1:2),models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,
            xlim=xlim,ylim=ylim,
            xlab=labels[1],ylab=ylab,xaxs=xaxs,yaxs=yaxs,axes=FALSE)
    # add points at equilibrium values
    points(x=rep(recruits$Yr[1],nlines), recruits[1, models], col=col, pch=pch, cex=1.2, lwd=lwd)    

    if(uncertainty){
      for(iline in 1:nlines){
        imodel <- models[iline]
        xvec <- recruits$Yr
        if(nlines>1) xvec <- xvec + 0.4*iline/nlines - 0.2
        arrows(x0=xvec, y0=pmax(as.numeric(recruitsLower[,imodel]),0),
               x1=xvec, y1=as.numeric(recruitsUpper[,imodel]),
               length=0.01, angle=90, code=3, col=col[iline])
      }
    }
    abline(h=0,col="grey")
    if(legend) legendfun()
    axis(1)
    yticks <- pretty(ylim)
    axis(2,at=yticks,lab=format(yticks/yunits),las=1)
    box()
  }

  plotRecDevs <- function(uncertainty=TRUE){ # plot recruit deviations
    # empty plot
    if(xlim[1]=="default"){
      xlim <- range(recdevs$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    ylim <- range(recdevs[,models],na.rm=TRUE)
    if(uncertainty) ylim <- range(recdevsLower[,models],recdevsUpper[,models],na.rm=TRUE)
    ylim <- range(-ylim,ylim) # make symmetric
                   
    plot(0,xlim=xlim,ylim=ylim,
         type="n",xlab=labels[1],ylab=labels[5],xaxs=xaxs,yaxs=yaxs,las=1)
    abline(h=0,col="grey")

    if(uncertainty){
      for(iline in 1:nlines){
        imodel <- models[iline]
        xvec <- recdevs$Yr
        if(nlines>1) xvec <- xvec + 0.4*iline/nlines - 0.2
        arrows(x0=xvec, y0=as.numeric(recdevsLower[,imodel]),
               x1=xvec, y1=as.numeric(recdevsUpper[,imodel]),
               length=0.01, angle=90, code=3, col=col[iline])
      }
    }
    
    # loop over vector of models to add lines
    for(iline in 1:nlines){
      imodel <- models[iline]
      yvec <- recdevs[,imodel]
      xvec <- recdevs$Yr[!is.na(yvec)]
      yvec <- yvec[!is.na(yvec)]
      points(xvec,yvec,pch=pch[iline],lwd=lwd[iline],col=col[iline])
    }
    if(legend) legendfun()
  }

  plotIndices <- function(log=FALSE){ # plot different fits to a single index of abundance

    # get a subset of index table including only 1 index per model
    # (hopefully matching each other)
    indices2 <- NULL
    for(iline in 1:nlines){
      imodel <- models[iline]
      subset <- indices$imodel==imodel & !is.na(indices$Like)
      if(length(unique(indices$FleetNum[subset])) > 1){
        if(!is.null(indexfleets[imodel])){
          ifleet <- indexfleets[imodel]
          indices2 <- rbind(indices2,indices[subset & indices$FleetNum==ifleet,])
        }else{
          stop("some models have multiple indices, 'indexfleets' required\n  for all models in summaryoutput")
        }
      }else{
        indices2 <- rbind(indices2,indices[subset,])
      }
    }
       
    # get quantities for plot
    yr <- indices2$Yr
    obs <- indices2$Obs
    exp <- indices2$Exp
    imodel <- indices2$imodel
    if(log){
      obs <- log(obs)
      exp <- log(exp)
      ylab=labels[7]
    }else{
      ylab=labels[6]
    }

    # get uncertainty intervals if requested
    if(indexUncertainty){
      if(indexPlotEach) {
        if(indexSEvec[1]=="default") indexSEvec <- indices2$SE    #there may be a little bit of confusion from using just the first element of indexSEvec
        y <- obs
        if(log){
            upper <- qnorm(.975,mean=y,sd=indexSEvec)
            lower <- qnorm(.025,mean=y,sd=indexSEvec)
        }else{
            upper <- qlnorm(.975,meanlog=log(y),sdlog=indexSEvec)
            lower <- qlnorm(.025,meanlog=log(y),sdlog=indexSEvec)
        }
      }else {      
        subset <- indices2$imodel==models[1]
        if(indexSEvec[1]=="default") indexSEvec <- indices2$SE[subset]
        y <- obs[subset]
        if(log){
            upper <- qnorm(.975,mean=y,sd=indexSEvec)
            lower <- qnorm(.025,mean=y,sd=indexSEvec)
        }else{
            upper <- qlnorm(.975,meanlog=log(y),sdlog=indexSEvec)
            lower <- qlnorm(.025,meanlog=log(y),sdlog=indexSEvec)
        }
      }
    }else{
      upper <- NULL
      lower <- NULL
    }
    
    # make plot
    ylim <- range(obs,exp,lower,upper)
    if(!log) ylim <- range(0,ylim) # 0 included if not in log space
    
    plot(0,type="n",xlim=range(yr),ylim=ylim,xlab="Year",ylab=ylab,axes=FALSE)
    if(!log) abline(h=0,col="grey")
    for(iline in (1:nlines)[!mcmcVec]){
      imodel <- models[iline]
      subset <- indices2$imodel==imodel
      x <- yr[subset]
      y <- exp[subset]
      lines(x, y, pch=pch[iline], lwd=lwd[iline],
            lty=lty[iline], col=col[iline], type=type)
      if(legend) legendfun()
    }
    
    # get uncertainty intervals if requested
      
    # put observed values on top
    #subset <- indices2$imodel==1
    # points(yr[subset],obs[subset],pch=16,cex=1.5,type="o",lty=3) # connected by dashed lines
    if(indexPlotEach) {  #plot observed values for each model or just the first model
        for(iline in (1:nlines)[!mcmcVec]){
            imodel <- models[iline]
            subset <- indices2$imodel==imodel
            if(indexUncertainty)
                arrows(x0=yr[subset], y0=lower[subset], x1=yr[subset], y1=upper[subset], length=0.01, angle=90, code=3, col=shadecol[iline])
            points(yr[subset],obs[subset],pch=16,cex=1.5,col=shadecol[iline])
        }
    }else {
        imodel <- models[1]
        subset <- indices2$imodel==imodel
        if(indexUncertainty)
            arrows(x0=yr[subset], y0=lower[subset], x1=yr[subset], y1=upper[subset], length=0.01, angle=90, code=3, col=1)
        points(yr[subset],obs[subset],pch=16,cex=1.5)
    }

    axis(1,at=yr)
    axis(2)
    box()
  } # end plotIndices function
  
  plotDensities <- function(parname,xlab,limit0=TRUE){
    if(any(!mcmcVec)) { 
        vals <- rbind(pars[grep(parname,pars$Label),],
                  quants[grep(parname,quants$Label),])
        if(nrow(vals)!=1){
            cat("problem getting values for parameter:",parname,"\n")
            if(nrow(vals)==0) cat("no Labels matching in either parameters or derived quantities\n")
            if(nrow(vals)>0){
                cat("Too many matching Labels:")
                print(vals)
            }
            return(NULL)  #previous versions had an else statement, but this will end the function here instead and saves indenting
        }
        valSDs <- rbind(parsSD[grep(parname,pars$Label),],
                      quantsSD[grep(parname,quants$Label),])
    }

    xmax <- xmin <- ymax <- NULL # placeholder for limits
    mcmcDens <- vector(mode="list",length=nlines)   #placeholder for the mcmc density estimates, if there are any
    # loop over models to set range
    good <- rep(TRUE,nlines) # indicator of which values to plot
    for(iline in 1:nlines){
      imodel <- models[iline]
      if(mcmcVec[iline]) {
        
        mcmcColumn <- grep(parname,colnames(mcmc[[imodel]]))
        if(length(mcmcColumn)==0) {
            cat("No columns selected from MCMC for '",parname,"' in model ",imodel,".\n",sep="")
            good[iline] <- FALSE 
        }
        if(length(mcmcColumn)>1) {
            cat("Too many columns selected from MCMC for model ",imodel,":\n",sep="")
            print(names(mcmc[[imodel]])[mcmcColumn])
            cat("Please specify a unique label in the mcmc dataframe\nor specify mcmcVec=F for model",imodel,"or specify mcmcVec='default'.\n")
            good[iline] <- FALSE 
        }
        if(good[iline]){
          mcmcVals <- mcmc[[imodel]][,mcmcColumn]
          if(nsexes[imodel]==1 &&  grepl("SPB",parname)) {   #divide by 2 for feamle only spawning biomass
            mcmcVals <- mcmcVals/2
          }
          xmin <- min(xmin, quantile(mcmcVals,0.001))
          xmax <- max(xmax, quantile(mcmcVals,0.999))
          z <- density(mcmcVals,from=0)      #density estimate of mcmc sample (posterior)
          z$x <- z$x[c(1,1:length(z$x),length(z$x))]
          z$y <- c(0,z$y,0)           #just to make sure that a good looking polygon is created
          ymax <- max(ymax,max(z$y))  #update ymax
          mcmcDens[[iline]] <- z      #save the density estimate for later plotting
        }
      }else{
        parval <- vals[1,imodel]
        parSD <- valSDs[1,imodel]
        if(!is.numeric(parval)) parval <- -1     #do this in case models added without the parameter
        if(!is.na(parSD) && parSD>0){ # if non-zero SD available
          if(nsexes[imodel]==1 &&  grepl("SPB",parname)) {   #divide by 2 for feamle only spawning biomass
            parval <- parval/2
            parSD <- parSD/2
          }
          # update x range
          xmin <- min(xmin, qnorm(0.001,parval,parSD))
          xmax <- max(xmax, qnorm(0.999,parval,parSD))
          # calculate density to get y range
          x <- seq(xmin,xmax,length=500)
          mle <- dnorm(x,parval,parSD)
          mlescale <- 1/(sum(mle)*mean(diff(x)))
          mle <- mle*mlescale
          # update ymax
          ymax <- max(ymax,max(mle)) 
        }else{ # if no SD, at least make sure interval includes MLE estimate
          xmin <- min(xmin, parval)
          xmax <- max(xmax, parval)
        }
      }
    }
    if(grepl("Bratio",parname)) xmin <- 0 # xmin=0 for depletion plots
    if(limit0) xmin <- max(0,xmin) # by default no plot can go below 0 
    if(fix0 & !grepl("SR_R0",parname)) xmin <- 0 # include 0 if requested (except for log(R0) plots)
    
    # calculate x-limits and vector of values for densities
    xlim <- c(xmin,xmin+(xmax-xmin)*densityscalex)
    x <- seq(xmin,xmax,length=500)
    
    # calculate some scaling stuff
    xunits <- 1
    if(xmax > 1e3 & xmax < 1e6){
      xunits <- 1e3
      xlab <- gsub("mt","x1000 mt",xlab)
    }
    if(xmax > 1e6){
      xunits <- 1e6
      xlab <- gsub("mt","million mt",xlab)
    }
    # make empty plot
    plot(0,type="n",xlim=xlim,axes=FALSE,xaxs="i",
         ylim=c(0,1.1*ymax*densityscaley),xlab=xlab,ylab="")

    # add vertical lines for target and threshold depletion values
    if(grepl("Bratio",parname)){
      if(btarg>0){
        abline(v=btarg,col="red",lty=2)
        text(btarg+0.03,par()$usr[4],"Management target",adj=1.05,srt=90)
      }
      if(minbthresh>0){
        abline(v=minbthresh,col="red",lty=2)
        text(minbthresh+0.03,par()$usr[4],"Minimum stock size threshold",adj=1.05,srt=90)
      }
    }
      
    symbolsQuants <- c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)
    # loop again to make plots
    for(iline in (1:nlines)[good]){
      imodel <- models[iline]
      if(mcmcVec[iline]) {
        mcmcColumn <- grep(parname,colnames(mcmc[[imodel]]))
        mcmcVals <- mcmc[[imodel]][,mcmcColumn]
        if(nsexes[imodel]==1 &&  grepl("SPB",parname)) {   #divide by 2 for feamle only spawning biomass
            mcmcVals <- mcmcVals/2
        }
        x2 <- quantile(mcmcVals,symbolsQuants)   # for symbols on plot
        #find the positions in the density that are closest to these quantiles
        x <- mcmcDens[[iline]]$x
        y <- mcmcDens[[iline]]$y
        yscale <- 1/(sum(y)*mean(diff(x)))
        y <- y*yscale
        y2 <- NULL
        for(ii in x2) {
            y2 <- c(y2,y[abs(x-ii)==min(abs(x-ii))])
        }
        polygon(c(x[1],x,rev(x)[1]),c(0,y,0),col=shadecol[iline],border=NA)
        lines(x,y,col=col[iline],lwd=2)
        points(x2,y2,col=col[iline],pch=pch[iline])
        lines(rep(x2[median(1:length(x2))],2),c(0,y2[median(1:length(x2))]),col=col[iline]) #really hokey and assumes that the middle value of the vector is the median
      }else{
        parval <- vals[1,imodel]
        parSD <- valSDs[1,imodel]
        if(!is.na(parSD) && parSD>0){
          if(nsexes[imodel]==1 &&  grepl("SPB",parname)) {   #divide by 2 for feamle only spawning biomass
            parval <- parval/2
            parSD <- parSD/2
          }
          #x2 <- parval+(-2:2)*parSD # 1 and 2 SDs away from mean to plot symbols
          x2 <- qnorm(symbolsQuants,parval,parSD)
          mle <- dnorm(x,parval,parSD)  # smooth line
          mle2 <- dnorm(x2,parval,parSD) # symbols
          mlescale <- 1/(sum(mle)*mean(diff(x)))
          mle <- mle*mlescale
          mle2 <- mle2*mlescale
          polygon(c(x[1],x,rev(x)[1]),c(0,mle,0),col=shadecol[iline],border=NA)
          lines(x,mle,col=col[iline],lwd=2)
          points(x2,mle2,col=col[iline],pch=pch[iline])
          lines(rep(parval,2),c(0,dnorm(parval,parval,parSD)*mlescale),col=col[iline]) #
                #,pch=pch[iline],type='o')
        }else{
          abline(v=parval,col=col[iline])
        }
      }
      abline(h=0,col="grey")
      xticks <- pretty(xlim)
      axis(1,at=xticks,lab=format(xticks/xunits))
      mtext(side=2,line=1,labels[8])
      box()
      legendfun()
    }
  } # end plotDensities function
  

  # subplot 1: spawning biomass
  if(1 %in% subplots){
    if(plot) plotSpawnBio(uncertainty=FALSE)
    if(print){
      pngfun("compare1_spawnbio.png")
      plotSpawnBio(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 2: spawning biomass with uncertainty intervals
  if(2 %in% subplots & uncertainty){
    if(plot) plotSpawnBio(uncertainty=uncertainty)
    if(print){
      pngfun("compare2_spawnbio_uncertainty.png")
      plotSpawnBio(uncertainty=uncertainty)
      dev.off()
    }
  }

  # subplot 3: biomass ratio (hopefully equal to spawning depletion)
  if(3 %in% subplots){
    if(plot) plotBratio(uncertainty=FALSE)
    if(print){
      pngfun("compare3_Bratio.png")
      plotBratio(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 4: biomass ratio with uncertainty
  if(4 %in% subplots){
    if(plot) plotBratio(uncertainty=uncertainty)
    if(print){
      pngfun("compare4_Bratio_uncertainty.png")
      plotBratio(uncertainty=uncertainty)
      dev.off()
    }
  }

  # subplot 5: SPR ratio
  if(5 %in% subplots){
    if(plot) plotSPRratio(uncertainty=FALSE)
    if(print){
      pngfun("compare5_SPRratio.png")
      plotSPRratio(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 6: SPR ratio with uncertainty
  if(6 %in% subplots){
    if(plot) plotSPRratio(uncertainty=uncertainty)
    if(print){
      pngfun("compare6_SPRratio_uncertainty.png")
      plotSPRratio(uncertainty=uncertainty)
      dev.off()
    }
  }

  # subplot 7: recruits
  if(7 %in% subplots){
    if(plot) plotRecruits(uncertainty=FALSE)
    if(print){
      pngfun("compare7_recruits.png")
      plotRecruits(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 8: recruits with uncertainty
  if(8 %in% subplots){
    if(plot) plotRecruits()
    if(print){
      pngfun("compare8_recruits_uncertainty.png")
      plotRecruits()
      dev.off()
    }
  }
  
  # subplot 9: recruit devs
  if(9 %in% subplots){
    if(plot) plotRecDevs(uncertainty=FALSE)
    if(print){
      pngfun("compare9_recdevs.png")
      plotRecDevs(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 10: recruit devs with uncertainty
  if(10 %in% subplots){
    if(plot) plotRecDevs()
    if(print){
      pngfun("compare10_recdevs_uncertainty.png")
      plotRecDevs()
      dev.off()
    }
  }
  
  # subplot 11: index fits
  if(11 %in% subplots){
    if(plot) plotIndices()
    if(print){
      pngfun("compare11_indices.png")
      plotIndices()
      dev.off()
    }
  }

  # subplot 12: index fits
  if(12 %in% subplots){
    if(plot) plotIndices(log=TRUE)
    if(print){
      pngfun("compare12_indices_log.png")
      plotIndices(log=TRUE)
      dev.off()
    }
  }

  # subplot 13: densities
  if(13 %in% subplots){
    ndensities <- length(densitynames)
    for(iplot in 1:ndensities){
      name <- densitynames[iplot]
      if(plot) {
        plotDensities(parname=name,xlab=densityxlabs[iplot])
      }
      if(print){
        pngfun(paste("compare13_densities_",name,".png"))
        plotDensities(parname=name,xlab=densityxlabs[iplot])
        dev.off()
      }
    }
  }

}
