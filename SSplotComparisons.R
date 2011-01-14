SSplotComparisons <-
  function(summaryoutput,subplots=1:10,
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
           densityxlabs=c("B0","Spawning Biomass in 2011","depletion in 2011","log(R0)","MSY"),
           densityscalex=1,
           densityscaley=1,
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
  recruits      <- summaryoutput$recruits
  recdevs       <- summaryoutput$recdevs
  indices       <- summaryoutput$indices
  mcmc          <- summaryoutput$mcmc               #a list of dataframes, 1 for each model with mcmc output

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
      recruits[recruits$Yr > endyr, imodel] <- NA
      recdevs[recdevs$Yr > endyr, imodel] <- NA
    }
  }
  
  if(col[1]=="default" & nlines>3) col <- rc(nlines+1)[-1]
  if(col[1]=="default" & nlines==2) col <- rc(nlines)
  if(col[1]=="default" & nlines==3) col <- c("blue","red","green3")
  if(shadecol[1]=="default" & nlines>3) shadecol <- rc(nlines+1,alpha=shadealpha)[-1]
  if(shadecol[1]=="default" & nlines==2) shadecol <- rc(nlines,alpha=shadealpha)
  if(shadecol[1]=="default" & nlines==3) shadecol <- rgb(red=c(0,1,0),green=c(0,0,0.8),blue=c(1,0,0),alpha=shadealpha)

  if(pch[1]=="default") pch <- 1:nlines
  if(lty[1]=="default") lty <- 1:nlines

  if(length(col) < nlines) col <- rep(col,nlines)
  if(length(pch) < nlines) pch <- rep(pch,nlines)
  if(length(lty) < nlines) lty <- rep(lty,nlines)
  if(length(lwd) < nlines) lwd <- rep(lwd,nlines)
  
  if(legendlabels[1]=="default") legendlabels <- paste("model",1:nlines)

  # determine operating system and open new window if requested
  if(length(grep("linux",version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw",version$os)) > 0) OS <- "Windows"
  # need appropriate line to support Mac operating systems

  if(plot & new){
    if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    if(OS=="Windows") windows(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
    if(OS=="Linux") X11(width=pwidth,height=pheight,pointsize=ptsize)
    if(OS=="Mac") quartz(width=pwidth,height=pheight,pointsize=ptsize)
  }

  if(mcmcVec[1]=="default") mcmcVec <- rep(F,nlines)

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
    if(xlim[1]=="default"){
      xlim <- range(SpawnBio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    ylim <- range(0, SpawnBio[,models], na.rm=TRUE)
    if(uncertainty) {
        #use mcmc results in available
        for(iline in 1:nlines){
            imodel <- models[iline]
            if(mcmcVec[iline]) {
                tmp <- grep("SPB",names(mcmc[[imodel]]))   #try it to see what you get
                if(length(tmp) > 0) {   #there are some mcmc values to use
                    mcmclabs <- names(mcmc[[imodel]][tmp])
                    lower <- apply(mcmc[[imodel]][,grep("SPB",names(mcmc[[imodel]]))],2,quantile,prob=0.025)   #hard-wired probability
                    med <- apply(mcmc[[imodel]][,grep("SPB",names(mcmc[[imodel]]))],2,quantile,prob=0.5)   #hard-wired probability
                    upper <- apply(mcmc[[imodel]][,grep("SPB",names(mcmc[[imodel]]))],2,quantile,prob=0.975)   #hard-wired probability
                    if(nsexes[iline] == 1) {
                        lower <- lower/2
                        upper <- upper/2
                        med <- med/2
                    }
                    SpawnBioLower[,imodel] <- SpawnBioUpper[,imodel] <- SpawnBio[,imodel] <- NA
                    SpawnBio[,imodel] <- med[match(SpawnBio$Label,mcmclabs)]
                    SpawnBioLower[,imodel] <- lower[match(SpawnBioLower$Label,mcmclabs)]
                    SpawnBioUpper[,imodel] <- upper[match(SpawnBioUpper$Label,mcmclabs)]
                }
            }
        }
        ylim <- range(ylim, SpawnBioUpper[,models], na.rm=TRUE)
    }
    plot(0,type="n",xlim=xlim,ylim=ylim,xlab=labels[1],ylab=labels[2],xaxs=xaxs,yaxs=yaxs)
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
  }

  plotBratio <- function(uncertainty=TRUE){ # plot biomass ratio (may be identical to previous plot)
    if(xlim[1]=="default"){
      xlim <- range(Bratio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    ylim <- range(0, Bratio[,models], na.rm=TRUE)
    if(uncertainty) {
        #use mcmc results in available
        for(iline in 1:nlines){
            imodel <- models[iline]
            if(mcmcVec[iline]) {
                tmp <- grep("Bratio",names(mcmc[[imodel]]))   #try it to see what you get
                if(length(tmp) > 0) {   #there are some mcmc values to use
                    mcmclabs <- names(mcmc[[imodel]][tmp])
                    lower <- apply(mcmc[[imodel]][,grep("Bratio",names(mcmc[[imodel]]))],2,quantile,prob=0.025)   #hard-wired probability
                    med <- apply(mcmc[[imodel]][,grep("Bratio",names(mcmc[[imodel]]))],2,quantile,prob=0.5)   #hard-wired probability
                    upper <- apply(mcmc[[imodel]][,grep("Bratio",names(mcmc[[imodel]]))],2,quantile,prob=0.975)   #hard-wired probability
                    BratioLower[,imodel] <- BratioUpper[,imodel] <- SpawnBio[,imodel] <- NA
                    Bratio[,imodel] <- med[match(Bratio$Label,mcmclabs)]
                    BratioLower[,imodel] <- lower[match(BratioLower$Label,mcmclabs)]
                    BratioUpper[,imodel] <- upper[match(BratioUpper$Label,mcmclabs)]
                }
            }
        }
        ylim=range(ylim, BratioUpper[,models], na.rm=TRUE)
    }
    plot(0,type="n",xlim=xlim,ylim=ylim,xlab=labels[1],ylab=labels[3],xaxs=xaxs,yaxs=yaxs)
    if(uncertainty) addpoly(Bratio$Yr, lower=BratioLower, upper=BratioUpper)
    matplot(Bratio$Yr,Bratio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,add=TRUE)
    abline(h=0,col="grey")
    abline(h=1,col="grey",lty=2)

    if(btarg>0){
      abline(h=btarg,col="red")
      text(min(Bratio$Yr)+4,btarg+0.03,"Management target",adj=0)
    }
    if(minbthresh>0){
      abline(h=minbthresh,col="red")
      text(min(Bratio$Yr)+4,minbthresh+0.03,"Minimum stock size threshold",adj=0)
    }

    if(legend) legendfun()
  }

  plotRecruits <- function(){ # plot recruitment
    if(xlim[1]=="default"){
      xlim <- range(recruits$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    # plot lines showing recruitment
    matplot(recruits$Yr[-(1:2)],recruits[-(1:2),models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,
            xlim=xlim,ylim=range(0,recruits[,models],na.rm=TRUE),
            xlab=labels[1],ylab=labels[4],xaxs=xaxs,yaxs=yaxs)
    # add points at equilibrium values
    points(x=rep(recruits$Yr[1],nlines), recruits[1, models], col=col, pch=pch, cex=1.2, lwd=lwd)
    abline(h=0,col="grey")
    if(legend) legendfun()
  }

  plotRecDevs <- function(){ # plot recruit deviations
    # empty plot
    if(xlim[1]=="default"){
      xlim <- range(recdevs$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }

    plot(0,xlim=xlim,ylim=c(-1,1)*max(abs(recdevs[,models]),na.rm=TRUE),
         type="n",xlab=labels[1],ylab=labels[5],xaxs=xaxs,yaxs=yaxs)
    abline(h=0,col="grey")
    # loop over vector of models to add lines
    for(iline in 1:nlines){
      imodel <- models[iline]
      yvec <- recdevs[,imodel]
      xvec <- recdevs$Yr[!is.na(yvec)]
      yvec <- yvec[!is.na(yvec)]
      lines(xvec,yvec,pch=pch[iline],lwd=lwd[iline],
            lty=lty[iline],col=col[iline],type=type)
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
    for(iline in 1:nlines){
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
        for(iline in 1:nlines) {
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
    for(iline in 1:nlines){
      imodel <- models[iline]
      if(mcmcVec[iline]) {
        mcmcColumn <- grep(parname,colnames(mcmc[[imodel]]))
        if(length(mcmcColumn)!=1) {
            cat("Too many columns selected from MCMC for model",imodel,". Please specify a unique label in the mcmc dataframe\nor specify mcmcVec=F for model",imodel,"or specify mcmcVec='default'.\n")
            return(NULL) #exit this function
        }
        mcmcVals <- mcmc[[imodel]][,mcmcColumn]
        if(nsexes[imodel]==1 &&  length(grep("SPB",parname))>0) {   #divide by 2 for feamle only spawning biomass
            mcmcVals <- mcmcVals/2
        }
        xmin <- min(xmin, quantile(mcmcVals,0.001))
        xmax <- max(xmax, quantile(mcmcVals,0.999))
        z <- density(mcmcVals,from=0)      #density estimate of mcmc sample (posterior)
        z$x <- z$x[c(1,1:length(z$x),length(z$x))]
        z$y <- c(0,z$y,0)           #just to make sure that a good looking polygon is created
        ymax <- max(ymax,max(z$y))  #update ymax
        mcmcDens[[iline]] <- z      #save the density estimate for later plotting
      }else{
        parval <- vals[1,imodel]
        parSD <- valSDs[1,imodel]
        if(!is.numeric(parval)) parval <- -1     #do this in case models added without the parameter
        if(!is.na(parSD) && parSD>0){ # if non-zero SD available
          if(nsexes[imodel]==1 &&  length(grep("SPB",parname))>0) {   #divide by 2 for feamle only spawning biomass
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
    if(length(grep("Bratio",parname))>0) xmin <- 0 # xmin=0 for depletion plots
    if(limit0) xmin <- max(0,xmin) # by default no plot can go below 0 
      
    # make empty plot
    plot(0,type="n",xlim=c(xmin,xmin+(xmax-xmin)*densityscalex),axes=FALSE,xaxs="i",
         ylim=c(0,1.1*ymax*densityscaley),xlab=xlab,ylab="")
    x <- seq(xmin,xmax,length=500)

    # add vertical lines for target and threshold depletion values
    if(length(grep("Bratio",parname))>0){
      if(btarg>0){
        abline(v=btarg,col="red")
        text(btarg+0.03,par()$usr[4],"Management target",adj=1.05,srt=90)
      }
      if(minbthresh>0){
        abline(v=minbthresh,col="red")
        text(minbthresh+0.03,par()$usr[4],"Minimum stock size threshold",adj=1.05,srt=90)
      }
    }
      
    symbolsQuants <- c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)
    # loop again to make plots
    for(iline in 1:nlines){
      imodel <- models[iline]
      if(mcmcVec[iline]) {
        mcmcColumn <- grep(parname,colnames(mcmc[[imodel]]))
        mcmcVals <- mcmc[[imodel]][,mcmcColumn]
        if(nsexes[imodel]==1 &&  length(grep("SPB",parname))>0) {   #divide by 2 for feamle only spawning biomass
            mcmcVals <- mcmcVals/2
        }
        print("here")
        print(x2)
        x2 <- quantile(mcmcVals,symbolsQuants)   # for symbols on plot
        print(x2)
        #find the positions in the density that are closest to these quantiles
        x <- mcmcDens[[iline]]$x
        y <- mcmcDens[[iline]]$y
        yscale <- 1/(sum(y)*mean(diff(x)))
        y <- y*yscale
        y2 <- NULL
        for(ii in x2) {
            y2 <- c(y2,y[abs(x-ii)==min(abs(x-ii))])
        }
        print(x);print(y);print(x2);print(y2)
        polygon(c(x[1],x,rev(x)[1]),c(0,y,0),col=shadecol[iline],border=NA)
        lines(x,y,col=col[iline],lwd=2)
        points(x2,y2,col=col[iline],pch=pch[iline])
        lines(rep(x2[median(1:length(x2))],2),c(0,y2[median(1:length(x2))]),col=col[iline]) #really hokey and assumes that the middle value of the vector is the median
      }else{
        parval <- vals[1,imodel]
        parSD <- valSDs[1,imodel]
        if(!is.na(parSD) && parSD>0){
          if(nsexes[imodel]==1 &&  length(grep("SPB",parname))>0) {   #divide by 2 for feamle only spawning biomass
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
      axis(1)
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

  ## # subplot 3: spawning depletion
  # removed because subplot 4 turned out to be better and redundant

  # subplot 4: biomass ratio (probably equal to spawning depletion)
  if(4 %in% subplots){
    if(plot) plotBratio(uncertainty=FALSE)
    if(print){
      pngfun("compare4_Bratio.png")
      plotBratio(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 5: biomass ratio with uncertainty
  if(5 %in% subplots){
    if(plot) plotBratio(uncertainty=uncertainty)
    if(print){
      pngfun("compare5_Bratio_uncertainty.png")
      plotBratio(uncertainty=uncertainty)
      dev.off()
    }
  }
  
  # subplot 6: recruits
  if(6 %in% subplots){
    if(plot) plotRecruits()
    if(print){
      pngfun("compare6_recruits.png")
      plotRecruits()
      dev.off()
    }
  }

  # subplot 7: recruit devs
  if(7 %in% subplots){
    if(plot) plotRecDevs()
    if(print){
      pngfun("compare7_recdevs.png")
      plotRecDevs()
      dev.off()
    }
  }

  # subplot 8: index fits
  if(8 %in% subplots){
    if(plot) plotIndices()
    if(print){
      pngfun("compare8_indices.png")
      plotIndices()
      dev.off()
    }
  }

  # subplot 9: index fits
  if(9 %in% subplots){
    if(plot) plotIndices(log=TRUE)
    if(print){
      pngfun("compare9_indices_log.png")
      plotIndices(log=TRUE)
      dev.off()
    }
  }

  # subplot 10: B0 densities
  if(10 %in% subplots){
    ndensities <- length(densitynames)
    for(iplot in 1:ndensities){
      name <- densitynames[iplot]
      if(plot) {
        plotDensities(parname=name,xlab=densityxlabs[iplot])
      }
      if(print){
        pngfun(paste("compare10_densities_",name,".png"))
        plotDensities(parname=name,xlab=densityxlabs[iplot])
        dev.off()
      }
    }
  }

}
