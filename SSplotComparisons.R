SSplotComparisons <-
  function(summaryoutput,subplots=1:9,
           plot=TRUE,print=FALSE,
           models="all",
           endyrvec=NULL,
           indexfleets="default",
           indexUncertainty=FALSE,
           indexSEvec="default",
           labels=c("Year",             #1
             "Spawning biomass (mt)",   #2
             "Spawning depletion",      #3
             "Age-0 recruits (1,000s)", #4
             "Recruitment deviations",  #5
             "Index",                   #6
             "Log index"),              #7
           col="default", shadecol="default",
           pch="default", lty=1, lwd=2,
           xlim="default", xaxs='r', yaxs='r',
           type="o", uncertainty=TRUE, shadealpha=0.1,
           legend=TRUE, legendlabels="default", legendloc="topright",
           btarg=0.4, minbthresh=0.25,
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           plotdir="workingdirectory",
           new=TRUE,
           verbose=TRUE)
{
  # the summaryoutput
  if(!is.list(summaryoutput) | names(summaryoutput)[1]!="n")
    stop("'summaryoutput' should be the result of the SSsummarize function")

  # subfunction to write png files
  pngfun <- function(file) png(file=paste(plotdir,file,sep="/"),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  # subfunction to add legend
  legendfun <- function() legend(legendloc, legend=legendlabels, col=col, lty=lty, lwd=lwd, pch=pch, bty='n')
  
  # get stuff from summary output
  n           <- summaryoutput$n
  pars        <- summaryoutput$pars
  parsSD      <- summaryoutput$parsSD
  parphases   <- summaryoutput$parphases
  quants      <- summaryoutput$quants
  quantsSD    <- summaryoutput$quantsSD
  SpawnBio    <- summaryoutput$SpawnBio
  SpawnBioSD  <- summaryoutput$SpawnBioSD
  Bratio      <- summaryoutput$Bratio
  BratioSD    <- summaryoutput$BratioSD
  recruits    <- summaryoutput$recruits
  recdevs     <- summaryoutput$recdevs
  indices     <- summaryoutput$indices
  
  if(models[1]=="all") models <- 1:n
  nlines <- length(models)
  if(length(endyrvec)==1) endyrvec <- rep(endyrvec,nlines)
  if(!is.null(endyrvec)){
    for(iline in 1:nlines){
      endyr <- endyrvec[iline]
      imodel <- models[iline]
      SpawnBio[SpawnBio$Yr > endyr, imodel] <- NA
      SpawnBioSD[SpawnBio$Yr > endyr, imodel] <- NA
      Bratio[Bratio$Yr > endyr, imodel] <- NA
      BratioSD[Bratio$Yr > endyr, imodel] <- NA
      recruits[recruits$Yr > endyr, imodel] <- NA
      recdevs[recdevs$Yr > endyr, imodel] <- NA
    }
  }
  
  if(col[1]=="default" & nlines!=2) col <- rich.colors.short(nlines+1)[-1]
  if(col[1]=="default" & nlines==2) col <- rich.colors.short(nlines)
  if(shadecol[1]=="default") shadecol <- rich.colors.short(nlines+1,alpha=shadealpha)[-1]
  if(pch[1]=="default") pch <- 1:nlines
  if(lty[1]=="default") lty <- 1:nlines

  if(length(col) < nlines) col <- rep(col,nlines)
  if(length(pch) < nlines) pch <- rep(pch,nlines)
  if(length(lty) < nlines) lty <- rep(lty,nlines)
  if(length(lwd) < nlines) lwd <- rep(lwd,nlines)
  
  if(legendlabels[1]=="default") legendlabels <- paste("model",1:nlines)
  if(plotdir=="workingdirectory") plotdir <- getwd()

  if(plot & new){
    if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    windows(record=TRUE)
  }

  addpoly <- function(vals, SDs, yrvec){ # add shaded uncertainty intervals behind line
    upper <- vals[,1:n] + 1.96*SDs[,1:n]
    lower <- vals[,1:n] - 1.96*SDs[,1:n]
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
  
  plotSpawnBio <- function(uncertainty=TRUE){ # plot spawning biomass
    if(xlim[1]=="default"){
      xlim <- range(SpawnBio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    plot(0,type='n',xlim=xlim,ylim=range(0,SpawnBio[,models],na.rm=TRUE),
         xlab=labels[1],ylab=labels[2],xaxs=xaxs,yaxs=yaxs)
    if(uncertainty) addpoly(SpawnBio, SpawnBioSD, SpawnBio$Yr)
    matplot(SpawnBio$Yr,SpawnBio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,
            add=TRUE)
    abline(h=0,col='grey')
    if(legend) legendfun()
  }

  plotDepl <- function(){ # plot spawning depletion
    depl <- SpawnBio
    if(xlim[1]=="default"){
      xlim <- range(SpawnBio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    for(i in 1:n){
      depl[,i] <- depl[,i]/depl[!is.na(depl[,i]),i][1]
    }
    plot(0,type='n',xlim=xlim,ylim=range(0,Bratio[,models],na.rm=TRUE),
         xlab=labels[1],ylab=labels[3],xaxs=xaxs,yaxs=yaxs)
    matplot(depl$Yr,depl[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,add=TRUE)
    abline(h=0,col='grey')

    if(btarg>0){
      abline(h=btarg,col="red")
      text(min(depl$Yr)+4,btarg+0.03,"Management target",adj=0)
    }
    if(minbthresh>0){
      abline(h=minbthresh,col="red")
      text(min(depl$Yr)+4,minbthresh+0.03,"Minimum stock size threshold",adj=0)
    }

    if(legend) legendfun()
  }

  plotBratio <- function(uncertainty=TRUE){ # plot biomass ratio (may be identical to previous plot)
    if(xlim[1]=="default"){
      xlim <- range(Bratio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    plot(0,type='n',xlim=xlim,ylim=range(0,Bratio[,models],na.rm=TRUE),
         xlab=labels[1],ylab=labels[3],xaxs=xaxs,yaxs=yaxs)
    if(uncertainty) addpoly(Bratio, BratioSD, Bratio$Yr)
    matplot(Bratio$Yr,Bratio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,add=TRUE)
    abline(h=0,col='grey')

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
    matplot(recruits$Yr,recruits[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,
            xlim=xlim,ylim=range(0,recruits[,models],na.rm=TRUE),
            xlab=labels[1],ylab=labels[4],xaxs=xaxs,yaxs=yaxs)
    abline(h=0,col='grey')
    if(legend) legendfun()
  }

  plotRecDevs <- function(){ # plot recruit deviations
    # empty plot
    if(xlim[1]=="default"){
      xlim <- range(recdevs$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }

    plot(0,xlim=xlim,ylim=c(-1,1)*max(abs(recdevs[,models]),na.rm=TRUE),
         type='n',xlab=labels[1],ylab=labels[5],xaxs=xaxs,yaxs=yaxs)
    abline(h=0,col='grey')
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
    }else{
      upper <- NULL
      lower <- NULL
    }
    
    # make plot
    ylim <- range(obs,exp,lower,upper)
    if(!log) ylim <- range(0,ylim) # 0 included if not in log space
    
    plot(0,type='n',xlim=range(yr),ylim=ylim,xlab="Year",ylab=ylab,axes=F)
    if(!log) abline(h=0,col='grey')
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
    if(indexUncertainty)
      arrows(x0=x, y0=lower, x1=x, y1=upper, length=0.01, angle=90, code=3, col=1)
      
    # put observed values on top
    subset <- indices2$imodel==1
    points(yr[subset],obs[subset],pch=16,cex=1.5,type='o',lty=3)

    axis(1,at=yr)
    axis(2)
    box()
  }  

  plotDensities <- function(){
    
  }

  
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

  # subplot 3: spawning depletion
  if(3 %in% subplots){
    if(plot) plotDepl()
    if(print){
      pngfun("compare3_depl.png")
      plotDepl()
      dev.off()
    }
  }
  
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
      plotDepl()
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


}
