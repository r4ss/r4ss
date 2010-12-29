SSplotComparisons <- function(summaryoutput,subplots=1:5,
                              plot=TRUE,print=FALSE,
                              models="all",
                              labels=c("Year",             #1
                                "Spawning biomass (mt)",   #2
                                "Spawning depletion",      #3
                                "Age-0 recruits (1,000s)", #4
                                "Recruitment deviations",  #5
                                "Index"),                  #6
                              col="default",
                              pch="default",
                              lty=1,
                              lwd=2,
                              type="o",
                              legend=TRUE,
                              legendlabels="default",
                              legendloc="topright",
                              pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
                              plotdir="workingdirectory",
                              verbose=TRUE){
  # the summaryoutput
  if(!is.list(summaryoutput) | names(summaryoutput)[1]!="n")
    stop("'summaryoutput' should be the result of the SSsummarize function")

  # subfunction to write png files
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  # subfunction to add legend
  legendfun <- function() legend(legendloc, legend=legendlabels, col=col, lty=lty, lwd=lwd, pch=pch, bty='n')
  
  # get stuff from summary output
  n         <- summaryoutput$n
  pars      <- summaryoutput$pars
  parstds   <- summaryoutput$parstds
  parphases <- summaryoutput$parphases
  quants    <- summaryoutput$quants
  quantstds <- summaryoutput$quantstds
  recdevs   <- summaryoutput$recdevs
  SpawnBio  <- summaryoutput$SpawnBio
  
  if(models[1]=="all") models <- 1:n
  nlines <- length(models)

  if(col[1]=="default") col <- rich.colors.short(nlines)
  if(pch[1]=="default") pch <- 1:nlines
  if(lty[1]=="default") lty <- 1:nlines

  if(length(col) < nlines) col <- rep(col,nlines)
  if(length(pch) < nlines) pch <- rep(pch,nlines)
  if(length(lty) < nlines) lty <- rep(lty,nlines)
  if(length(lwd) < nlines) lwd <- rep(lwd,nlines)
  
  if(legendlabels[1]=="default") legendlabels <- paste("model",1:nlines)
  if(plotdir=="workingdirectory") plotdir <- getwd()

  if(plot) windows(record=TRUE)

  plotSpawnBio <- function(){ # spawning biomass
    matplot(SpawnBio$Yr,SpawnBio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,
            ylim=range(0,SpawnBio[,models],na.rm=TRUE),xlab=labels[1],ylab=labels[2])
    abline(h=0,col='grey')
    if(legend) legendfun()
  }

  plotDepl <- function(){
    depl <- SpawnBio
    for(i in 1:n){
      depl[,i] <- depl[,i]/depl[!is.na(depl[,i]),i][1]
    }
    matplot(depl$Yr,depl[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,
            ylim=range(0,depl[,models],na.rm=TRUE),xlab=labels[1],ylab=labels[3])
    abline(h=0,col='grey')
    if(legend) legendfun()
  }

  plotRecDevs <- function(){
    # empty plot
    plot(0,xlim=range(recdevs$Yr),ylim=c(-1,1)*max(abs(recdevs[,models]),na.rm=TRUE),
         type='n',xlab=labels[1],ylab=labels[5])
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

  # subplot 1: spawning biomass
  if(1 %in% subplots){
    if(plot) plotSpawnBio()
    if(print){
      pngfun("compare1_spawnbio.png")
      plotSpawnBio()
      dev.off()
    }
  }

  # subplot 2: spawning depletion
  if(2 %in% subplots){
    if(plot) plotDepl()
    if(print){
      pngfun("compare2_depl.png")
      plotDepl()
      dev.off()
    }
  }

  # subplot 3: recruits
  if(3 %in% subplots){
  }

  # subplot 4: recruit devs
  if(4 %in% subplots){
    if(plot) plotRecDevs()
    if(print){
      pngfun("compare4_recdevs.png")
      plotRecDevs()
      dev.off()
    }
  }

  # subplot 5: index fits
  if(5 %in% subplots){
  }

}
