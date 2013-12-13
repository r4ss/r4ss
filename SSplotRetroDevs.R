# this function replaced by SSplotRetroRecruits

SSplotRetroDevs <- function(retroSummary,endyrvec,cohorts,ylim=c(-3,3),uncertainty=FALSE,
                            labels=c('Recruitment deviation relative to recent estimate',
                              'Recruitment deviation',
                              'Age'),
                            main="Retrospective analysis of recruitment deviations",
                            mcmcVec=FALSE,
                            relative=FALSE,labelyears=TRUE,legend=FALSE,leg.ncols=4){

  
  addpoly <- function(yrvec, lower, upper, shadecol=rgb(0,0,0,.1),col=1){
    # add shaded uncertainty intervals behind line
    # modified from SSplotComparisons in r4ss package
    polygon(x=c(yrvec,rev(yrvec)),
            y=c(lower,rev(upper)),
            border=NA,col=shadecol)
    lines(yrvec,lower,lty=3,col=col)
    lines(yrvec,upper,lty=3,col=col)
  }

  n             <- retroSummary$n
  recdevs       <- retroSummary$recdevs
  recdevsLower  <- retroSummary$recdevsLower
  recdevsUpper  <- retroSummary$recdevsUpper
  lowerCI       <- retroSummary$lowerCI
  upperCI       <- retroSummary$upperCI

  colvec      <- rich.colors.short(length(cohorts),alpha=.7)
  shadecolvec <- rich.colors.short(length(cohorts),alpha=.1)
  colvec      <- rainbow(length(cohorts),alpha=.7)
  shadecolvec <- rainbow(length(cohorts),alpha=.1)
  colvec.txt <- colvec
  # make text darker
  for(i in 1:length(colvec)){
    tmp <- col2rgb(colvec[i])/255
    colvec.txt[i] <- rgb(tmp[1]/2,tmp[2]/2,tmp[3]/2,alpha=.7)
  }
  print(cbind(colvec,colvec.txt))
  
  ylab <- ifelse(relative,labels[1],labels[2])
  maxage <- max(endyrvec)-min(cohorts)
  xlim <- c(0,maxage)
  if(labelyears) xlim <- xlim + c(-.8,.8) # expand x-axis to make room for labels
  
  plot(0,type='n',xlim=xlim,ylim=ylim,xlab=labels[3],
       ylab=ylab,main=main,axes=FALSE)
  axis(1,at=0:maxage)
  axis(2,at=ylim[1]:ylim[2],las=1)
  abline(h=0,col='grey')
  box()
  
  if(legend) ylim <- ylim + c(0,1)

  if(length(mcmcVec)==1) mcmcVec <- rep(mcmcVec,n)
  if(any(mcmcVec)) mcmc <- retroSummary$mcmc
  for(imodel in (1:n)[mcmcVec]){
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
  
  for(iy in 1:length(cohorts)){
    y <- cohorts[iy]
    cohortdevs      <- recdevs[recdevs$Yr==y,1:n]
    cohortdevsLower <- recdevsLower[recdevsLower$Yr==y,1:n]
    cohortdevsUpper <- recdevsUpper[recdevsUpper$Yr==y,1:n]
    # combine rows where the parameter labels may differ
    if(nrow(cohortdevs)>1){
      cohortdevs2      <- rep(NA,n)
      cohortdevsLower2 <- rep(NA,n)
      cohortdevsUpper2 <- rep(NA,n)
      for(icol in 1:n){
        cohortdevs2[icol]      <- cohortdevs[!is.na(cohortdevs[,icol]),icol]
        cohortdevsLower2[icol] <- cohortdevsLower[!is.na(cohortdevsLower[,icol]),icol]
        cohortdevsUpper2[icol] <- cohortdevsUpper[!is.na(cohortdevsUpper[,icol]),icol]
      }
      cohortdevs <- cohortdevs2
      cohortdevsLower <- cohortdevsLower2
      cohortdevsUpper <- cohortdevsUpper2
    }
    cohortdevs <- as.numeric(cohortdevs)
    cohortdevsLower <- as.numeric(cohortdevsLower)
    cohortdevsUpper <- as.numeric(cohortdevsUpper)

    goodmodels <- (1:n)[endyrvec-y>=0]
    if(relative){
      #relative to final estimate
      if(uncertainty)
        addpoly(yrvec=endyrvec[goodmodels] - y,
                lower=cohortdevsLower[goodmodels] - cohortdevs[max(goodmodels)],
                upper=cohortdevsUpper[goodmodels] - cohortdevs[max(goodmodels)],
                shadecol=shadecolvec[iy],col=colvec[iy])
      lines(endyrvec[goodmodels] - y,
            cohortdevs[goodmodels] - cohortdevs[max(goodmodels)],
            type='o',col=colvec[iy],lwd=3,pch=16)
      if(labelyears)
        text(x=(endyrvec[goodmodels] - y)[1] - 0.5,
             y=(cohortdevs[goodmodels] - cohortdevs[max(goodmodels)])[1],
             labels=y,
             col=colvec.txt[iy],
             cex=.7)
    }else{
      #true value
      if(uncertainty)
        addpoly(yrvec=endyrvec[goodmodels] - y,
                lower=cohortdevsLower[goodmodels],
                upper=cohortdevsUpper[goodmodels],
                shadecol=shadecolvec[iy],col=colvec[iy])
      lines(endyrvec[goodmodels] - y,
            cohortdevs[goodmodels],
            type='o',col=colvec[iy],lwd=3,pch=16)
      if(labelyears)
        text(x=rev(endyrvec[goodmodels] - y)[1] + 0.5,
             y=rev(cohortdevs[goodmodels])[1],
             labels=y,
             col=colvec.txt[iy],
             cex=.7)
    }
  }
  if(legend) legend('topright',lwd=3,lty=1,pch=16,col=colvec,legend=cohorts,
                    title='Cohort birth year',ncol=leg.ncols,
                    bg=rgb(1,1,1,.3),box.col=NA)
}

## if(FALSE){
##   #### example use
##   # source this file
##   source('c:/SS/hake/Hake_2012/retro/retro_script.R')

##   # move to directory one level above existing model run
##   setwd('C:/ss/hake/Hake_2013/runs/')

##   # run the function above
##   SS_doRetro(olddir='2013hake_12',years=0:-10)
##   # read in output
##   retroModels <- SSgetoutput(dirvec=paste('retrospectives/retro',-10:0,sep=''))
##   # summarize output
##   retroSummary <- SSsummarize(retroModels)

##   # set the ending year of each model in the set
##   endyrvec <- retroModels[[1]]$endyr-10:0
##   # make comparison plot
##   pdf('retrospectives/retrospective_comparison_plots.pdf')
##   SSplotComparisons(retroSummary,endyrvec=endyrvec,new=FALSE)
##   dev.off()

##   # make Ianelli-style plot of recdev retrospectives 
##   pdf('retrospectives/retrospective_dev_plots.pdf',width=7,height=10)
##   par(mfrow=c(2,1))
##   # first scaled relative to most recent estimate
##   SSplotRetroDevs(retroSummary, endyrvec=endyrvec, cohorts=1999:2012, relative=TRUE, legend=FALSE)
##   # second without scaling
##   SSplotRetroDevs(retroSummary, endyrvec=endyrvec, cohorts=1999:2012, relative=FALSE, legend=FALSE)
##   dev.off()

## }

