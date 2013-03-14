SSplotRetroRecruits <-
  function(retroSummary,endyrvec,cohorts,ylim=NULL,uncertainty=FALSE,
           labels=c('Recruitment deviation',
             'Recruitment (billions)',
             'relative to recent estimate',
             'Age'),
           main="Retrospective analysis of recruitment deviations",
           mcmcVec=FALSE,devs=TRUE,
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

  # number of models
  n <- retroSummary$n

  # get either recruitment deviations or true recruitments
  if(devs){
    # devs
    recvals       <- retroSummary$recdevs
    recvalsLower  <- retroSummary$recdevsLower
    recvalsUpper  <- retroSummary$recdevsUpper
  }else{
    # recruits
    recvals       <- retroSummary$recruits
    recvalsLower  <- retroSummary$recruitsLower
    recvalsUpper  <- retroSummary$recruitsUpper
    scale <- 1e6 # should generalize this in the future for non-hake species
  }
  # lower and upper quantiles as defined in summary
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

  ylab <- ifelse(devs,labels[1],labels[2])
  if(relative) ylab <- paste(ylab,labels[3])
  
  maxage <- max(endyrvec)-min(cohorts)
  xlim <- c(0,maxage)
  if(labelyears) xlim <- xlim + c(-.8,.8) # expand x-axis to make room for labels

  # determine y-limits
  if(is.null(ylim)){
    if(uncertainty){
      ylim <- c(min(recvalsLower[,1:n]),max(recvalsUpper[,1:n]))
    }else{
      ylim <- c(min(recvals[,1:n]),max(recvals[,1:n]))
    }
    if(devs){
      ylim <- 1.1*range(c(ylim,-ylim)) # make symmetric for devs
    }else{
      if(relative){
        ylim <- c(-1.0*max(ylim),1.0*max(ylim)) # include 0 for recruitments
      }else{
        ylim <- c(0,1.1*max(ylim)) # include 0 for recruitments
      }
    }
    ylim <- ylim/scale
  }
  yticks <- NULL
  if(devs) yticks <- ylim[1]:ylim[2]

  # make empty plot with axes
  plot(0,type='n',xlim=xlim,ylim=ylim,xlab=labels[3],
       ylab=ylab,main=main,axes=FALSE)
  axis(1,at=0:maxage)
  axis(2,at=yticks,las=1)
  abline(h=0,col='grey')
  box()
  
  if(legend) ylim <- ylim + c(0,.1*(ylim[2]-ylim[1]))
  if(length(mcmcVec)==1) mcmcVec <- rep(mcmcVec,n)
  if(any(mcmcVec)) mcmc <- retroSummary$mcmc
  for(imodel in (1:n)[mcmcVec]){
    if(devs){
      tmp <- unique(c(grep("_RecrDev_",names(mcmc[[imodel]])),
                    grep("_InitAge_",names(mcmc[[imodel]])),
                    grep("ForeRecr_",names(mcmc[[imodel]]))))
    }else{
      tmp <- unique(grep("Recr_",names(mcmc[[imodel]])))
    }

    if(length(tmp) > 0) { #there are some mcmc values to use
      mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
      mcmclabs <- names(mcmc.tmp)
      lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
      med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
      upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
      recvals[,imodel] <- med[match(recvals$Label,mcmclabs)]
      recvalsLower[,imodel] <- lower[match(recvalsLower$Label,mcmclabs)]
      recvalsUpper[,imodel] <- upper[match(recvalsUpper$Label,mcmclabs)]
    }
  }
  
  for(iy in 1:length(cohorts)){
    y <- cohorts[iy]
    cohortvals      <- recvals[recvals$Yr==y,1:n]
    cohortvalsLower <- recvalsLower[recvalsLower$Yr==y,1:n]
    cohortvalsUpper <- recvalsUpper[recvalsUpper$Yr==y,1:n]
    # combine rows where the parameter labels may differ
    if(nrow(cohortvals)>1){
      cohortvals2      <- rep(NA,n)
      cohortvalsLower2 <- rep(NA,n)
      cohortvalsUpper2 <- rep(NA,n)
      for(icol in 1:n){
        cohortvals2[icol]      <- cohortvals[!is.na(cohortvals[,icol]),icol]
        cohortvalsLower2[icol] <- cohortvalsLower[!is.na(cohortvalsLower[,icol]),icol]
        cohortvalsUpper2[icol] <- cohortvalsUpper[!is.na(cohortvalsUpper[,icol]),icol]
      }
      cohortvals <- cohortvals2
      cohortvalsLower <- cohortvalsLower2
      cohortvalsUpper <- cohortvalsUpper2
    }
    cohortvals <- as.numeric(cohortvals)/scale
    cohortvalsLower <- as.numeric(cohortvalsLower)/scale
    cohortvalsUpper <- as.numeric(cohortvalsUpper)/scale

    goodmodels <- (1:n)[endyrvec-y>=0]
    if(relative){
      #relative to final estimate
      if(uncertainty)
        addpoly(yrvec=endyrvec[goodmodels] - y,
                lower=cohortvalsLower[goodmodels] - cohortvals[max(goodmodels)],
                upper=cohortvalsUpper[goodmodels] - cohortvals[max(goodmodels)],
                shadecol=shadecolvec[iy],col=colvec[iy])
      lines(endyrvec[goodmodels] - y,
            cohortvals[goodmodels] - cohortvals[max(goodmodels)],
            type='o',col=colvec[iy],lwd=3,pch=16)
      if(labelyears)
        text(x=(endyrvec[goodmodels] - y)[1] - 0.5,
             y=(cohortvals[goodmodels] - cohortvals[max(goodmodels)])[1],
             labels=y,
             col=colvec.txt[iy],
             cex=.7)
    }else{
      #true value
      if(uncertainty)
        addpoly(yrvec=endyrvec[goodmodels] - y,
                lower=cohortvalsLower[goodmodels],
                upper=cohortvalsUpper[goodmodels],
                shadecol=shadecolvec[iy],col=colvec[iy])
      lines(endyrvec[goodmodels] - y,
            cohortvals[goodmodels],
            type='o',col=colvec[iy],lwd=3,pch=16)
      if(labelyears)
        text(x=rev(endyrvec[goodmodels] - y)[1] + 0.5,
             y=rev(cohortvals[goodmodels])[1],
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

