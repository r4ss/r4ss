SSplotRetroDevs <- function(retroSummary,endyrvec,cohorts,ylim=c(-3,3),
                            relative=FALSE,labels=TRUE,legend=FALSE,leg.ncols=4){
  n <- retroSummary$n
  colvec <- rich.colors.short(length(cohorts),alpha=.7)
  ylab <- ifelse(relative,
                 'Recruitment deviation relative to recent estimate',
                 'Recruitment deviation')
  xlim <- c(0,max(endyrvec)-min(cohorts))
  if(labels) xlim <- xlim + c(-.8,.8) # expand x-axis to make room for labels
  if(legend) ylim <- ylim + c(0,1)
  
  plot(0,type='n',xlim=xlim,ylim=ylim,xlab='Age',ylab=ylab,main=ylab)
  abline(h=0,col='grey',lty=3)
  for(iy in 1:length(cohorts)){
    y <- cohorts[iy]
    cohortdevs <- retroSummary$recdevs[retroSummary$recdevs$Yr==y,1:n]
    # combine rows where the parameter labels may differ
    if(nrow(cohortdevs)>1){
      cohortdevs2 <- rep(NA,n)
      for(icol in 1:n) cohortdevs2[icol] <- cohortdevs[!is.na(cohortdevs[,icol]),icol]
      cohortdevs <- cohortdevs2
    }
    cohortdevs <- as.numeric(cohortdevs)

    goodmodels <- (1:n)[endyrvec-y>=0]
    if(relative){
      #relative to final estimate
      lines(endyrvec[goodmodels] - y,
            cohortdevs[goodmodels] - cohortdevs[max(goodmodels)],
            type='o',col=colvec[iy],lwd=3,pch=16)
      if(labels) text(x=(endyrvec[goodmodels] - y)[1] - 0.5,
                      y=(cohortdevs[goodmodels] - cohortdevs[max(goodmodels)])[1],
                      labels=y,
                      col=colvec[iy],
                      cex=.7)
    }else{
      #true value
      lines(endyrvec[goodmodels] - y,
            cohortdevs[goodmodels],
            type='o',col=colvec[iy],lwd=3,pch=16)
      if(labels) text(x=rev(endyrvec[goodmodels] - y)[1] + 0.5,
                      y=rev(cohortdevs[goodmodels])[1],
                      labels=y,
                      col=colvec[iy],
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

