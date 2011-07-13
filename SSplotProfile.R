SSplotProfile <-
  function(summaryoutput,subplots=1,
           plot=TRUE,print=FALSE,
           models="all",
           profile.string="steep",
           profile.label="Spawner-recruit steepness (h)",
           ylab="Change in -log-likelihood",
           components=c("TOTAL","Catch","Equil_catch","Survey","Mean_body_wt","Length_comp","Age_comp","Recruitment","Forecast_Recruitment","Parm_priors","Parm_softbounds","Parm_devs","Crash_Pen"),
           component.labels=c("Total","Catch","Equilibrium catch","Index data","Mean body weight","Length data","Age data","Recruitment","Forecast recruitment","Priors","Soft bounds","Parameter deviations","Crash penalty"),
           minfraction=0.01,
           sort.by.max.change=TRUE,
           col="default",
           pch="default",
           lty=1, lty.total=1,
           lwd=2, lwd.total=3,
           cex=1, cex.total=1.5,
           xlim="default",
           ymax="default",
           xaxs="r", yaxs="r",
           type="o",
           legend=TRUE, legendlabels="default", legendloc="topright",
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           plotdir=NULL,
           new=TRUE,
           verbose=TRUE)
{
  # subfunction to write png files
  pngfun <- function(file) png(file=paste(plotdir,file,sep="/"),width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
  if(print & is.null(plotdir)) stop("to print PNG files, you must supply a directory as 'plotdir'")

  # get stuff from summary output
  n             <- summaryoutput$n
  nsexes        <- summaryoutput$nsexes
  likelihoods   <- summaryoutput$likelihoods
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

  # find the parameter that the profile was over
  parnumber <- grep(profile.string,pars$Label)
  if(length(parnumber)<=0) stop("No parameters matching profile.string='",profile.string,"'",sep="")
  parlabel <- pars$Label[parnumber]
  if(length(parlabel) > 1)
    stop("Multiple parameters matching profile.string='",profile.string,"': ",paste(parlabel,collapse=", "),sep="")

  parvec <- as.numeric(pars[pars$Label==parlabel,1:n])
  cat("Parameter matching profile.string='",profile.string,"': '",parlabel,"'\n",sep="")
  cat("Values:\n")
  print(parvec)
  if(xlim[1]=="default") xlim <- range(parvec)

  # rearange likelihoods to be in columns by type
  prof.table <- data.frame(t(likelihoods[,1:n]))
  names(prof.table) <- likelihoods[,ncol(likelihoods)]
  
  
  # subtract minimum value from each likelihood component (over requested parameter range)
  subset <- parvec >= xlim[1] & parvec <= xlim[2]
  for(icol in 1:ncol(prof.table)){
    prof.table[,icol] <- prof.table[,icol] - min(prof.table[subset,icol])
  }
  if(ymax=="default") ymax <- 1.1*max(prof.table[subset,])
  ylim <- c(0,ymax)

  # remove columns that have change less than minfraction change relative to total
  column.max <- apply(prof.table[subset,],2,max)
  change.fraction <- column.max / column.max[1]
  include <- change.fraction >= minfraction
  cat("Likelihood components with max. change as fraction of max. total change.\n",sep="")
  print(data.frame(fraction_of_total_change=round(change.fraction,4),include=include,label=component.labels))
  component.labels <- component.labels[include]

  # reorder values
  prof.table <- prof.table[order(parvec),include]
  parvec <- parvec[order(parvec)]

  # reorder columns by largest change (if requested)
  change.fraction <- change.fraction[include]
  if(sort.by.max.change){
    neworder <- c(1,1+order(change.fraction[-1],decreasing=TRUE))
    prof.table <- prof.table[,neworder]
    component.labels <- component.labels[neworder]
  }
    
  nlines <- ncol(prof.table)
  if(col[1]=="default") col <- rich.colors.short(nlines)
  if(pch[1]=="default") pch <- 1:nlines
  lwd <- c(lwd.total,rep(lwd,nlines-1))
  cex <- c(cex.total,rep(cex,nlines-1))
  lty <- c(lty.total,rep(lty,nlines-1))
  #return(prof.table)
  
  # make plot
  plotprofile <- function(){
    plot(0,type='n',xlim=xlim,ylim=ylim,xlab=profile.label, ylab=ylab,
         yaxs=yaxs,xaxs=xaxs)
    abline(h=0,col='grey')
    matplot(parvec, prof.table, type=type,
            pch=pch, col=col,
            cex=cex, lty=lty, lwd=lwd, add=T)
    legend('topleft',bty='n',legend=component.labels,
           lwd=lwd, pt.cex=cex, lty=lty, pch=pch, col=col)
    box()
  }

  if(plot) plotprofile()
  if(print){
    pngfun("profile_plot_likelihood.png")
    plotprofile()
    dev.off()
  }
  out <- data.frame(parvec=parvec,prof.table)
  names(out)[1] <- parlabel
  return(invisible(out))
}
