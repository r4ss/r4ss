PinerPlot <-
  function(summaryoutput,
           plot=TRUE,print=FALSE,
           component="Length_like",
           main="Changes in length-composition likelihoods by fleet",
           models="all",
           fleets="all",
           profile.string="R0",
           profile.label=expression(log(italic(R)[0])),
           ylab="Change in -log-likelihood",
           col="default",
           pch="default",
           lty=1, lty.total=1,
           lwd=2, lwd.total=3,
           cex=1, cex.total=1.5,
           xlim="default",
           ymax="default",
           xaxs="r", yaxs="r",
           type="o",
           legend=TRUE, legendloc="topright",
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           plotdir=NULL,
           verbose=TRUE)
{
  # this function is very similar to SSplotProfile, but shows fleet-specific likelihoods
  # for a single components rather than multiple components aggregated across fleets
  
  # subfunction to write png files
  pngfun <- function(file){
    png(filename=paste(plotdir,file,sep="/"),width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
  }
  
  if(print & is.null(plotdir)) stop("to print PNG files, you must supply a directory as 'plotdir'")

  # get stuff from summary output
  n    <- summaryoutput$n
  lbf  <- summaryoutput$likelihoods_by_fleet
  nfleets <- ncol(lbf)-3
  pars <- summaryoutput$pars

  if(!component %in% lbf$Label) stop("input 'component' needs to be one of the following\n",
                                     paste("    ",unique(lbf$Label),"\n"))

  # check number of models to be plotted
  if(models[1]=="all"){
    models <- 1:n
  }else{
    if(!all(models %in% 1:n))
      stop("Input 'models' should be a vector of values from 1 to n=",n," (for your inputs).\n")
  }
  # check number of fleets to be plotted
  if(fleets[1]=="all"){
    fleets <- 1:n
  }else{
    if(!all(fleets %in% 1:n))
      stop("Input 'fleets' should be a vector of values from 1 to nfleets=",nfleets," (for your inputs).\n")
  }
  
  # find the parameter that the profile was over
  parnumber <- grep(profile.string,pars$Label)
  if(length(parnumber)<=0) stop("No parameters matching profile.string='",profile.string,"'",sep="")
  parlabel <- pars$Label[parnumber]
  if(length(parlabel) > 1)
    stop("Multiple parameters matching profile.string='",profile.string,"': ",paste(parlabel,collapse=", "),sep="")

  parvec <- as.numeric(pars[pars$Label==parlabel,models])
  cat("Parameter matching profile.string='",profile.string,"': '",parlabel,"'\n",sep="")
  cat("Parameter values (after subsetting based on input 'models'):\n")
  print(parvec)
  if(xlim[1]=="default") xlim <- range(parvec)

  # rearange likelihoods to be in columns by type
  prof.table <- lbf[lbf$model %in% models & lbf$Label==component, ]

  # subtract minimum value from each likelihood component (over requested parameter range)
  subset <- parvec >= xlim[1] & parvec <= xlim[2]

  for(icol in 3:ncol(prof.table)){
    prof.table[,icol] <- prof.table[,icol] - min(prof.table[subset,icol])
  }
  if(ymax=="default") ymax <- 1.1*max(prof.table[subset,-(1:2)])
  ylim <- c(0,ymax)
  
  # reorder values
  prof.table <- prof.table[order(parvec),]
  parvec <- parvec[order(parvec)]
    
  nlines <- ncol(prof.table)-2
  if(col[1]=="default") col <- rich.colors.short(nlines)
  if(pch[1]=="default") pch <- 1:nlines
  lwd <- c(lwd.total,rep(lwd,nlines-1))
  cex <- c(cex.total,rep(cex,nlines-1))
  lty <- c(lty.total,rep(lty,nlines-1))
  #return(prof.table)
  
  # make plot
  plotprofile <- function(){
    plot(0,type='n',xlim=xlim,ylim=ylim,xlab=profile.label, ylab=ylab,
         yaxs=yaxs,xaxs=xaxs,main=main)
    abline(h=0,col='grey')
    matplot(parvec, prof.table[,-(1:2)], type=type,
            pch=pch, col=col,
            cex=cex, lty=lty, lwd=lwd, add=T)
    if(legend)
      legend(legendloc,bty='n',legend=names(prof.table)[-(1:2)],
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
