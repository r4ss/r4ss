SSplotSummaryF <- function(replist,yrs="all",Ftgt=NA,ylab="Summary Fishing Mortality",
                           plot=TRUE,print=FALSE,plotdir="default",verbose=TRUE,
                           uncertainty=TRUE,
                           pwidth=7,pheight=7,punits="in",res=300,ptsize=12) {
  #plots the summary F (or harvest rate) as set up in the starter file
  #needs a lot of work to be generalized

  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL
  if(plotdir=="default") plotdir <- replist$inputs$dir

  if(yrs[1]=="all") {yrs <- replist$startyr:replist$endyr}
  Ftot <- replist$derived_quants[match(paste("F_",yrs,sep=""),replist$derived_quants$LABEL),]
  if(all(is.na(Ftot$Value))){
    warning("Skipping SSplotSummaryF because no real values found in DERIVED_QUANTITIES\n",
            "    Values with labels like F_2012 may not be real.\n")
    return()
  }
  Fmax <- max(c(Ftot$Value,Ftgt+0.01),na.rm=TRUE)
  if(uncertainty){
    uppFtot <- Ftot$Value + 1.96*Ftot$StdDev
    lowFtot <- Ftot$Value - 1.96*Ftot$StdDev
    Fmax <- max(c(uppFtot,Ftgt+0.01),na.rm=TRUE)
  }
  plotfun <- function(){
    plot(0,type="n",,xlab="Year",ylab=ylab,xlim=range(yrs),ylim=c(0,Fmax),
         cex.lab=1.0,cex.axis=1.0,cex=0.7)
    abline(h=0,col='grey')
    if(uncertainty) segments(as.numeric(substring(Ftot$LABEL,3,6)),uppFtot,as.numeric(substring(Ftot$LABEL,3,6)),lowFtot,col=gray(0.5))
    points(as.numeric(substring(Ftot$LABEL,3,6)),Ftot$Value,pch=16,type="p")
    abline(h=Ftgt,col="red")
  }
  if(plot) plotfun()
  if(print){
    file <- file.path(plotdir,"ts_summaryF.png")
    caption <- "Summary F (definition of F depends on setting in starter.ss)"
    plotinfo <- pngfun(file=file, caption=caption)
    plotfun()
    dev.off()
    if(!is.null(plotinfo)) plotinfo$category <- "Timeseries"
  }
  if(verbose) cat("Plotting Summary F\n")
  return(invisible(plotinfo))
}
