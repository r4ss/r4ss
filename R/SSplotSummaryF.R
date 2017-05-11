#' Plot the summary F (or harvest rate).
#' 
#' Plots the summary F (or harvest rate) as set up in the starter file Needs a
#' lot of work to be generalized
#' 
#' @param replist List created by \code{SS_output}
#' @param yrs Which years to include.
#' @param Ftgt Target F where horizontal line is shown.
#' @param ylab Y-axis label.
#' @param plot Plot to active plot device?
#' @param print Print to PNG files?
#' @param plotdir Directory where PNG files will be written. By default it will
#' be the directory where the model was run.
#' @param verbose Verbose output to R console?
#' @param uncertainty Show 95\% uncertainty intervals around point estimates?
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize point size for PNG file
#' @author Allan Hicks
#' @export
#' @seealso \code{\link{SSplotTimeseries}}, ~~~
SSplotSummaryF <- function(replist,yrs="all",Ftgt=NA,ylab="Summary Fishing Mortality",
                           plot=TRUE,print=FALSE,plotdir="default",verbose=TRUE,
                           uncertainty=TRUE,
                           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10) {
  #plots the summary F (or harvest rate) as set up in the starter file
  #needs a lot of work to be generalized

  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL
  if(plotdir=="default"){
    plotdir <- replist$inputs$dir
  }

  if(yrs[1]=="all") {yrs <- replist$startyr:replist$endyr}
  Ftot <- replist$derived_quants[match(paste("F_",yrs,sep=""),replist$derived_quants$Label),]
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
    if(uncertainty) segments(as.numeric(substring(Ftot$Label,3,6)),uppFtot,as.numeric(substring(Ftot$Label,3,6)),lowFtot,col=gray(0.5))
    points(as.numeric(substring(Ftot$Label,3,6)),Ftot$Value,pch=16,type="p")
    abline(h=Ftgt,col="red")
  }
  if(plot) plotfun()
  if(print){
    caption <- "Summary F (definition of F depends on setting in starter.ss)"
    plotinfo <- pngfun(file="ts_summaryF.png", caption=caption)
    plotfun()
    dev.off()
    if(!is.null(plotinfo)) plotinfo$category <- "Timeseries"
  }
  if(verbose) cat("Plotting Summary F\n")
  return(invisible(plotinfo))
}
