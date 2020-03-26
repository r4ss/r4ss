#' Create a plot for the TSC report
#' 
#' Creates a plot of catch and spawning biomass from the output of
#' \code{\link{SS_output}} for the NOAA TSC report.
#' 
#' It creates a plot on the current graphics device, in a pdf file, or as a png
#' image of the figure used in the TSC report produced by the NWFSC.  It
#' expects the SS results read in by \code{\link{SS_output}}.  If MCMC results
#' are to be plotted, a 'mcmc' list element should be added using the
#' \code{\link{SSgetMCMC}} function. See the examples below.
#' 
#' @param SSout The output from \code{\link{SS_output}}
#' @param yrs The vector of years to plot
#' @param ylimBar y-axis limits for catch barplot
#' @param ylimDepl y-axis limits for depletion line
#' @param colBar colors of the bars
#' @param cexBarLabels character expansion for the labels underneath the bars
#' (years)
#' @param cex.axis character expansion for the axis labels
#' @param space space between bars (see space argument of \code{barplot})
#' @param pchDepl character type for points on the depletion line
#' @param colDepl color of the points on the depletion line
#' @param lwdDepl width of the depletion line
#' @param shiftDepl shift from beginning of the year for the points on the
#' depletion line. Helps to guide the eye for exactly which year it corresponds
#' to.
#' @param pchSpace number of years between points on the depletion line. Higher
#' numbers help tidy up the plot when plotting many years.
#' @param ht Height of the plot in inches
#' @param wd Width of the plot in inches
#' @param labelLines line argument for \code{mtext} to move the axis labels
#' @param makePDF filename for a pdf file. If NULL it does not make a pdf.  Can
#' specify a pdf filename or a png filename. Not both at the same time.
#' @param makePNG filename for a png image. If NULL it does not make a png.
#' Can specify a pdf filename or a png filename. Not both at the same time.
#' @param MCMC If TRUE, will use mcmc results. It needs a list element called
#' 'mcmc' on SSout.
#' @return Returns a data frame with the years, spawning biomass, depletion,
#' and total dead catch.
#' @author Allan Hicks
#' @export
#' @seealso \code{\link{SS_output}} \code{\link{SSgetMCMC}}
#' @examples
#' 
#'   \dontrun{
#'
#'     # define directory
#'     directory <- "C:\NOAA2011\Dover\Models\base_20110701"
#'     # read model output
#'     base <- SS_output(dir=directory,covar = FALSE,verbose = FALSE)
#' 
#'     #show the plot in R
#'     TSCplot(base)
#'     TSCplot(base,yrs=2000:2011,pchSpace = 1)
#'
#'     #Create the plot as a PNG file
#'     TSCplot(base,makePNG="C:\NOAA2012\Assessments\TSCdover.png")
#'     #Create the plot as a PDF file
#'     TSCplot(base,makePDF="C:\NOAA2012\Assessment\TSCdover.pdf")
#' 
#'     # Model with MCMC results
#'     directory <- "C:/Models"
#'     base <- SS_output(dir = directory, dir.mcmc = 'mcmc')
#'     TSCplot(base, ylimDepl = c(0, 1.25), pchSpace = 1, MCMC = TRUE)
#' 
#'   }
#' 
TSCplot <- function(SSout,
                    yrs="default",
                    ylimBar="default",
                    ylimDepl=c(0,1.025),
                    colBar= "yellow",
                    cexBarLabels=1.1,
                    cex.axis=1.1,
                    space=0.0,
                    pchDepl=19,
                    colDepl="red",
                    lwdDepl=3,
                    shiftDepl = 0.25,
                    pchSpace = 5,
                    ht=4,wd=7,
                    labelLines=2.8,
                    makePDF=NULL,
                    makePNG=NULL,
                    MCMC = FALSE)
{
  
  ### Plots the barchart of catches and depletion trajctory for the TSC report

  if(!is.null(makePDF) & !is.null(makePNG)){
    stop("Cannot specify both makePDF and makePNG. Choose only one.\n")
  }

  indVirgin <- which(SSout$timeseries$Era=="VIRG")
  ind <- which(SSout$timeseries$Era=="TIME")
  ind <- c(ind,max(ind)+1)
  if(yrs[1]=="default") yrs <- unique(sort(SSout$timeseries$Yr[ind]))

  #get catches + discards summed over areas
  deadCatch <- SSplotCatch(SSout,plot = FALSE,verbose = FALSE)$totcatchmat     
  if(ncol(deadCatch) > 2) {  #sum over fisheries
    deadCatch <- cbind(apply(deadCatch[,-ncol(deadCatch)],1,sum),deadCatch[,ncol(deadCatch)])
  }
  deadCatch <- deadCatch[match(yrs,deadCatch[,2]),]
  rownames(deadCatch) <- yrs

  if(!MCMC) {
    SBzero <- SSout$SBzero
    SB <- SSout$derived_quants[substring(SSout$derived_quants$Label,1,4)=="SSB_",]
    SB <- SB[match(as.character(yrs),substring(SB$Label,5)),]
    depl <- SSout$derived_quants[substring(SSout$derived_quants$Label,1,7)=="Bratio_",]
    depl <- depl[match(as.character(yrs),substring(depl$Label,8)),]
    SP <- data.frame(Yr=yrs, SpawnBio=SB[,"Value"], Depl=depl[,"Value"],Dead_Catch=deadCatch[,1])
  }
  if(MCMC) {
    if(is.null(SSout$mcmc)){
      stop("There is no mcmc element on the model list.\n",
           "Set MCMC = FALSE or add in the mcmc element to the list.\n")
    }
    SBzero <- median(SSout$mcmc$SSB_Virgin)
    SB <- SSout$mcmc[,substring(names(SSout$mcmc),1,4)=="SSB_"]
    SB <- apply(SB[,match(as.character(yrs),substring(names(SB),5))],2,median)
    depl <- SSout$mcmc[,substring(names(SSout$mcmc),1,7)=="Bratio_"]
    tmp1 <- match(as.character(yrs),substring(names(depl),8))  #can have an NA in it and will cause an error
    tmp2 <- tmp1[!is.na(tmp1)]   #remove NA's to get the medians
    depl <- apply(depl[,tmp2],2,median)
    depl <- depl[match(as.character(yrs),substring(names(depl),8))]
    SP <- data.frame(Yr=yrs, SpawnBio=SB, Depl=depl, Dead_Catch=deadCatch[,1])
  }
  
  if(ylimBar=="default") {
    ylimBar <- c(0,max(SP$Dead_Catch,na.rm = TRUE)*1.05)
  }
  ind <- seq(1,nrow(SP),pchSpace)
  
  if(is.null(makePDF) & is.null(makePNG)) { dev.new(height=ht,width=wd) }
  if(!is.null(makePDF)) { pdf(file=makePDF,width=wd,height=ht) }
  if(!is.null(makePNG)) { png(filename=makePNG,width=wd,height=ht,units = "in", pointsize = 10, res=300) }
  par(mar=c(4,5,2,5))
  barOut <- barplot(SP$Dead_Catch,  names.arg = SP$Yr, ylim=ylimBar, ylab="",
                    col='yellow', cex=cexBarLabels, cex.axis=cex.axis,
                    space=space,xlim=c(0,nrow(SP)),axisnames = FALSE)
  axis(1,at=barOut[ind,1],labels=yrs[ind])
  par(new = TRUE)
  xpts <- (0:(nrow(SP)-1))+shiftDepl
  plot(xpts, SP$Depl, yaxt='n', yaxs='i', xaxt = 'n', ylab="", xlab="",
       ylim=ylimDepl, type='l', lwd=lwdDepl,  cex.axis=cex.axis, xlim=c(0,nrow(SP)))
  points(xpts[ind], SP$Depl[ind], pch=pchDepl, col=colDepl)
  axis(4, at=seq(ylimDepl[1], ylimDepl[2], 0.1), cex.axis=cex.axis)
  mtext(c("Year","Total mortality catch (mt)", "Depletion"), side=c(1,2,4), line=labelLines, cex=1.5)

  if(!is.null(makePDF)) {
    dev.off()
    cat("The plot is in pdf file",makePDF,"\n")
  }
  if(!is.null(makePNG)) {
    dev.off()
    cat("The plot is in png file",makePNG,"\n")
  }

  invisible(SP)
}
