#' Plot SPR quantities.
#' 
#' Plot SPR quantities, including 1-SPR and phase plot.
#' 
#' 
#' @param replist list created by \code{SSoutput}
#' @param add add to existing plot (not yet implemented)
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param uncertainty include plots showing uncertainty?
#' @param subplots vector controlling which subplots to create
#' @param forecastplot Include forecast years in plot?
#' @param col1 first color used
#' @param col2 second color used
#' @param col3 third color used
#' @param col4 fourth color used
#' @param sprtarg F/SPR proxy target. "default" chooses based on model output.
#' @param btarg target depletion to be used in plots showing depletion. May be
#' omitted by setting to NA. "default" chooses based on model output.
#' @param labels vector of labels for plots (titles and axis labels)
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize point size for PNG file
#' @param cex.main character expansion for plot titles
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param verbose report progress to R GUI?
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
SSplotSPR <-
  function(replist,add=FALSE,plot=TRUE,print=FALSE,
           uncertainty=TRUE,
           subplots=1:4,forecastplot=FALSE,
           col1="black",col2="blue",col3="green3",col4="red",
           sprtarg="default", btarg="default",
           labels=c("Year", #1
             "SPR",         #2
             "1-SPR"),      #3
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
           plotdir="default",
           verbose=TRUE)
{
  # plot SPR-related quantities
  
  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  if(plotdir=="default") plotdir <- replist$inputs$dir

  sprseries             <- replist$sprseries
  timeseries            <- replist$timeseries
  derived_quants        <- replist$derived_quants
  nsexes                <- replist$nsexes
  nseasons              <- replist$nseasons
  nareas                <- replist$nareas
  endyr                 <- replist$endyr
  managementratiolabels	<- replist$managementratiolabels

  if(sprtarg=="default") sprtarg <- replist$sprtarg
  if(btarg=="default") btarg <- replist$btarg

  # choose which points to plot
  good <- sprseries$Yr <= endyr
  if(forecastplot) good <- rep(TRUE,nrow(sprseries))
  
  sprfunc <- function(){
    if(!add) plot(0,xlab=labels[1],ylab=labels[2],xlim=range(sprseries$Yr[good]),
                  ylim=c(0,max(1,max(sprseries$spr[!is.na(sprseries$spr)]))),type="n")
    lines(sprseries$Yr[good],sprseries$spr[good],type="o",col=col2)
    if(sprtarg>0) abline(h=sprtarg,col=col4,lty=2)
    abline(h=0,col="grey")
    abline(h=1,col="grey")
  }
  
  if(1 %in% subplots){
    if(plot) sprfunc()
    if(print){
      file <- "SPR1_series.png"
      caption <- "Timeseries of SPR"
      plotinfo <- pngfun(file=file, caption=caption)
      sprfunc()
      dev.off()
    }
  }

  # temporary disable multi-season models until code cleanup
  if(nseasons>1) cat("Skipped additional SPR plots because they're not yet configured for multi-season models\n")
  if(nseasons==1){ 
    sprfunc2 <- function(){
      if(!add) plot(0,xlim=range(sprseries$Yr[good]),
                    xlab=labels[1],ylab=labels[3],ylim=c(0,1),type="n")
      lines(sprseries$Yr[good],(1-sprseries$spr[good]),type="o",col=col2)
      if(sprtarg>0) abline(h=(1-sprtarg),col=col4,lty=2)
      abline(h=0,col="grey")
      abline(h=1,col="grey")}

    if(2 %in% subplots){
      if(plot) sprfunc2()
      if(print){
        file <- "SPR2_minusSPRseries.png"
        caption <- "Timeseries of 1-SPR"
        plotinfo <- pngfun(file=file, caption=caption)
        sprfunc2()
        dev.off()
      }
    }

    if(!uncertainty | sprtarg<=0){
      cat("skipped SPR ratio timeseries: requires both sprtarg>0 and uncertainty=TRUE.\n")
    }else{
      sprratiostd <- derived_quants[substring(derived_quants$LABEL,1,8)=="SPRratio",]
      sprratiostd$Yr <- as.numeric(substring(sprratiostd$LABEL,10))
      sprratiostd$period <- "fore"
      sprratiostd$period[sprratiostd$Yr<=(endyr)] <- "time"
      sprratiostd$upper <- sprratiostd$Value + 1.96*sprratiostd$StdDev
      sprratiostd$lower <- pmax(sprratiostd$Value - 1.96*sprratiostd$StdDev,0) # max of value or 0
      ylab <- managementratiolabels[1,2]
      ylim=c(0,max(1,sprratiostd$upper[sprratiostd$period=="time"]))
      sprfunc3 <- function(){
        if(!add) plot(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$Value[sprratiostd$period=="time"],
                      xlab=labels[1],ylim=ylim,ylab=ylab,type="n")
        lines(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$Value[sprratiostd$period=="time"],
              type="o",col=col2)
        abline(h=0,col="grey")
        abline(h=1,col=col4)
        text((min(sprratiostd$Yr)+4),(1+0.02),"Management target",adj=0)
        lines(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$upper[sprratiostd$period=="time"],col=col2,lty="dashed")
        lines(sprratiostd$Yr[sprratiostd$period=="time"],sprratiostd$lower[sprratiostd$period=="time"],col=col2,lty="dashed")
      }
      if(3 %in% subplots){
        if(plot) sprfunc3()
        if(print){
          file <- "SPR3_ratiointerval.png"
          caption <- "Timeseries of SPR ratio"
          plotinfo <- pngfun(file=file, caption=caption)
          sprfunc3()
          dev.off()
        }
      }
    }

    if(4 %in% subplots){
      if(btarg<=0 | sprtarg<=0){
        cat("skipped SPR phase plot because btarg or sprtarg <= 0\n")
      }else{
        timeseries$Yr <- timeseries$Yr + (timeseries$Seas-1)/nseasons
        #!subsetting to season 1 only, initially just getting area 1
        ts <- timeseries[timeseries$Seas==1 &
                           timeseries$Area==1 &
                             timeseries$Yr <= endyr,]
        # if there is more than 1 area, add them in now
        # this could be done using "aggregate" but this approach is more foolproof (hopefully)
        if(nareas>1){
          for(iarea in 2:nareas){
            ts_area_i <- timeseries[timeseries$Seas==1 &
                                      timeseries$Area==iarea &
                                        timeseries$Yr <= endyr,]
            ts$SpawnBio <- ts$SpawnBio + ts_area_i$SpawnBio
          }
        }
        # divide spawning biomass by 2 for single-sex models
        if(nsexes==1){
          ts$SpawnBio <- ts$SpawnBio/2
        }
        # calculate depletion
        depletionseries <- ts$SpawnBio/ts$SpawnBio[1]
        reldep <- depletionseries[ts$Yr %in% sprseries$Yr]/btarg
        relspr <- (1-sprseries$spr[sprseries$Yr <= endyr])/(1-sprtarg)
        # set axis limits
        xmax <- 1.1*max(reldep)
        ymax <- 1.1*max(1,relspr[!is.na(relspr)])
        ylab <- managementratiolabels[1,2]
        # function to make the plot
        phasefunc <- function(){
          if(!add) plot(reldep,relspr,xlab="B/Btarget",
                        xlim=c(0,xmax),ylim=c(0,ymax),ylab=ylab,type="n")
          lines(reldep,relspr,type="o",col=col2)
          abline(h=0,col="grey")
          abline(v=0,col="grey")
          lines(reldep,relspr,type="o",col=col2)
          points(reldep[length(reldep)],relspr[length(relspr)],col=col4,pch=19)
          abline(h=1,col=col4,lty=2)
          abline(v=1,col=col4,lty=2)
        }

        if(plot) phasefunc()
        if(print){
          file <- "SPR4_phase.png"
          caption <- "Phase plot of biomass ratio vs. SPR ratio"
          plotinfo <- pngfun(file=file, caption=caption)
          phasefunc()
          dev.off()
        }
      }
    } # end test for making phase plot
  } # end check for number of seasons=1
  if(!is.null(plotinfo)) plotinfo$category <- "SPR"
  return(invisible(plotinfo))
}
