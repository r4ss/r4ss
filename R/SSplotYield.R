#' Plot yield and surplus production.
#' 
#' Plot yield and surplus production from Stock Synthesis output. Surplus
#' production is based on Walters et al. (2008).
#' 
#' 
#' @param replist list created by \code{SS_output}
#' @param subplots vector controlling which subplots to create
#' @param add add to existing plot? (not yet implemented)
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param labels vector of labels for plots (titles and axis labels)
#' @param col line color (only applied to equilbrium yield plot at this time)
#' @param lty line type (only applied to equilbrium yield plot at this time)
#' @param lwd line width (only applied to equilbrium yield plot at this time)
#' @param cex.main character expansion for plot titles
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize point size for PNG file
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param verbose report progress to R GUI?
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
#' @references Walters, Hilborn, and Christensen, 2008, Surplus production
#' dynamics in declining and recovering fish populations.  Can. J. Fish. Aquat.
#' Sci. 65: 2536-2551
#' @keywords hplot
SSplotYield <-
  function(replist,
           subplots=1:2,
           add=FALSE,plot=TRUE,print=FALSE,
           labels=c("Relative depletion", #1
             "Equilibrium yield (mt)",    #2
             "Total biomass (mt)",        #3
             "Surplus production (mt)"),  #4
           col="blue", lty=1, lwd=2, cex.main=1,
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,
           plotdir="default",
           verbose=TRUE)
{
  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  equil_yield <- replist$equil_yield
  # column named changed from Catch to Tot_Catch in SSv3.30
  if("Tot_Catch" %in% names(equil_yield)){
    equil_yield$Catch <- equil_yield$Tot_Catch
  }
  nareas      <- replist$nareas
  nseasons    <- replist$nseasons
  timeseries  <- replist$timeseries
  SS_versionshort <- replist$SS_versionshort
  if(is.null(SS_versionshort)) SS_versionshort <- "older than SS-V3.20"

  # test if data is available
  if(!is.null(equil_yield[[1]][1]) && any(!is.na(equil_yield[[1]]))){
    # function for yeild curve
    yieldfunc <- function(){
      if(!add){
        # empty plot
        plot(0,type="n",xlim=c(0,max(equil_yield$Depletion,1,na.rm=TRUE)),
             ylim=c(0,max(equil_yield$Catch,na.rm=TRUE)),
             xlab=labels[1],ylab=labels[2])
        abline(h=0,col="grey")
        abline(v=0,col="grey")
      }
      # add lines
      lines(equil_yield$Depletion,equil_yield$Catch,
            lwd=lwd,col=col,lty=lty)
    }
    # make plot
    if(1 %in% subplots){
      if(plot){yieldfunc()}
      if(print){
        file <- paste(plotdir,"yield1_yield_curve.png",sep="")
        caption <- "Yield curve"
        plotinfo <- pngfun(file=file, caption=caption)
        yieldfunc()
        dev.off()}
    }
  }else{
    cat("Skipped equilibrium yield plot: no equil_yield results in this model\n")
  }

  ts <- timeseries
  ts$Yr <- ts$Yr + (ts$Seas-1)/nseasons

  # get total biomass and total catch (across areas)
  arearows <- ts$Area==1
  Bio_all <- ts$Bio_all[arearows]
  if(SS_versionshort=="SS-V3.11") stringB <- "enc(B)" else stringB <- "sel(B)"

  totcatchmat <- as.matrix(ts[arearows, substr(names(ts),1,nchar(stringB))==stringB])

  if(nareas > 1){
    for(iarea in 2:nareas){
      arearows <- ts$Area==iarea
      Bio_all <- ts$Bio_all[arearows]
      totcatchmat <- totcatchmat + as.matrix(ts[arearows, substr(names(ts),1,nchar(stringB))==stringB])
    }
  }

  ls <- nrow(totcatchmat)
  sprodfunc <- function(){
    totcatch <- 0
    totcatch[3:ls] <- rowSums(totcatchmat)[3:ls]
    sprod <- NA
    sprod[3:(ls-1)] <- Bio_all[4:ls] - Bio_all[3:(ls-1)] + totcatch[3:(ls-1)]
    sprodgood <- !is.na(sprod)
    Bio_all_good <- Bio_all[sprodgood]
    sprod_good <- sprod[sprodgood]
    xlim <- c(0,max(Bio_all_good,na.rm=TRUE))
    ylim <- c(min(0,sprod_good,na.rm=TRUE),max(sprod_good,na.rm=TRUE))
    plot(Bio_all_good,sprod_good,ylim=ylim,xlim=xlim,xlab=labels[3],ylab=labels[4],type="l",col="black")

    # make arrows
    old_warn <- options()$warn      # previous setting
    options(warn=-1)                # turn off "zero-length arrow" warning
    s <- seq(length(sprod_good)-1)
    arrows(Bio_all_good[s],sprod_good[s],Bio_all_good[s+1],sprod_good[s+1],length=0.06,angle=20,col="black",lwd=1.2)
    options(warn=old_warn)  #returning to old value

    abline(h=0,col="grey")
    abline(v=0,col="grey")
    points(Bio_all_good[1],sprod_good[1],col="blue",pch=19)
  } # end sprodfunc

  if(2 %in% subplots){
    if(plot){sprodfunc()}
    if(print){
      file <- paste(plotdir,"yield2_Hilborn_surplus_production.png",sep="")
      caption <-
        paste("Surplus production plot. For interpretation, see<br>",
              "<blockquote>Walters, Hilborn, and  Christensen, 2008,",
              "Surplus production dynamics in declining and",
              "recovering fish populations. <i>Can. J. Fish. Aquat. Sci.</i>",
              "65: 2536-2551.</blockquote>")
      plotinfo <- pngfun(file=file, caption=caption)
      sprodfunc()
      dev.off()
    }
  }
  if(!is.null(plotinfo)) plotinfo$category <- "Yield"
  return(invisible(plotinfo))
} # end function
