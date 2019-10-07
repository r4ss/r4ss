#' Plot yield and surplus production.
#' 
#' Plot yield and surplus production from Stock Synthesis output. Surplus
#' production is based on Walters et al. (2008).
#' 
#' 
#' @param replist list created by \code{SS_output}
#' @param subplots vector controlling which subplots to create
#' Numbering of subplots is as follows:
#' \itemize{
#'   \item 1 yield curve
#'   \item 2 yield curve with reference points
#'   \item 3 surplus production vs. biomass plots (Walters et al. 2008) 
#' }
#' @param refpoints character vector of which reference points to display in
#' subplot 2, from the options 'MSY', 'Btgt', and 'SPR'.
#' @param add add to existing plot? (not yet implemented)
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param labels vector of labels for plots (titles and axis labels)
#' @param col line color for equilibrium plot
#' @param col2 line color for dynamic surplus production plot
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
SSplotYield <-
  function(replist,
           subplots=1:3,
           refpoints = c('MSY', 'Btgt', 'SPR', 'Current'),
           add=FALSE,plot=TRUE,print=FALSE,
           labels=c("Fraction unfished", #1
             "Equilibrium yield (mt)",    #2
             "Total biomass (mt)",        #3
             "Surplus production (mt)"),  #4
           col="blue", col2="black", lty=1, lwd=2, cex.main=1,
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,
           plotdir="default",
           verbose=TRUE)
{
  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
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
  #SSB0          <- replist$SBzero
  SSB0        <- replist$derived_quants["SSB_Virgin", "Value"]
  # function for yield curve
  yieldfunc <- function(refpoints = NULL){
    if(!add){
      # empty plot
      plot(0,type="n",xlim=c(0,max(equil_yield$Depletion,1,na.rm=TRUE)),
           ylim=c(0,max(equil_yield$Catch,na.rm=TRUE)),
           xlab=labels[1],ylab=labels[2])
      abline(h=0,col="grey")
      abline(v=0,col="grey")
    }
    # add lines for reference points (if requested)
    lines(equil_yield$Depletion, equil_yield$Catch,
          lwd = lwd, col = col, lty = lty)
    colvec <- c(4,2,3,1)
    if('MSY' %in% refpoints){
      lines(x = rep(replist$derived_quants["SSB_MSY", "Value"]/SSB0, 2),
            y = c(0, replist$derived_quants["Dead_Catch_MSY", "Value"]),
            col = colvec[1], lwd = 2, lty = 2)
    }
    if('Btgt' %in% refpoints){
      lines(x = rep(replist$derived_quants["SSB_Btgt", "Value"]/SSB0, 2),
            y = c(0, replist$derived_quants["Dead_Catch_Btgt", "Value"]),
            col = colvec[2], lwd = 2, lty = 2)
    }
    if('SPR' %in% refpoints){
      lines(x = rep(replist$derived_quants["SSB_SPR", "Value"]/SSB0, 2),
            y = c(0, replist$derived_quants["Dead_Catch_SPR", "Value"]),
            col = colvec[3], lwd = 2, lty = 2)
    }
    if('Current' %in% refpoints){
      which_val <- abs(equil_yield$Depletion - replist$current_depletion) ==
        min(abs(equil_yield$Depletion - replist$current_depletion))
      lines(x = rep(replist$current_depletion, 2),
            y = c(0, equil_yield$Catch[which_val]),
            col = colvec[4], lwd = 2, lty = 2)
    }
    # legend
    which_lines <- c('MSY' %in% refpoints,
                     'Btgt' %in% refpoints,
                     'SPR' %in% refpoints,
                     'Current' %in% refpoints)
    if(any(which_lines)){
      legend('topright', bty = 'n', lwd = 2, lty = 2,
             col = colvec[which_lines],
             legend = c('MSY', 'B target', 'SPR target', 'Current'))
    }
  }

  if(1 %in% subplots | 2 %in% subplots){
    # test if data is available
    if(!is.null(equil_yield[[1]][1]) && any(!is.na(equil_yield[[1]]))){
      # further test for bad values
      # (not sure the circumstances where this is needed)
      if(any(!is.na(equil_yield$Depletion)) &
         any(!is.na(equil_yield$Catch)) &
         any(!is.infinite(equil_yield$Depletion))){
        if(1 %in% subplots){
          # make plot
          if(plot){
            yieldfunc()
          }
          if(print){
            file <- "yield1_yield_curve.png"
            caption <- "Yield curve"
            plotinfo <- pngfun(file=file, caption=caption)
            yieldfunc()
            dev.off()
          }
        }
        if(2 %in% subplots & !is.null(refpoints)){
          # make plot
          if(plot){
            yieldfunc(refpoints = refpoints)
          }
          if(print){
            file <- "yield2_yield_curve_with_refpoints.png"
            caption <- "Yield curve with reference points"
            plotinfo <- pngfun(file=file, caption=caption)
            yieldfunc(refpoints = refpoints)
            dev.off()
          }
        }          
      }else{
        cat("Skipped equilibrium yield plots: equil_yield has all NA values\n")
      }
    }else{
      cat("Skipped equilibrium yield plots: no equil_yield results in this model\n")
    }
  }
  
  # timeseries excluding equilibrium conditions or forecasts
  ts <- timeseries[!timeseries$Era %in% c("VIRG","FORE"),]

  # get total dead catch
  stringB <- "dead(B)"
  catchmat <- as.matrix(ts[, substr(names(ts),1,nchar(stringB))==stringB])
  # aggregate catch across fleets
  catch <- rowSums(catchmat)

  # aggregate catch and biomass across seasons and areas
  catch_agg <- aggregate(x=catch, by=list(ts$Yr), FUN=sum)$x
  Bio_agg <- aggregate(x=ts$Bio_all, by=list(ts$Yr), FUN=sum)$x

  # number of years to consider
  Nyrs <- length(Bio_agg)

  # function to calculate and plot surplus production
  sprodfunc <- function(){
    sprod <- rep(NA, Nyrs)
    # calculate surplus production as difference in biomass adjusted for catch
    sprod[1:(Nyrs-1)] <- Bio_agg[2:Nyrs] - Bio_agg[1:(Nyrs-1)] + catch_agg[1:(Nyrs-1)]
    sprodgood <- !is.na(sprod)
    Bio_agg_good <- Bio_agg[sprodgood]
    sprod_good <- sprod[sprodgood]
    xlim <- c(0, max(Bio_agg_good, na.rm=TRUE))
    ylim <- c(min(0, sprod_good, na.rm=TRUE), max(sprod_good, na.rm=TRUE))
    # make empty plot
    if(!add){
      plot(0, ylim=ylim, xlim=xlim, xlab=labels[3], ylab=labels[4], type="n")
    }
    # add lines
    lines(Bio_agg_good, sprod_good, col=col2)
    # make arrows
    old_warn <- options()$warn      # previous setting
    options(warn=-1)                # turn off "zero-length arrow" warning
    s <- seq(length(sprod_good)-1)
    arrows(Bio_agg_good[s], sprod_good[s], Bio_agg_good[s+1], sprod_good[s+1],
           length=0.06, angle=20, col=col2, lwd=1.2)
    options(warn=old_warn)  #returning to old value

    # add lines at 0 and 0
    abline(h=0,col="grey")
    abline(v=0,col="grey")
    # add blue point at start
    points(Bio_agg_good[1], sprod_good[1], col=col2, bg="white", pch=21)
  } # end sprodfunc

  if(3 %in% subplots){
    if(plot){sprodfunc()}
    if(print){
      file <- "yield3_surplus_production.png"
      caption <-
        paste("Surplus production vs. biomass plot. For interpretation, see<br>",
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
