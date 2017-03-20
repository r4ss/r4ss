#' Plot catch related quantities.
#' 
#' Plot catch related quantities from Stock Synthesis output. Plots include
#' harvest rate, continuous F, landings, and discard fraction.
#' 
#' 
#' @param replist List created by \code{SS_output}
#' @param subplots Vector controlling which subplots to create
#' @param add Add to existing plot? (not yet implemented)
#' @param areas Optional subset of areas to plot for spatial models
#' @param plot Plot to active plot device?
#' @param print Print to PNG files?
#' @param type Type parameter passed to plot function. Default "l" is lines
#' only.  Other options include "o" for overplotting points on lines.
#' @param fleetlty Vector of line type by fleet
#' @param fleetpch Vector of plot character by fleet
#' @param fleetcols Vector of colors by fleet
#' @param fleetnames Optional replacement for fleenames used in data file,
#' should only include fleets with catch that appear in catch plots.
#' @param lwd Line width
#' @param areacols Vector of colors by area. Default uses rich.colors by Arni
#' Magnusson
#' @param areanames Names for areas. Default is to use Area1, Area2,...
#' @param minyr Optional input for minimum year to show in plots
#' @param maxyr Optional input for maximum year to show in plots
#' @param annualcatch Include plot of catch aggregated across seasons within
#' each year
#' @param forecastplot Add points from forecast years
#' @param plotdir Directory where PNG or PDF files will be written. By default
#' it will be the directory where the model was run.
#' @param showlegend Put legend on plot
#' @param legendloc Location of legend (see ?legend for more info)
#' @param order Optional input to change the order of fleets in stacked plots.
#' @param xlab x-label for all plots
#' @param labels Vector of labels for plots (titles and axis labels)
#' @param catchasnumbers Is catch in numbers instead of biomass? Should be set
#' automatically if set to NULL. If fleets include a mix of biomass and
#' numbers, then catch plots should be interpreted carefully.
#' @param catchbars Show catch by fleet as barplot instead of stacked polygons?
#' (default=TRUE)
#' @param addmax Add a point on the y-axis for the maximum catch (default=TRUE)
#' @param ymax Optional input for ymax value (can be used to add or subtract
#' white space at the top of the figure)
#' @param pwidth Width of plot
#' @param pheight Height of plot
#' @param punits Units for PNG file
#' @param res Resolution for PNG file
#' @param ptsize point size for PNG file
#' @param cex.main Character expansion for plot titles
#' @param verbose Report progress to R console?
#' @author Ian Taylor, Ian Stewart
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
SSplotCatch <-
  function(replist,subplots=1:15,add=FALSE,areas=1,
           plot=TRUE,print=FALSE,
           type="l",
           fleetlty=1, fleetpch=1,
           fleetcols="default", fleetnames="default",
           lwd=3, areacols="default", areanames="default",
           minyr=NULL,maxyr=NULL,
           annualcatch=TRUE,
           forecastplot=FALSE,
           plotdir="default",showlegend=TRUE,
           legendloc="topleft",
           order="default",
           xlab="Year",
           labels=c("Harvest rate/Year",  #1
             "Continuous F",              #2
             "Landings",                  #3
             "Total catch",               #4
             "Predicted Discards",        #5  # should add units
             "Discard fraction",          #6  # need to add by weight or by length
             "(mt)",                      #7
             "(numbers x1000)",           #8
             "Observed and expected",     #9
             "aggregated across seasons"),
           catchasnumbers=NULL,
           catchbars=TRUE,
           addmax=TRUE,
           ymax=NULL,
           pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,
           cex.main=1, # note: no plot titles yet implemented
           verbose=TRUE)
{
  # plot catch-related time-series for Stock Synthesis
  # note from Ian Taylor to himself: "make minyr and maxyr connect to something!"

  # note: stacked plots depend on multiple fleets
  subplot_names <- c("1: landings",
                     "2: landings stacked",
                     "3: observed and expected landings (if different)",
                     # note: subplots 4-8 depend on discards
                     "4: total catch (including discards)",
                     "5: total catch (including discards) stacked",
                     "6: discards",
                     "7: discards stacked plot (depends on multiple fleets)" ,
                     "8: discard fraction",
                     "9: harvest rate",
                     # note: subplots 10-15 are only for seasonal models
                     "10: landings aggregated across seasons",
                     "11: landings aggregated across seasons stacked",
                     "12: total catch (if discards present) aggregated across seasons",
                     "13: total catch (if discards present) aggregated across seasons stacked",
                     "14: discards aggregated across seasons",
                     "15: discards aggregated across seasons stacked")

  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  F_method         <- replist$F_method
  timeseries       <- replist$timeseries
  nseasons         <- replist$nseasons
  nareas           <- replist$nareas
  nfleets          <- replist$nfleets
  nfishfleets      <- replist$nfishfleets
  catch_units      <- replist$catch_units
  endyr            <- replist$endyr
  FleetNames       <- replist$FleetNames
  IsFishFleet      <- replist$IsFishFleet
  SS_versionshort  <- toupper(substr(replist$SS_version,1,8))

  if(order[1]=="default") order <- replist$nfishfleets:1
  if(is.null(catchasnumbers)){
    if(min(catch_units,na.rm=TRUE)==2){
      catchasnumbers <- TRUE
      cat("  Note: catch_units ")
    }else{
      catchasnumbers <- FALSE
      if(2 %in% catch_units){
        cat("  Note: catch is in numbers for some, but not all fleets,\n",
            "       so be careful interpreting catch plots.\n")
      }
    }
  }

  if(nfishfleets==1 & verbose){
    cat("  Note: skipping stacked plots of catch for single-fleet model\n")
  }

  if(fleetnames[1]=="default"){
    fleetnames <- FleetNames[IsFishFleet]
  }
  if(plotdir=="default"){
    plotdir <- replist$inputs$dir
  }

  if(length(fleetlty)<nfishfleets) fleetlty <- rep(fleetlty,nfishfleets)
  if(length(fleetpch)<nfishfleets) fleetpch <- rep(fleetpch,nfishfleets)

  if(fleetcols[1]=="default"){
    fleetcols <- rich.colors.short(nfishfleets)
    if(nfishfleets > 2) fleetcols <- rich.colors.short(nfishfleets+1)[-1]
  }

  if(areacols[1]=="default"){
    areacols  <- rich.colors.short(nareas)
    if(nareas > 2) areacols <- rich.colors.short(nareas+1)[-1]
  }

  if(catchasnumbers){
    labels[3] <- paste(labels[3],labels[8])
    labels[4] <- paste(labels[4],labels[8])
  }else{
    labels[3] <- paste(labels[3],labels[7])
    labels[4] <- paste(labels[4],labels[7])
  }


  # time series (but no forecast) quantities used for multiple plots
  if(nseasons>1) timeseries$Yr <- timeseries$Yr + replist$seasfracs
  ts <- timeseries[timeseries$Yr <= endyr+1,]
  #ts.fore <- timeseries[timeseries$Yr >= endyr+1,]
  if(forecastplot){
    ts <- timeseries
  }
      
  # harvest rates
  if(F_method==1){
    stringF <- "Hrate:_"
    ylabF <- labels[1]
  }else{ # for either continuous F or hybrid F (methods 2 and 3)
    stringF <- "F:_"
    ylabF <- labels[2]
  }

  ### total landings (retained) & catch (encountered)
  goodrows <- ts$Area==1 & ts$Era %in% c("INIT","TIME")
  if(forecastplot){
    goodrows <- ts$Area==1 & ts$Era %in% c("INIT","TIME","FORE")
  }
  catchyrs <- ts$Yr[goodrows] # T/F indicator of the lines for which we want to plot catch

  if(SS_versionshort=="SS-V3.11"){
    stringN <- "enc(N)"
    stringB <- "enc(B)"
  }else{
    stringN <- "sel(N)"
    stringB <- "sel(B)"
  }
  if(catchasnumbers){
    retmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar("retain(N)"))=="retain(N)"])
    totcatchmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar(stringN))==stringN])
  }else{
    retmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
    totcatchmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar(stringB))==stringB])
  }
  totobscatchmat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar("obs_cat"))=="obs_cat"])
  Hratemat <- as.matrix(ts[goodrows, substr(names(ts),1,nchar(stringF))==stringF])

  # add total across areas
  if(nareas > 1){
    for(iarea in 2:nareas){
      arearows <- ts$Area==iarea & ts$Era %in% c("INIT","TIME")
      if(catchasnumbers){
        retmat <- retmat + as.matrix(ts[arearows, substr(names(ts),1,nchar("retain(N)"))=="retain(N)"])
        totcatchmat <- totcatchmat + as.matrix(ts[arearows, substr(names(ts),1,nchar(stringN))==stringN])
      }else{
        retmat <- retmat + as.matrix(ts[arearows, substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
        totcatchmat <- totcatchmat + as.matrix(ts[arearows, substr(names(ts),1,nchar(stringB))==stringB])
      }
      totobscatchmat <- totobscatchmat + as.matrix(ts[arearows, substr(names(ts),1,nchar("obs_cat"))=="obs_cat"])
      Hratemat  <- Hratemat  + as.matrix(ts[arearows, substr(names(ts),1,nchar(stringF))==stringF])
    }
  }

  # ghost is a fleet with no catch (or a survey for these purposes)
  ghost <- rep(TRUE,nfleets)
  ghost[(1:nfishfleets)[colSums(totcatchmat)>0]] <- FALSE
  if(all(ghost)) showlegend <- FALSE
  discmat <- totcatchmat - retmat

  discfracmat <- discmat/totcatchmat
  discfracmat[totcatchmat==0] <- NA

  # add total across seasons "mat2" indicates aggregation across seasons
  if(nseasons > 1){
    catchyrs2 <- floor(ts$Yr[goodrows & ts$Seas==1]) # T/F indicator of the lines for which we want to plot catch
    subset <- ts$Seas[goodrows]==1
    retmat2         <- retmat[subset,]
    totcatchmat2    <- totcatchmat[subset,]
    #totcatchmat2Yr  <- ts$Yr[subset]
    totobscatchmat2 <- totobscatchmat[subset,]
    discmat2        <- discmat[subset,]
    for(iseason in 2:nseasons){
      subset <- ts$Seas[goodrows]==iseason
      retmat2         <- retmat2         + retmat[subset,]
      totcatchmat2    <- totcatchmat2    + totcatchmat[subset,]
      totobscatchmat2 <- totobscatchmat2 + totobscatchmat[subset,]
      discmat2        <- discmat2        + discmat[subset,]
    }
  }
  # generic function to plot catch, landings, discards or harvest rates
  linefunc <- function(ymat,ylab,ymax=NULL,addtotal=TRUE,x=catchyrs){
    ymat <- as.matrix(ymat)
    if(addtotal & nfishfleets>1){
      ytotal <- rowSums(ymat)
      if(is.null(ymax)) ymax <- max(ytotal,na.rm=TRUE)
    }else{
      ytotal <- rep(NA,nrow(ymat))
      if(is.null(ymax)) ymax <- max(ymat,na.rm=TRUE)
    }
    plot(x, ytotal, ylim=c(0,ymax), xlab=xlab, ylab=ylab, type=type, lwd=lwd, col="black")
    abline(h=0,col="grey")
    #abline(h=1,col="grey")
    for(f in 1:nfishfleets){
      if(max(ymat[,f],na.rm=TRUE)>0){
        lines(x, ymat[,f], type=type, col=fleetcols[f],
              lty=fleetlty[f], lwd=lwd, pch=fleetpch[f])
      }
    }
    if(showlegend & nfishfleets!=1){
      if(type=="l") pchvec <- NA else pchvec <- c(1,fleetpch[!ghost])
      if(sum(!ghost)>1 & addtotal){
        legend(legendloc, lty=fleetlty[!ghost], lwd=lwd, pch=pchvec,
               col=c("black",fleetcols[!ghost]), legend=c("Total",fleetnames[!ghost]), bty="n")
      }else{
        legend(legendloc, lty=fleetlty[!ghost], lwd=lwd, pch=pchvec,
               col=fleetcols[!ghost], legend=fleetnames[!ghost], bty="n")
      }
    }
    return(TRUE)
  } # end linefunc

  # function for stacked polygons
  stackfunc <- function(ymat, ylab, x=catchyrs, hashyrs=NULL){
    ## call to function in plotrix (formerly copied into r4ss)
    if(length(order)==ncol(ymat)) ymat <- ymat[,order]
    stackpoly(x=x, y=ymat, border="black",
              xlab=xlab, ylab=ylab, col=fleetcols[order], x.hash=hashyrs)
    if(showlegend){
      legend(legendloc, fill=fleetcols[!ghost], legend=fleetnames[!ghost], bty="n")
    }
    return(TRUE)
  } # end stackfunc

  barfunc <- function(ymat,ylab,ymax=NULL,x=catchyrs){
    # adding labels to barplot as suggested by Mike Prager on R email list:
    #    http://tolstoy.newcastle.edu.au/R/e2/help/07/03/13013.html
    if(is.null(ymax)){
      ylim <- NULL
    }else{
      ylim <- c(0,ymax)
    }
    if(length(order)==ncol(ymat)) ymat <- ymat[,order]
    mp <- barplot(t(ymat), xlab=xlab, ylab=ylab,axisnames=FALSE,ylim=ylim,
                  col=fleetcols[order],space=0,yaxs='i', axes=FALSE)
    # Get major and minor multiples for choosing labels:
    ntick <- length(mp)
      { if (ntick < 16) mult = c(2, 2)
      else if(ntick < 41) mult = c(5, 5)
      else if (ntick < 101) mult = c(10, 5) else mult = c(20, 5)
      }
    # vertical axis
    if(nfishfleets > 1){
      ymax2 <- round(max(apply(ymat,1,sum)))
    }else{
      ymax2 <- round(max(ymat))
    }
    yticks <- pretty(c(0,ymax2))
    if(addmax) yticks <- sort(c(yticks,ymax2))
    axis(2,at=yticks)
    label.index <- which(x %% mult[1] == 0)
    minor.index <- which(x %% mult[2] == 0)
    for(i in 1:length(yticks)) lines(x=c(-100,ntick),y=rep(yticks[i],2),
                                     lty=3,col=rgb(0,0,0,.3),lwd=1)
    # Draw all ticks:
    axis(side = 1, at = mp, labels = FALSE, tcl = -0.2)
    # Draw minor ticks:
    axis(side = 1, at = mp[minor.index], labels = FALSE, tcl = -0.5)
    # Draw major ticks & labels:
    axis(side = 1, at = mp[label.index], labels = x[label.index], tcl = -0.7)

    # add legend
    if(showlegend) legend(legendloc, fill=fleetcols[!ghost], legend=fleetnames[!ghost], bty="n")
    return(TRUE)
  }

  # choose one of the above functions
  if(catchbars){
    stackfunc <- barfunc # unsophisticated way to implement choice of plot type
  }
  
  makeplots <- function(subplot){
    a <- FALSE
    if(subplot==1) a <- linefunc(ymat=retmat, ymax=ymax, ylab=labels[3], addtotal=TRUE)
    if(subplot==2) a <- stackfunc(ymat=retmat, ylab=labels[3])
    # if observed catch differs from estimated by more than 0.1%, then make plot to compare
    if(subplot==3 & diff(range(retmat-totobscatchmat))/max(totobscatchmat) > 0.001){
      a <- linefunc(ymat=retmat, ylab=paste(labels[9],labels[3]), addtotal=FALSE,
                    ymax=max(totobscatchmat,retmat))
      for(f in 1:nfishfleets){
        if(max(totobscatchmat[,f])>0){
          lines(catchyrs, totobscatchmat[,f], type=type, col=fleetcols[f],
                lty=3, lwd=lwd, pch=4)
        }
      }
      legend(legendloc, lty=c(fleetlty[!ghost],rep(3,sum(!ghost))), lwd=lwd,
             pch=c(fleetpch[!ghost],rep(4,sum(!ghost))), col=fleetcols[!ghost],
             legend=c(fleetnames[!ghost],paste(fleetnames[!ghost],"obs.")), bty="n")
    }
    if(max(discmat,na.rm=TRUE)>0){
      if(subplot==4) a <- linefunc(ymat=totcatchmat, ymax=ymax, ylab=labels[4], addtotal=TRUE)
      if(subplot==5 & nfishfleets>1) a <- stackfunc(ymat=totcatchmat, ylab=labels[4])
      if(subplot==6) a <- linefunc(ymat=discmat, ymax=ymax, ylab=labels[5], addtotal=TRUE)
      if(subplot==7 & nfishfleets>1) a <- stackfunc(ymat=discmat,ylab=labels[5])
      if(subplot==8) a <- linefunc(ymat=discfracmat, ymax=ymax, ylab=labels[6], addtotal=FALSE)
    }
    if(subplot==9) a <- linefunc(ymat=Hratemat, ymax=ymax, ylab=ylabF, addtotal=FALSE)
    if(nseasons>1){
      if(subplot==10) a <- linefunc(ymat=retmat2, ymax=ymax, ylab=paste(labels[3],labels[10]), addtotal=TRUE, x=catchyrs2)
      if(subplot==11 & nfishfleets>1) a <- stackfunc(ymat=retmat2, ylab=paste(labels[3],labels[10]), x=catchyrs2)
      if(max(discmat,na.rm=TRUE)>0){
        if(subplot==12) a <- linefunc(ymat=totcatchmat2, ymax=ymax, ylab=paste(labels[4],labels[10]), addtotal=TRUE, x=catchyrs2)
        if(subplot==13 & nfishfleets>1) a <- stackfunc(ymat=totcatchmat2, ylab=paste(labels[4],labels[10]), x=catchyrs2)
        if(subplot==14) a <- linefunc(ymat=discmat2, ymax=ymax, ylab=paste(labels[5],labels[10]), addtotal=TRUE, x=catchyrs2)
        if(subplot==15 & nfishfleets>1) a <- stackfunc(ymat=discmat2,ylab=paste(labels[5],labels[10]), x=catchyrs2)
      }
    }
    if(verbose & a) cat("  finished catch subplot",subplot_names[subplot],"\n")
    return(a)
  } # end makeplots

  if(plot) for(isubplot in subplots) makeplots(isubplot)

  if(print){
    for(isubplot in subplots){
      a <- FALSE
      myname <- subplot_names[isubplot]
      badstrings <- c(":","  ","__")
      for(i in 1:length(badstrings)){
        myname <- gsub(pattern=badstrings[i],replacement=" ",x=myname,fixed=T)
      }
      filename <- paste0("catch", myname, ".png")
      plotinfo2 <- pngfun(filename, caption=substring(myname,3))
      # "a" is TRUE/FALSE indicator that plot got produced
      a <- makeplots(isubplot)
      dev.off()
      # delete empty files if they somehow got created
      if(!a & file.exists(filename)){
        file.remove(filename)
      }
      if(a) plotinfo <- plotinfo2
    }
  }

  totcatchmat <- as.data.frame(totcatchmat)
  totobscatchmat <- as.data.frame(totobscatchmat)
  names(totcatchmat) <- fleetnames[1:nfishfleets]
  names(totobscatchmat) <- fleetnames[1:nfishfleets]
  totcatchmat$Yr <- catchyrs
  totobscatchmat$Yr <- catchyrs
  returnlist <- list()
  returnlist[["totcatchmat"]] <- totcatchmat
  returnlist[["totobscatchmat"]] <- totobscatchmat
  if(nseasons > 1){
    totcatchmat2 <- as.data.frame(totcatchmat2)
    names(totcatchmat2) <- fleetnames[1:nfishfleets]
    #totcatchmat2$Yr <- totcatchmat2Yr
    returnlist[["totcatchmat2"]] <- totcatchmat2
  }
  if(!is.null(plotinfo)) plotinfo$category <- "Catch"
  returnlist$plotinfo <- plotinfo
  return(invisible(returnlist))
  # if(verbose) cat("  finished catch plots\n")
}
