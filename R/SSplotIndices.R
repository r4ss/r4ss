#' Plot indices of abundance and associated quantities.
#' 
#' Plot indices of abundance and associated quantities.
#' 
#' 
#' @param replist list created by \code{SS_output}
#' @param subplots vector controlling which subplots to create
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param fleets optional vector to subset fleets for which plots will be made
#' @param fleetnames optional replacement for fleenames used in data file
#' @param smooth add smoothed line to plots of observed vs. expected sample
#' sizes
#' @param add add to existing plot (not yet implemented)
#' @param datplot make plot of data only?
#' @param labels vector of labels for plots (titles and axis labels)
#' @param col1 vector of colors for points in each season for time series plot.
#' Default is red for single season models and a rainbow using the
#' rich.colors.short function for multiple seasons.
#' @param col2 vector of colors for points in each season for obs. vs. exp.
#' plot.  Default is blue for single season models and a rainbow using the
#' rich.colors.short function for multiple seasons.
#' @param col3 color of line showing expected index in time series plot.
#' Default is blue.
#' @param col4 color of smoother shown in obs. vs. exp. plots. Default is red.
#' @param pch1 single value or vector of plotting characters (pch parameter)
#' for time-series plots of index fit. Default=21.
#' @param pch2 single value or vector of plotting characters (pch parameter)
#' for sample size plots of index fit. Default=16.
#' @param cex character expansion factor for points showing observed values.
#' Default=1.
#' @param bg Background color for points with pch=21.
#' @param legend add a legend to seasonal colors (only for seasonal models)
#' @param legendloc add a legend to seasonal colors (default is "topright")
#' @param seasnames optional vector of names for each season to replace
#' defaults if a legend is used
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @param res resolution for PNG file
#' @param ptsize point size for PNG file
#' @param cex.main character expansion for plot titles
#' @param addmain switch which allows the plot title to be left off
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param minyr First year to show in plot (for zooming in on a subset of
#' values)
#' @param maxyr Last year to show in plot (for zooming in on a subset of
#' values)
#' @param maximum_ymax_ratio Maximum allowed value for ymax (specified 
#' as ratio of y), which overrides any 
#' value of ymax that is greater (default = Inf)
#' @param show_input_uncertainty switch controlling whether to add thicker
#' uncertainty interval lines indicating the input uncertainty relative to
#' the total uncertainty which may result from estimating a parameter for
#' extra standard deviations
#' @param verbose report progress to R GUI?
#' @param \dots Extra arguments to pass to calls to \code{plot}
#' @author Ian Stewart, Ian Taylor, James Thorson
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
SSplotIndices <-
function(replist,subplots=1:9,
         plot=TRUE,print=FALSE,
         fleets="all",fleetnames="default",
         smooth=TRUE,add=FALSE,datplot=FALSE,
         labels=c("Year",        #1
           "Index",              #2
           "Observed index",     #3
           "Expected index",     #4
           "Log index",          #5
           "Log observed index", #6
           "Log expected index", #7
           "Standardized index", #8
           "Catchability (Q)",   #9
           "Time-varying catchability", #10
           "Vulnerable biomass", #11
           "Catchability vs. vulnerable biomass"), #12
         col1="default", col2="default", col3="blue", col4="red",
         pch1=21, pch2=16, cex=1, bg="white",
         legend=TRUE, legendloc="topright", seasnames=NULL,
         pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
         addmain=TRUE,plotdir="default", minyr=NULL, maxyr=NULL,
         maximum_ymax_ratio=Inf, show_input_uncertainty=TRUE, verbose=TRUE, ...)
{
  # get some quantities from replist
  cpue              <- replist$cpue
  SS_versionNumeric <- replist$SS_versionNumeric

  # confirm that some CPUE values are present
  if(is.null(dim(cpue))){
    cat("skipping index plots: no CPUE data in this model\n")
    return()
  }

  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  if(length(grep("supr_per",cpue$Supr_Per))){
    cat("Note: some indices have superperiods. Values will be plotted in year/season associated with data in report file.\n")
    cpue <- cpue[!is.na(cpue$Dev),]
  }
  
  FleetNames  <- replist$FleetNames
  nfleets     <- replist$nfleets
  nseasons    <- replist$nseasons

  # find any extra SD parameters
  parameters  <- replist$parameters
  Q_extraSD_info <- parameters[grep("Q_extraSD", parameters$Label),]
  # calculate how many of these parameters there are
  nSDpars <- nrow(Q_extraSD_info)
  if(nSDpars > 0){
    # parse the parameter label to get the fleet number
    Q_extraSD_info$FleetNum <- NA
    for(ipar in 1:nSDpars){
      if(SS_versionNumeric >= 3.3){
        # parsing label with ending like "(2)" assuming only one set of parentheses
        num <- strsplit(Q_extraSD_info$Label[ipar], split="[()]", fixed=FALSE)[[1]][2]
      }else{
        num <- strsplit(substring(Q_extraSD_info$Label[ipar], nchar("Q_extraSD_")+1),
                        split="_", fixed=TRUE)[[1]][1]
      }
      Q_extraSD_info$FleetNum[ipar] <- as.numeric(num)
    }
    # NOTE: important columns in Q_extraSD_info to use below are $Value and $FleetNum
  }
  if(nseasons>1){
    # if seasons, put CPUE at season midpoint
    cpue$YrSeas <- cpue$Yr + (cpue$Seas - 0.5)/nseasons
  }else{
    # if no seasons, put at integer year value
    cpue$YrSeas <- cpue$Yr
  }
  if(plotdir=="default") plotdir <- replist$inputs$dir

  if(fleetnames[1]=="default") fleetnames <- FleetNames
  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{ if(length(intersect(fleets,1:nfleets))!=length(fleets)){
    return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
  }}
  
  # subset fleets as requested
  fleetvec <- intersect(fleets, unique(as.numeric(cpue$FleetNum)))

  # use fancy colors only if any index spans more than one season
  usecol <- FALSE
  for(ifleet in fleetvec){
    if(length(unique(cpue$Seas[cpue$Obs > 0 & cpue$FleetNum==ifleet])) > 1){
      usecol <- TRUE
    }else{
      legend=FALSE
    }
  }

  if(col1[1]=="default"){
    colvec1 <- "black"
    if(usecol & nseasons==4) colvec1 <- c("blue4","green3","orange2","red3")
    if(usecol & !nseasons %in% c(1,4)) colvec1 <- rich.colors.short(nseasons)
  }else{
    colvec1 <- col1
  }
  if(col2[1]=="default"){
    colvec2 <- "blue"
    if(usecol & nseasons==4) colvec2 <- c("blue4","green3","orange2","red3")
    if(usecol & !nseasons %in% c(1,4)) colvec2 <- rich.colors.short(nseasons)
  }else{
    colvec2 <- col2
  }
  if(is.null(seasnames)) seasnames <- paste("Season",1:nseasons,sep="")


  allcpue <- data.frame()

  # loop over fleets
  for(ifleet in fleetvec){
    Fleet <- fleetnames[ifleet]
    cpueuse <- cpue[cpue$Obs > 0 & cpue$FleetNum==ifleet,]
    cpueuse <- cpueuse[order(cpueuse$YrSeas),]
    # look for time-vary
    time <- diff(range(cpueuse$Calc_Q))>0
    # look for time-varying effective Q
    time2 <- diff(range(cpueuse$Eff_Q))>0
    # Teresa's model had NA values in Eff_Q for unknown reasons
    # line below will allow model to play on
    if(is.na(time2)){
      time2 <- FALSE
    }
    # look for extra SD and calculate input SD (if different from final value)
    if(exists("Q_extraSD_info") && ifleet %in% Q_extraSD_info$FleetNum){
      # input uncertainty is final value minus extra SD parameter (if present)
      cpueuse$SE_input <- cpueuse$SE - Q_extraSD_info$Value[Q_extraSD_info$FleetNum==ifleet]
    }else{
      cpueuse$SE_input <- NULL # could also set equal to $SE but then additional test required to not display
    }
    # use short variable names for often-used quantities
    x <- cpueuse$YrSeas
    y <- cpueuse$Obs
    z <- cpueuse$Exp
    include <- !is.na(cpueuse$Like)
    if(any(include)){
      if(usecol){
        s <- cpueuse$Seas
      }else{
        s <- 1 # only use colorvector if more than 1 season
      }
      if(datplot){
        cpueuse$Index <- rep(ifleet,length(cpueuse$YrSeas))
        cpueuse$stdvalue <- cpueuse$Obs/mean(cpueuse$Obs)
        tempcpue <- cbind(cpueuse$Index,cpueuse$YrSeas,cpueuse$Obs,cpueuse$stdvalue)
        colnames(tempcpue) <- c("Index","year","value","stdvalue")
        allcpue <- rbind(allcpue,tempcpue)
      }
      uiw <- qlnorm(.975,meanlog=log(y),sdlog=cpueuse$SE) - y
      if(max(uiw)==Inf){
        cat("!warning: removing upper interval on indices with infinite upper quantile values\n",
            "         check the uncertainty inputs to for the indices\n")
        uiw[uiw==Inf] <- 1000*max(cpueuse$Obs[uiw==Inf])
      }
      liw <- y - qlnorm(.025,meanlog=log(y),sdlog=cpueuse$SE)
      npoints <- length(z)
      main=paste(labels[2], Fleet,sep=" ")
      if(!addmain) main <- ""

      addlegend <- function(pch, colvec){
        names <- paste(seasnames,"observations")
      }
      # print(cbind(x, y, liw, uiw)) # debugging line
      cpuefun1 <- function(addexpected=TRUE, ...){
        # plot of time-series of observed and expected (if requested)
        xlim <- c(max(minyr,min(x)),min(maxyr,max(x)))
        if(!add) plot(x=x[include], y=y[include], type='n', xlab=labels[1],
                      ylab=labels[2], main=main, cex.main=cex.main, xlim=xlim,
                      ylim=c(0,min(max(y+uiw,na.rm=TRUE), max(maximum_ymax_ratio*y))),
                      ...)
        # show thicker lines behind final lines for input uncertainty (if different)
        if(show_input_uncertainty && any(!is.null(cpueuse$SE_input[include]))){
          segments(x[include], qlnorm(.025,meanlog=log(y[include]),sdlog=cpueuse$SE_input[include]),
                   x[include], qlnorm(.975,meanlog=log(y[include]),sdlog=cpueuse$SE_input[include]),
                   col = colvec1[s], lwd = 3, lend = 1)
        }
        # add intervals
        plotCI(x=x[include],y=y[include],sfrac=0.005,uiw=uiw[include],liw=liw[include],
               ylo=0,col=colvec1[s],
               main=main,cex.main=cex.main,lty=1,add=TRUE,pch=pch1,
               bg=bg,cex=cex)
        abline(h=0,col="grey")
        if(addexpected){
          lines(x,z,lwd=2,col=col3)
        }
        if(legend & length(colvec1)>1){
          legend(x=legendloc, legend=seasnames, pch=pch1, col=colvec1, cex=cex)
        }
      }
      cpuefun2 <- function(...){
        # plot of observed vs. expected with smoother
        if(!add) plot(y[include],z[include],xlab=labels[3],main=main,cex.main=cex.main,
                      ylim=c(0,max(z)),xlim=c(0,max(y)),ylab=labels[4], ...)
        points(y[include],z[include],col=colvec2[s],pch=pch2,cex=cex)
        abline(h=0,col="grey")
        lines(x=c(0,max(z[include])),y=c(0,max(z[include])))
        if(smooth && npoints > 6 && diff(range(y))>0){
          psmooth <- loess(z[include]~y[include],degree=1)
          lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],
                lwd=1.2,col=col4,lty="dashed")
        }
        if(legend & length(colvec2)>1) legend(x=legendloc, legend=seasnames,
                                              pch=pch2, col=colvec2, cex=cex)
      }

      if(plot){
        if(1 %in% subplots & datplot) cpuefun1(addexpected=FALSE)
        if(2 %in% subplots) cpuefun1()
        if(3 %in% subplots) cpuefun2()
      }
      if(print){
        if(1 %in% subplots & datplot){
          file <- paste0("index1_cpuedata_",Fleet,".png")
          caption <- paste0("Index data for ", Fleet, ". ",
                            "Lines indicate 95% uncertainty interval around index values. ",
                            "Thicker lines (if present) indicate input uncertainty before addition of ",
                            "estimated additional uncertainty parameter.")
          plotinfo <- pngfun(file=file, caption=caption)
          cpuefun1(addexpected=FALSE)
          dev.off()
        }
        if(2 %in% subplots){
          file <- paste0("index2_cpuefit_",Fleet,".png")
          caption <- paste0("Fit to index data for ", Fleet,". ",
                            "Lines indicate 95% uncertainty interval around index values. ",
                            "Thicker lines (if present) indicate input uncertainty before addition of ",
                            "estimated additional uncertainty parameter.")
          plotinfo <- pngfun(file=file, caption=caption)
          cpuefun1()
          dev.off()
        }
        if(3 %in% subplots){
          file <- paste0("index3_cpuecheck_",Fleet,".png")
          caption <- paste("Observed vs. expected index values with smoother for",Fleet)
          plotinfo <- pngfun(file=file, caption=caption)
          cpuefun2()
          dev.off()
        }
      }

      # same plots again in log space (someday should create generalized set of commands)
      main <- paste(labels[5], Fleet, sep=" ")
      if(!addmain) main <- ""
      uiw <- qnorm(.975,mean=log(y),sd=cpueuse$SE) - log(y)
      liw <- log(y) - qnorm(.025,mean=log(y),sd=cpueuse$SE)
      cpuefun3 <- function(addexpected=TRUE){
        # plot of time-series of log(observed) and log(expected) (if requested)
        xlim <- c(max(minyr,min(x)),min(maxyr,max(x)))
        if(!add) plot(x=x[include], y=log(y[include]), type='n',
                      xlab=labels[1], ylab=labels[5],
                      main=main, cex.main=cex.main,
                      xlim=xlim, ylim=range(log(y[include])-liw[include],
                                     log(y[include])+uiw[include],na.rm=TRUE))
        # show thicker lines behind final lines for input uncertainty (if different)
        if(show_input_uncertainty & any(!is.null(cpueuse$SE_input[include]))){
          segments(x[include], qnorm(.025,mean=log(y[include]),sd=cpueuse$SE_input[include]),
                   x[include], qnorm(.975,mean=log(y[include]),sd=cpueuse$SE_input[include]),
                   col = colvec1[s], lwd = 3, lend = 1)
        }
        plotCI(x=x[include],y=log(y[include]),sfrac=0.005,uiw=uiw[include],
               liw=liw[include],
               col=colvec1[s],lty=1,add=TRUE,pch=pch1,bg=bg,cex=cex)
        if(addexpected) lines(x,log(z),lwd=2,col=col3)
        if(length(colvec1)>1) legend(x=legendloc, legend=seasnames,
                                     pch=pch1, col=colvec1, cex=cex)
      }
      cpuefun4 <- function(){
        # plot of log(observed) vs. log(expected) with smoother
        if(!add) plot(log(y[include]),log(z[include]),type='n',xlab=labels[6],main=main,
                      cex.main=cex.main,ylab=labels[7])
        points(log(y[include]),log(z[include]),col=colvec2[s],pch=pch2)
        lines(x=range(log(z[include])),y=range(log(z[include])))
        if(smooth && npoints > 6 && diff(range(y))>0){
          psmooth <- loess(log(z[include])~log(y[include]),degree=1)
          lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],
                lwd=1.2,col=col4,lty="dashed")}
        if(length(colvec2)>1) legend(x=legendloc, legend=seasnames,
                                     pch=pch2, col=colvec2, cex=cex)
      }
      cpuefun5 <- function(){
        # plot of time-varying catchability (if present)
        main <- paste(labels[10], Fleet, sep=" ")
        if(!addmain) main <- ""
        q <- cpueuse$Calc_Q
        if(!add) plot(x,q,type='o',xlab=labels[1],main=main,
                      cex.main=cex.main,ylab=labels[9],
                      col=colvec2[1],pch=pch2)
      }
      cpuefun6 <- function(){
        # plot of time-varying catchability (if present)
        main <- paste(labels[12], Fleet, sep=" ")
        if(!addmain) main <- ""
        v <- cpueuse$Vuln_bio
        q1 <- cpueuse$Calc_Q
        q2 <- cpueuse$Eff_Q
        if(all(q1==q2)) ylab <- labels[9] else ylab <- "Effective catchability"
        if(!add) plot(v,q2,type='o',xlab=labels[11],main=main,
                      cex.main=cex.main,ylab=ylab,
                      col=colvec2[1],pch=pch2)
      }
      if(plot){
        if(4 %in% subplots & datplot) cpuefun3(addexpected=FALSE)
        if(5 %in% subplots) cpuefun3()
        if(6 %in% subplots) cpuefun4()
        if(7 %in% subplots & time) cpuefun5()
        if(8 %in% subplots & time2) cpuefun6()
      }
      
      if(print){
        if(4 %in% subplots & datplot){
          file <- paste0("index4_logcpuedata_",Fleet,".png")
          caption <- paste0("Log index data for ", Fleet, ". ",
                            "Lines indicate 95% uncertainty interval around index values. ",
                            "Thicker lines (if present) indicate input uncertainty before addition of ",
                            "estimated additional uncertainty parameter.")
          plotinfo <- pngfun(file=file, caption=caption)
          cpuefun3(addexpected=FALSE)
          dev.off()
        }
        if(5 %in% subplots){
          file <- paste0("index5_logcpuefit_",Fleet,".png")
          caption <- paste0("Fit to log index data on log scale for ", Fleet, ". ",
                            "Lines indicate 95% uncertainty interval around index values. ",
                            "Thicker lines (if present) indicate input uncertainty before addition of ",
                            "estimated additional uncertainty parameter.")
          plotinfo <- pngfun(file=file, caption=caption)
          cpuefun3()
          dev.off()
        }
        if(6 %in% subplots){
          file <- paste0("index6_logcpuecheck_",Fleet,".png")
          caption <- paste("log(observed) vs. log(expected) index values with smoother for",Fleet)
          plotinfo <- pngfun(file=file, caption=caption)
          cpuefun4()
          dev.off()
        }
        if(7 %in% subplots & time){
          file <- paste0("index7_timevaryingQ_",Fleet,".png")
          caption <- paste("Timeseries of catchability for",Fleet)
          plotinfo <- pngfun(file=file, caption=caption)
          cpuefun5()
          dev.off()
        }
        if(8 %in% subplots & time2){
          file <- paste0("index8_Q_vs_Vuln_bio_",Fleet,".png")
          caption <-
            paste("Catchability vs. vulnerable biomass for fleet ",Fleet,"<br> \n",
                  "This plot should illustrate curvature of nonlinear catchability relationship<br> \n",
                  "Or reveal patterns associated with random-walk catchability<br> \n",
                  "It was inspired by Jim Thorson, so blame him if you don't like it.",sep="")
          plotinfo <- pngfun(file=file, caption=caption)
          cpuefun6()
          dev.off()
        }
      }
    } # end check for any values to include
  } # nfleets

  ### New the standardized plot of all CPUE indices
  if(datplot==TRUE & nrow(allcpue)>0){
    all_cpue_fun <- function(){
      main="All cpue plot"
      if(!addmain) main <- ""
      xlim <- c(min(allcpue$year,na.rm=TRUE)-1,max(allcpue$year,na.rm=TRUE)+1)

      # change range if requested
      xlim[1] <- max(xlim[1],minyr)
      xlim[2] <- min(xlim[2],maxyr)
      
      ylim <- c(range(allcpue$stdvalue,na.rm=TRUE))
      usecols <- rich.colors.short(max(allcpue$Index,na.rm=TRUE))
      if(max(allcpue$Index,na.rm=TRUE) >= 2){
        usecols <- rich.colors.short(max(allcpue$Index,na.rm=TRUE)+1)[-1]
      }
      if(!add) plot(0, type="n", xlab=labels[1], main=main, cex.main=cex.main,
                    col=usecols[1], ylab=labels[8], xlim=xlim,ylim=ylim)
      for(ifleet in fleetvec){
        points(x=allcpue$year[allcpue$Index==ifleet],
               y=allcpue$stdvalue[allcpue$Index==ifleet],
               pch=pch2, col=usecols[ifleet], cex=cex, lwd=0.4, lty="dashed", type="o")
      }
    } # end all_cpue_fun
    if(plot & (9 %in% subplots)){all_cpue_fun()}
    if(print & (9 %in% subplots)){
      file <- paste0("index9_standcpueall",".png")
      caption <- "Standardized indices overlaid"
      plotinfo <- pngfun(file=file, caption=caption)
      all_cpue_fun()
      dev.off()}
  } # end datplot

  if(!is.null(plotinfo)) plotinfo$category <- "Index"
  return(invisible(plotinfo))
} # end function
