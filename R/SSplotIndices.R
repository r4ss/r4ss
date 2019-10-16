#' Plot indices of abundance and associated quantities.
#' 
#' Plot indices of abundance with or without model fit as well as other diagnostic
#' plots such as observed vs. expected index and plots related to time-varying
#' catchability (if present).
#' 
#' 
#' @param replist list created by \code{SS_output}
#' @param subplots vector controlling which subplots to create
#' Numbering of subplots is as follows:
#' \itemize{
#'   \item 1  index data by fleet
#'   \item 2  index data with fit by fleet
#'   \item 3  observed vs expected index values with smoother
#'   \item 4  index data by fleet on a log scale (lognormal error only)
#'   \item 5  index data with fit by fleet on a log scale (lognormal error only)
#'   \item 6  log(observed) vs log(expected) with smoother (lognormal error only)
#'   \item 7  time series of time-varying catchability (only if actually time-varying)
#'   \item 8  catchability vs. vulnerable biomass (if catchability is not constant)
#'   \item 9  comparison of all indices
#' }
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
#' @param mainTitle switch which allows the plot title to be left off
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
function(replist,subplots=c(1:9),
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
         mainTitle=TRUE,plotdir="default", minyr=NULL, maxyr=NULL,
         maximum_ymax_ratio=Inf, show_input_uncertainty=TRUE, verbose=TRUE, ...)
{
  # get some quantities from replist
  cpue              <- replist$cpue
  SS_versionNumeric <- replist$SS_versionNumeric

  # confirm that some CPUE values are present
  if(is.null(dim(cpue))){
    message("skipping index plots: no index data in this model")
    return()
  }

  # define a bunch of internal functions
  
  # subfunction to write png files
  pngfun <- function(file, caption=NA){
    png(filename=file.path(plotdir, file),
        width=pwidth, height=pheight, units=punits, res=res, pointsize=ptsize)
    plotinfo <- rbind(plotinfo, data.frame(file=file, caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL

  
  index.fn <- function(addexpected = TRUE, log = FALSE, ...){
    # plot of time series of observed values with fit (if requested)

    # don't do anything if error structure is not lognormal
    if(error != 0 & log == TRUE){
      return()
    }
    # interval around points with total SE (input + any estimated extra)
    if(error == 0){
      if(!log){
        lower_total <- qlnorm(.025, meanlog = log(y[include]),
                              sdlog = cpueuse$SE[include])
        upper_total <- qlnorm(.975, meanlog = log(y[include]),
                              sdlog = cpueuse$SE[include])
      }else{
        lower_total <- qnorm(.025, mean = log(y[include]),
                              sd = cpueuse$SE[include])
        upper_total <- qnorm(.975, mean = log(y[include]),
                              sd = cpueuse$SE[include])
      }
    }
    # normal error interval
    if(error == -1){ 
      lower_total <- qnorm(.025, mean = y[include], sd = cpueuse$SE[include])
      upper_total <- qnorm(.975, mean = y[include], sd = cpueuse$SE[include])
    }
    # T-distribution interval
    if(error > 0){ 
      lower_total <- -cpueuse$SE[include]*qt(.025, df = y[include])
      upper_total <-  cpueuse$SE[include]*qt(.975, df = y[include])
    }

    if(max(upper_total)==Inf){
      warning("Removing upper interval on indices with infinite upper quantile values.\n",
              "Check the uncertainty inputs for the indices.")
      upper_total[upper_total == Inf] <- 100*max(cpueuse$Obs[upper_total == Inf])
    }

    # plot title
    main <- paste0(labels[2], Fleet)
    if(log){
      main <- paste0(labels[5], Fleet)
    }
    
    # no title
    if(!mainTitle){
      main <- ""
    }

    xlim <- c(max(minyr,min(x)), min(maxyr,max(x)))
    if(!add){
      # y-limits with lognormal error
      if(error == 0){
        if(!log){
          # ylim for standard scale
          ylim <- c(0, 1.05*min(max(upper_total, na.rm = TRUE),
                                max(maximum_ymax_ratio * y)))
        }
        if(log){
          # ylim for log scale plot
          ylim <- range(c(lower_total, upper_total), na.rm = TRUE)
        }
      }
      # ylimits with normal or T-distributed error
      if(error != 0){
        ylim <- 1.05 * c(min(lower_total, na.rm = TRUE),
                         max(upper_total, na.rm = TRUE))
      }
      
      plot(x = x[include], y = y[include], type = 'n', xlab = labels[1],
           ylab = ifelse(!log, labels[2], labels[5]),
           main = main, cex.main = cex.main, xlim = xlim,
           ylim = ylim,
           yaxs = ifelse(log, 'r', 'i'),
           ...)
    }
    # show thicker lines behind final lines for input uncertainty (if different)
    if(show_input_uncertainty && any(!is.null(cpueuse$SE_input[include]))){
      # lognormal error interval
      if(error == 0){
        if(!log){
          lower_input <- qlnorm(.025, meanlog = log(y[include]),
                                sdlog = cpueuse$SE_input[include])
          upper_input <- qlnorm(.975, meanlog = log(y[include]),
                                sdlog = cpueuse$SE_input[include])
        }else{
          lower_input <- qnorm(.025, mean = log(y[include]),
                                sd = cpueuse$SE_input[include])
          upper_input <- qnorm(.975, mean = log(y[include]),
                                sd = cpueuse$SE_input[include])
        }
      }
      # normal error interval
      if(error == -1){ 
        lower_input <- qnorm(.025, mean = y[include], sd = cpueuse$SE_input[include])
        upper_input <- qnorm(.975, mean = y[include], sd = cpueuse$SE_input[include])
      }
      # T-distribution interval
      if(error > 0){ 
        lower_input <- -cpueuse$SE_input[include]*qt(.025, df = y[include])
        upper_input <-  cpueuse$SE_input[include]*qt(.975, df = y[include])
      }
      # add segments
      segments(x[include], lower_input,
               x[include], upper_input,
               col = colvec1[s], lwd = 3, lend = 1)
    }

    # add intervals
    arrows(x0 = x[include], y0 = lower_total,
           x1 = x[include], y1 = upper_total,
           length = 0.03, angle = 90, code = 3, col = colvec1[s])
    # add points and expected values on standard scale
    if(!log){
      points(x = x[include], y = y[include],
             pch = pch1, cex = cex, bg = bg, col = colvec1[s])
      if(addexpected){
        lines(x, z, lwd = 2, col = col3)
      }
    }else{
      # add points and expected values on log scale
      points(x = x[include], y = log(y[include]),
             pch = pch1, cex = cex, bg = bg, col = colvec1[s])
      if(addexpected){
        lines(x, log(z), lwd = 2, col = col3)
      }
    }
    if(legend & length(colvec1)>1){
      legend(x=legendloc, legend=seasnames, pch=pch1, col=colvec1, cex=cex)
    }
  }

  obs_vs_exp.fn <- function(log = FALSE, ...){
    # plot of observed vs. expected with smoother

    # plot title
    main <- paste(labels[2], Fleet,sep=" ")
    # no title
    if(!mainTitle){
      main <- ""
    }

    if(!add){
      if(!log){
        # standard plot
        plot(y[include], z[include], type = 'n',
             xlab = labels[3], ylab = labels[4], 
             main = main, cex.main = cex.main,
             ylim = c(0, 1.05*max(z)), xlim = c(0, 1.05*max(y)),
             xaxs = 'i', yaxs = 'i', ...)
      }else{
        # log-scale plot doesn't specificy y limits
        plot(log(y[include]), log(z[include]), type='n',
             xlab=labels[6], ylab=labels[7],
             main=main, cex.main=cex.main)
      }        
    }
    if(!log){
      points(y[include],z[include],col=colvec2[s],pch=pch2,cex=cex)
    }else{
      points(log(y[include]), log(z[include]),
             col=colvec2[s], pch=pch2, cex=cex)
    }
    abline(a = 0, b = 1, lty = 3)
    if(smooth && npoints > 6 && diff(range(y))>0){
      if(!log){
        psmooth <- loess(z[include] ~ y[include], degree=1)
        lines(psmooth$x[order(psmooth$x)], psmooth$fit[order(psmooth$x)],
              lwd=1.2, col=col4, lty="dashed")
      }else{
        psmooth <- loess(log(z[include]) ~ log(y[include]), degree=1)
        lines(psmooth$x[order(psmooth$x)], psmooth$fit[order(psmooth$x)],
              lwd=1.2, col=col4, lty="dashed")
      }
    }
    if(legend & length(colvec2)>1){
      legend(x=legendloc, legend=seasnames, pch=pch2, col=colvec2, cex=cex)
    }
  }

  timevarying_q.fn <- function(){
    # plot of time-varying catchability (if present)
    main <- paste(labels[10], Fleet, sep=" ")
    if(!mainTitle) main <- ""
    q <- cpueuse$Calc_Q
    if(!add) plot(x,q,type='o',xlab=labels[1],main=main,
                  cex.main=cex.main,ylab=labels[9],
                  col=colvec2[1],pch=pch2)
  }
  
  q_vs_vuln_bio.fn <- function(){
    # plot of time-varying catchability (if present)
    main <- paste(labels[12], Fleet, sep=" ")
    if(!mainTitle) main <- ""
    v <- cpueuse$Vuln_bio
    q1 <- cpueuse$Calc_Q
    q2 <- cpueuse$Eff_Q
    if(all(q1==q2)) ylab <- labels[9] else ylab <- "Effective catchability"
    if(!add) plot(v,q2,type='o',xlab=labels[11],main=main,
                  cex.main=cex.main,ylab=ylab,
                  col=colvec2[1],pch=pch2)
  }

  # check for super periods
  if(length(grep("supr_per",cpue$Supr_Per))){
    warning("Some indices have superperiods. Values will be plotted\n",
            "in year/season associated with data in report file.")
    cpue <- cpue[!is.na(cpue$Dev),]
  }
  
  FleetNames   <- replist$FleetNames
  nfleets      <- replist$nfleets
  nseasons     <- replist$nseasons
  
  # find any extra SD parameters
  parameters  <- replist$parameters
  Q_extraSD_info <- parameters[grep("Q_extraSD", parameters$Label),]
  # calculate how many of these parameters there are
  nSDpars <- nrow(Q_extraSD_info)
  if(nSDpars > 0){
    # parse the parameter label to get the fleet number
    Q_extraSD_info$Fleet <- NA
    for(ipar in 1:nSDpars){
      if(SS_versionNumeric >= 3.3){
        # parsing label with ending like "(2)" assuming only one set of parentheses
        num <- strsplit(Q_extraSD_info$Label[ipar], split="[()]", fixed=FALSE)[[1]][2]
      }else{
        num <- strsplit(substring(Q_extraSD_info$Label[ipar], nchar("Q_extraSD_")+1),
                        split="_", fixed=TRUE)[[1]][1]
      }
      Q_extraSD_info$Fleet[ipar] <- as.numeric(num)
    }
    # NOTE: important columns in Q_extraSD_info to use below are $Value and $Fleet
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
  fleetvec <- intersect(fleets, unique(as.numeric(cpue$Fleet)))

  # use fancy colors only if any index spans more than one season
  usecol <- FALSE
  for(ifleet in fleetvec){
    if(length(unique(cpue$Seas[cpue$Fleet==ifleet])) > 1){
      usecol <- TRUE
    }
  }
  # turn off use of legend if there's never more than 1 season per index
  if(!usecol){
    legend <- FALSE
  }

  if(col1[1]=="default"){
    colvec1 <- "black"
    if(usecol & nseasons==4){
      colvec1 <- c("blue4","green3","orange2","red3")
    }
    if(usecol & !nseasons %in% c(1,4)){
      colvec1 <- rich.colors.short(nseasons)
    }
  }else{
    colvec1 <- col1
    # if user provides single value (or vector of length less than nseasons)
    # make sure it's adequate to cover all seasons
    if(length(colvec1) < nseasons){
      colvec1 <- rep(col1, nseasons)
    }
  }
  if(col2[1]=="default"){
    colvec2 <- "blue"
    if(usecol & nseasons==4){
      colvec2 <- c("blue4","green3","orange2","red3")
    }
    if(usecol & !nseasons %in% c(1,4)){
      colvec2 <- rich.colors.short(nseasons)
    }
  }else{
    colvec2 <- col2
    # if user provides single value (or vector of length less than nseasons)
    # make sure it's adequate to cover all seasons
    if(length(colvec1) < nseasons){
      colvec1 <- rep(col1, nseasons)
    }
  }
  if(is.null(seasnames)) seasnames <- paste("Season",1:nseasons,sep="")

  # empty data.frame to store data for comparison among indices
  allcpue <- data.frame()
  # keep track of whether any indices with negative observations is excluded
  any_negative <- FALSE 



  
  # loop over fleets
  for(ifleet in fleetvec){
    
    Fleet <- fleetnames[ifleet]
    error <- replist$survey_error[ifleet]

    cpueuse <- cpue[cpue$Fleet==ifleet,]
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
    if(exists("Q_extraSD_info") && ifleet %in% Q_extraSD_info$Fleet){
      # input uncertainty is final value minus extra SD parameter (if present)
      cpueuse$SE_input <- cpueuse$SE - Q_extraSD_info$Value[Q_extraSD_info$Fleet==ifleet]
    }else{
      cpueuse$SE_input <- NULL # could also set equal to $SE but then additional test required to not display
    }
    # use short variable names for often-used quantities
    x <- cpueuse$YrSeas
    y <- cpueuse$Obs
    z <- cpueuse$Exp
    npoints <- length(z)
    include <- !is.na(cpueuse$Like)
    if(any(include)){
      if(usecol){
        s <- cpueuse$Seas[which(include)]
      }else{
        s <- 1 # only use colorvector if more than 1 season
      }
      if(datplot){
        # add index data to data frame which is used to compare all indices
        if(min(cpueuse$Obs >= 0)){
          cpueuse$Index <- rep(ifleet,length(cpueuse$YrSeas))
          cpueuse$stdvalue <- cpueuse$Obs/mean(cpueuse$Obs)
          tempcpue <- cbind(cpueuse$Index,cpueuse$YrSeas,cpueuse$Obs,cpueuse$stdvalue)
          colnames(tempcpue) <- c("Index","year","value","stdvalue")
          allcpue <- rbind(allcpue,tempcpue)
        }else{
          if(verbose){
            message("Excluding fleet ", ifleet,
                    " from index comparison figure because it has negative values")
          }
          any_negative <- TRUE
        }
      }

      addlegend <- function(pch, colvec){
        names <- paste(seasnames,"observations")
      }

      if(plot){
        if(1 %in% subplots & datplot) index.fn(addexpected=FALSE)
        if(2 %in% subplots) index.fn()
        if(3 %in% subplots) obs_vs_exp.fn()
      }
      if(print){
        if(1 %in% subplots & datplot){
          file <- paste0("index1_cpuedata_",Fleet,".png")
          caption <- paste0("Index data for ", Fleet, ". ",
                            "Lines indicate 95% uncertainty interval around index values. ",
                            "Thicker lines (if present) indicate input uncertainty before addition of ",
                            "estimated additional uncertainty parameter.")
          plotinfo <- pngfun(file=file, caption=caption)
          index.fn(addexpected=FALSE)
          dev.off()
        }
        if(2 %in% subplots){
          file <- paste0("index2_cpuefit_",Fleet,".png")
          caption <- paste0("Fit to index data for ", Fleet,". ",
                            "Lines indicate 95% uncertainty interval around index values. ",
                            "Thicker lines (if present) indicate input uncertainty before addition of ",
                            "estimated additional uncertainty parameter.")
          plotinfo <- pngfun(file=file, caption=caption)
          index.fn()
          dev.off()
        }
        if(3 %in% subplots){
          file <- paste0("index3_obs_vs_exp_",Fleet,".png")
          caption <- paste("Observed vs. expected index values with smoother for",Fleet)
          plotinfo <- pngfun(file=file, caption=caption)
          obs_vs_exp.fn()
          dev.off()
        }
      }

      # same plots again in log space
      # check for lognormal error
      if(error == 0){

        # plot subplots 4-6 to current device
        if(plot){
          if(4 %in% subplots & datplot){
            index.fn(log = TRUE, addexpected = FALSE)
          }
          if(5 %in% subplots){
            index.fn(log = TRUE)
          }
          if(6 %in% subplots){
            obs_vs_exp.fn(log = TRUE)
          }
        }

        # print subplots 4-6 to PNG files
        if(print){

          if(4 %in% subplots & datplot){
            file <- paste0("index4_logcpuedata_",Fleet,".png")
            caption <- paste0("Log index data for ", Fleet, ". ",
                              "Lines indicate 95% uncertainty interval around index values. ",
                              "Thicker lines (if present) indicate input uncertainty before addition of ",
                              "estimated additional uncertainty parameter.")
            plotinfo <- pngfun(file=file, caption=caption)
            index.fn(log = TRUE, addexpected = FALSE)
            dev.off()
          }
          if(5 %in% subplots){
            file <- paste0("index5_logcpuefit_",Fleet,".png")
            caption <- paste0("Fit to log index data on log scale for ", Fleet, ". ",
                              "Lines indicate 95% uncertainty interval around index values. ",
                              "Thicker lines (if present) indicate input uncertainty before addition of ",
                              "estimated additional uncertainty parameter.")
            plotinfo <- pngfun(file=file, caption=caption)
            index.fn(log = TRUE)
            dev.off()
          }
          if(6 %in% subplots){
            file <- paste0("index6_log_obs_vs_exp_",Fleet,".png")
            caption <- paste("log(observed) vs. log(expected) index values with smoother for",Fleet)
            plotinfo <- pngfun(file=file, caption=caption)
            obs_vs_exp.fn(log = TRUE)
            dev.off()
          }
        }
      } # end plots that require lognormal error

      # plots 7 and 8 related to time-varying catchability
      if(plot){
        if(7 %in% subplots & time){
          timevarying_q.fn()
        }
        if(8 %in% subplots & time2){
          q_vs_vuln_bio.fn()
        }
      } # end plot to graphics device

      if(print){
        if(7 %in% subplots & time){
          file <- paste0("index7_timevarying_q_",Fleet,".png")
          caption <- paste("Timeseries of catchability for",Fleet)
          plotinfo <- pngfun(file=file, caption=caption)
          timevarying_q.fn()
          dev.off()
        }
        if(8 %in% subplots & time2){
          file <- paste0("index8_q_vs_vuln_bio_",Fleet,".png")
          caption <-
            paste0("Catchability vs. vulnerable biomass for fleet ", Fleet, "<br> \n",
                   "This plot should illustrate curvature of nonlinear catchability relationship<br> \n",
                   "or reveal patterns associated with random-walk catchability.")
          plotinfo <- pngfun(file=file, caption=caption)
          q_vs_vuln_bio.fn()
          dev.off()
        }
      } # end print to PNG
    } # end check for any values to include
  } # end loop over fleets

  ### standardized plot of all CPUE indices
  if(datplot==TRUE & nrow(allcpue)>0){
    all_index.fn <- function(){
      main <- "All index plot"
      if(!mainTitle){
        main <- ""
      }
      xlim <- c(min(allcpue$year,na.rm=TRUE) - 1,
                max(allcpue$year,na.rm=TRUE) + 1)

      # change year range if requested
      xlim[1] <- max(xlim[1],minyr)
      xlim[2] <- min(xlim[2],maxyr)

      # set y limits
      ylim <- c(range(allcpue$stdvalue,na.rm=TRUE))
      # set colors
      usecols <- rich.colors.short(max(allcpue$Index,na.rm=TRUE), alpha = 0.7)
      if(max(allcpue$Index,na.rm=TRUE) >= 2){
        usecols <- rich.colors.short(max(allcpue$Index,na.rm=TRUE)+1,
                                     alpha = 0.7)[-1]
      }
      # make empty plot
      if(!add) plot(0, type="n", xlab=labels[1], main=main, cex.main=cex.main,
                    col=usecols[1], ylab=labels[8], xlim=xlim,ylim=ylim)
      # add points and lines for each fleet
      for(ifleet in fleetvec){
        points(x=allcpue$year[allcpue$Index==ifleet],
               y=allcpue$stdvalue[allcpue$Index==ifleet],
               pch=pch2, col=usecols[ifleet], cex=cex,
               lwd=1, lty="dashed", type="o")
      }
    } # end all_index.fn
    if(plot & (9 %in% subplots)){
      all_index.fn()
    }
    if(print & (9 %in% subplots)){
      file <- paste0("index9_standcpueall",".png")
      caption <- "Standardized indices overlaid"
      if(any_negative){
        caption <- paste0(caption,
                          ". Indices with negative observations have been excluded.")
      }
      plotinfo <- pngfun(file=file, caption=caption)
      all_index.fn()
      dev.off()}
  } # end datplot

  if(!is.null(plotinfo)) plotinfo$category <- "Index"
  return(invisible(plotinfo))
} # end function
