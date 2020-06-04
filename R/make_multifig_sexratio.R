#' Create multi-figure sex ratio plots.
#'
#' Modified version of \code{\link{make_multifig}} for multi-figure
#' plots of sex ratio data with crude confidence intervals (+/i 1 se) and
#' fits from Stock Synthesis output.
#'
#' @param dbase element of list created by \code{\link{SS_output}} passed from
#' \code{\link{SSplotSexRatio}}
#' @param sexratio.option code to choose among (1) female:male ratio or
#' (2) fraction females out of the total (the default)
#' @param CI confidence interval for uncertainty
#' @param sampsizeround rounding level for sample size values
#' @param maxrows maximum (or fixed) number or rows of panels in the plot
#' @param maxcols maximum (or fixed) number or columns of panels in the plot
#' @param rows number or rows to return to as default for next plots to come or
#' for single plots
#' @param cols number or cols to return to as default for next plots to come or
#' for single plots
#' @param fixdims fix the dimensions at maxrows by maxcols or resize based on
#' number of elements in \code{yr} input.
#' @param main title of plot
#' @param cex.main character expansion for title
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param horiz_lab axis labels set horizontal all the time (TRUE), never
#' (FALSE) or only when relatively short ("default")
#' @param xbuffer extra space around points on the left and right as fraction
#' of total width of plot
#' @param ybuffer extra space around points on the bottom and top as fraction
#' of total height of plot. "default" will cause c(0,.15) for sexratio.option=1
#' and c(.15, .3) for sexratio.option=2.
#' @param yupper upper limit on ymax (applied before addition of ybuffer)
#' @param datonly make plots of data without fits?
#' @param showsampsize add sample sizes to plot
#' @param showeffN add effective sample sizes to plot
#' @param axis1 position of bottom axis values
#' @param axis2 position of left size axis values
#' @param ptscex character expansion factor for points (default=1)
#' @param ptscol color for points/bars
#' @param linescol color for fitted model
#' @param lty line type
#' @param lwd line width
#' @param nlegends number of lines of text to add as legends in each plot
#' @param legtext text in legend, a list of length=nlegends. values may be any
#' of 1.  "yr", 2. "sampsize", 3. "effN", or a vector of length = ptsx.
#' @param legx vector of length=nlegends of x-values of legends (default is
#' first one on left, all after on right)
#' @param legy vector of length=nlegends of y-values of legends (default is top
#' for all plots)
#' @param legadjx left/right adjustment of legends around legx
#' @param legadjy left/right adjustment of legends around legy
#' @param legsize font size for legends. default=c(1.2,1.0) (larger for year
#' and normal for others)
#' @param legfont font type for legends, same as "font" under ?par
#' @param ipage which page of plots when covering more than will fit within
#' maxrows by maxcols.
#' @param multifig_oma vector of outer margins. Can be input to SS_plots and will be
#' passed to this function via the ... argument.
#' @param \dots additional arguments (NOT YET IMPLEMENTED).
#' @author Cole Monnahan. Adapted from \code{\link{make_multifig}}.
#' @export
#' @details The SE of the sex ratio is crude and calculated as
#' follows. First, assume a multinomial which as MLEs of proportions. Then
#' use the delta method of the ratio F/M, using the MLE as the expected
#' values and analytical variances and covariance between F and M. After
#' some algebra this calculation reduces to: SE(F/M)= sqrt((f/m)^2*(
#' (1-f)/(f*N) + (1-m)/(m*N) +2/N )). Confidence intervals created from
#' these should be considered very crude and would not necessarily be
#' appropriate for future alternative compositional likelihoods.
#'
#' This function was derived from make_multifig and hence has a lot of
#' overlap in functionality and arguments.
#' @seealso \code{\link{SS_plots}},\code{\link{SSplotSexRatio}}
make_multifig_sexratio <-
  function(dbase, sexratio.option=2, CI=0.75,
           sampsizeround=1, maxrows=6, maxcols=6, 
           rows=1, cols=1, fixdims=TRUE, main="",cex.main=1,
           xlab="", ylab="Fraction female", horiz_lab="default", xbuffer=c(.1,.1),
           ybuffer="default", yupper=NULL,
           datonly = FALSE,
           showsampsize = TRUE, showeffN = TRUE,
           axis1=NULL,
           axis2=NULL, ptscex=1,
           ptscol=gray(.5), linescol=4, lty=1, lwd=2, nlegends=3,
           legtext=list("yr","sampsize","effN"),
           legx="default",legy="default",
           legadjx="default", legadjy="default", legsize=c(1.2,1.0),
           legfont=c(2,1), ipage=0, multifig_oma=c(5,5,5,2)+.1, ...)
{
  if(sexratio.option == 1 & ylab == "Fraction female"){
    # change the default ylab if the user hasn't replaced it
    ylab <- "Female:Male Ratio"
  }
  ## define dimensions
  yrvec <- sort(unique(dbase$Yr))
  npanels <- length(yrvec)
  nrows <- min(ceiling(sqrt(npanels)), maxrows)
  ncols <- min(ceiling(npanels/nrows), maxcols)
  if(fixdims){
    nrows <- maxrows
    ncols <- maxcols
  }
  npages <- ceiling(npanels/nrows/ncols) # how many pages of plots

  ## Build data frame containing the key data for plotting. This data frame
  ## is calculated ahead of time and then used for determining ranges and
  ## plotting below. Many bins are empty in the data file and thus in the
  ## report file they are ommitted so the division isn't defined.
  df.list <- list();k <- 1
  # minimum observation is likely to be the additive constant required
  # by the multinomial and can be subtracted from all obs and exp values
  minobs <- min(dbase$Obs, na.rm=TRUE)
  
  for(yr.temp in yrvec){
    for(bin in unique(dbase$Bin)){
      female <- dbase[dbase$Sex==1 & dbase$Bin==bin & dbase$Yr==yr.temp,]
      male <- dbase[dbase$Sex==2 & dbase$Bin==bin &dbase$Yr==yr.temp,]
      nm <- nrow(male); nf <- nrow(female)
      ## Four cases depending on which data were observed. If only one sex
      ## was observed, do some special things. If none, we'll skip it
      ## below. Otherwise do the calculations.
      se.ratio <- NA
      lwr <- NA
      upr <- NA
      if(nm == 1 & nf == 0){
        effN <- male$effN; Nsamp_adj <- male$Nsamp_adj; e <- NA; o <- 0
      } else if(nm == 0 & nf == 1) {
        effN <- female$effN; Nsamp_adj <- female$Nsamp_adj; e <- NA; o <- Inf
      } else if(nrow(female)==1 & nrow(male)==1){
        ## Calculate the ratio if data exists for both. Use delta method for
        ## multinomial estimators to get approximate SE for the ratio of the
        ## two (assuming expected value of estimator is the MLE). See Casella
        ## and Berger p245.  SS should divide the observations by n but redo
        ## just to be sure.

        # sample size is shared across both vectors (at least if Sexes==3)
        # IGT 2019-05-02: should check for case where sexes are input separately
        effN <- female$effN
        Nsamp_adj <- female$Nsamp_adj
        if(sexratio.option == 1){ # females:males
          e <- (female$Exp - minobs)/(male$Exp - minobs)
          o <- (female$Obs - minobs)/(male$Obs - minobs)
        }
        if(sexratio.option == 2){ # female:total
          e <- (female$Exp - minobs)/(female$Exp + male$Exp - 2*minobs)
          o <- (female$Obs - minobs)/(female$Obs + male$Obs - 2*minobs)
        }
        # need rounding to avoid differences like -2.122513e-17
        pf <- female$Obs
        pm <- male$Obs
        e <- round(e, 10)
        o <- round(o, 10)

        # calculate SE of the ratio
        if(sexratio.option == 1){ # females:males
          se.ratio <-
            sqrt((pf/pm)^2*( (1-pf)/(pf*Nsamp_adj) + (1-pm)/(pm*Nsamp_adj) +2/Nsamp_adj ))
          lwr <- qnorm((1 - CI)/2, o, se.ratio)
          upr <- qnorm(1 - (1 - CI)/2, o, se.ratio)
        }
        if(sexratio.option == 2){ # female:total
          pt <- female$Obs + male$Obs - 2*minobs
          # assuming sample size for this bin is at least 1
          Nbin <- max(pt*Nsamp_adj, 1)
          if(pt > 0){
            # Jeffreys interval
            # https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Jeffreys_interval
            # beta approximation to binomial quantiles used to get
            # confidence interval for non-integer values
            lwr <- qbeta(p = (1 - CI)/2,
                         shape1 = o*Nbin + 0.5,
                         shape2 = Nbin - o*Nbin + 0.5)
            upr <- qbeta(p = 1 - (1 - CI)/2,
                         shape1 = o*Nbin + 0.5,
                         shape2 = Nbin - o*Nbin + 0.5)
            # replacing bounds for 0% or 100% values as described in the Wikipedia
            # article above
            if(o == 1){
              upr <- 1
            }
            if(o == 0){
              lwr <- 0
            }
          }else{
            lwr <- NA
            upr <- NA
          }
        }
        # remove points that have 0 observations of either sex
        lwr[pf == 0 & pm == 0] <- NA
        upr[pf == 0 & pm == 0] <- NA
      } else {
        o <- e <- se.ratio <- lwr <- upr <- effN <- Nsamp_adj <- NA
      }
      df.list[[k]] <- data.frame(Yr=yr.temp, Bin=bin, Exp=e, Obs=o,
                                 lwr=lwr, upr=upr, effN=effN, Nsamp_adj=Nsamp_adj)
      k <- k+1
    }
  }

  df <- do.call(rbind, df.list)

  ## Calculate ranges of plots
  xrange <- range(df$Bin, na.rm=TRUE)
  if(sexratio.option == 1){ # females:males
    if(nrow(df[!is.na(df$se.ratio),])==0){
      warning(paste("No SE of ratio found, defaulting to ymax of 4"))
      yrange <- c(0,4)
    } else {
      yrange <- c(0, max(c(df$Exp, df$Obs+0*df$se.ratio), na.rm=TRUE))
    }
    if(!is.null(yupper)) yrange <- c(0,yupper)
  }
  if(sexratio.option == 2){ # females:total
    yrange <- c(0, 1)
  }
  if(ybuffer[1] == "default"){
    if(sexratio.option == 1){
      ybuffer <- c(0, 0.15)
    }
    if(sexratio.option == 2){
      ybuffer <- c(0.15, 0.4)
    }
  }
  xrange_big <- xrange+c(-1,1)*xbuffer*diff(xrange)
  yrange_big <- yrange+c(-1,1)*ybuffer*diff(yrange)
  ## I replaced <number>/0 above with Inf so replace those and any points
  ## outside the region to be "x" on the top of the plot
  if(sexratio.option == 1){ # females:males
    which.toobig <- which(df$Obs > yrange_big[2])
    which.toosmall <- which(df$Obs==0)
    df$Obs[which.toobig] <- yrange_big[2]
    ## Use different pch for those that are truncated to the plot
    df$pch2 <- 16
    df$pch2[which.toobig] <- 4
    df$pch2[which.toosmall] <- 4
  }
  if(sexratio.option == 2){ # females:total
    df$pch2 <- 16
  }

  ## get axis labels
  yaxs_lab <- pretty(yrange)
  maxchar <- max(nchar(yaxs_lab))
  ## should y-axis label be horizontal?
  if(horiz_lab=="default") horiz_lab <- maxchar<6
  if(is.null(axis1)) axis1 <- pretty(xrange)
  if(is.null(axis2)) axis2 <- pretty(yrange)

  ## create multifigure layout, set inner margins all to 0 and add outer margins
  ## old graphics parameter settings
  par_old <- par()
  on.exit( par(mfcol=par_old$mfcol, mar=par_old$mar, oma=par_old$oma) )
  ## new settings
  par(mfcol=c(nrows,ncols),mar=rep(0,4),oma=multifig_oma)
  panelrange <- 1:npanels
  if(npages > 1 & ipage!=0){
    panelrange <- intersect(panelrange, 1:(nrows*ncols) + nrows*ncols*(ipage-1))
  }
  ## Loop through each panel (year) and make plot
  for(ipanel in panelrange){
    yr_i <- yrvec[ipanel]
    plot(0,type="n", axes=FALSE, xlab="",ylab="", xlim=xrange_big,
         ylim=yrange_big, xaxs="i", yaxs='i')
    if(sexratio.option == 1){ # females:males
      abline(h=1,col="grey") # grey line at 0
    }
    if(sexratio.option == 2){ # females:total
      abline(h=0.5,col="grey") # grey line at 0.5
      abline(h=c(0,1),col="grey", lty=3) # grey line at 0.5
    }
    df2 <- na.omit(df[df$Yr == yr_i,])
    df2 <- df2[order(df2$Bin),]
    points(x=df2$Bin, y=df2$Obs ,ylim=yrange_big, xlim=xrange_big, pch=df2$pch2,
           col=ptscol, cex=ptscex)
    segments(x0=df2$Bin, y0=df2$lwr, y1=df2$upr, col=ptscol)

    # add model expectation (unless data-only plot is requested)
    if(!datonly){
      lines(df2$Bin, y=df2$Exp, lwd=lwd, lty=lty, col=linescol)
    }
    
    ## add legends
    usr <- par("usr") # get dimensions of panel
    for(i in 1:nlegends){
      legtext_i <- legtext[[i]] # grab element of list
      if(legtext_i=="Yr"){
        text_i <- yr_i
      } else if(legtext_i=="effN" & nrow(df2)>0 & showeffN){
        ## all rows should be same so grab first
        text_i <- paste0('effN=', round(df2$effN[1], sampsizeround))
      } else if(legtext_i == "N" & nrow(df2)>0 & showsampsize){
        # all rows should be same so grab first
        text_i <- paste0('N=', round(df2$Nsamp_adj[1], sampsizeround)) 
      } else {
        text_i <- ''
      }
      ## location of legend
      if(legx[1]=="default"){
        ## default is left side for first plot, right thereafter
        textx <- ifelse(i==1, usr[1], usr[2])
      } else {
        textx <- legx[i]
      }
      if(legy[1]=="default"){
        texty  <- usr[4] # default is top for all plots
        texty2 <- usr[3] # default is bottom legends associated with males
      } else {
        texty  <- legy[i]
        texty2 <- -legy[i] # this setting probably won't work too well
      }
      if(legadjx[1]=="default"){
        ## default x-value is left side for first legend, right thereafter
        adjx <- ifelse(i==1, -.1, 1.0)
      } else {
        adjx <- legadjx[i]
      }
      if(legadjy[1]=="default"){
        ## default y-value is top for first 2 legends, below thereafter
        adjy <- ifelse(i<3, 1.3, 1.3 + 1.3*(i-2))
      } else {
        adjy <- legadjy[i]
      }
      ## add legend text
      text(x=textx, y=texty, labels=text_i, adj=c(adjx, adjy),
           cex=legsize[i], font=legfont[i])
    } # end of legend loop
    ## add axes in left and lower outer margins
    mfg <- par("mfg")
    ## axis on bottom panels and final panel
    if(mfg[1]==mfg[3] | ipanel==npanels) axis(side=1,at=axis1)
    if(mfg[2]==1){
      ## axis on left side panels
      axis(side=2,at=axis2,las=horiz_lab)
    }
    box() # add box around panel
    ## if this is the first panel of a given page, then do a few things
    if(npanels==1 | ipanel %% (nrows*ncols) == 1){
      ## add title after plotting first panel on each page of panels
      fixcex <- 1 # compensates for automatic adjustment caused by par(mfcol)
      if(max(nrows,ncols)==2) fixcex <- 1/0.83
      if(max(nrows,ncols)>2) fixcex <- 1/0.66
      if(npanels>1){
        title(main=main, line=c(2,0,3,3), outer=TRUE, cex.main=cex.main*fixcex)
        title(xlab=xlab, outer=TRUE, cex.lab=fixcex)
        title(ylab=ylab, line=ifelse(horiz_lab,max(3,2+.4*maxchar),3.5),
              outer=TRUE, cex.lab=fixcex)
      } else {
        title(main=main, xlab=xlab, ylab=ylab, outer=TRUE, cex.main=cex.main)
      }
    } # end of first panel checks
  } # end of loop through years
  ## return information on what was plotted
  return(list(npages=npages, npanels=npanels, ipage=ipage))
} # end of function
