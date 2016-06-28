#' Create multi-figure sex ratio plots.
#'
#' Modified version of \code{\link{make_multifig}} for multi-figure
#' plots of sex ratio data with crude confidence intervals (+/i 1 se) and
#' fits from Stock Synthesis output.
#'
#' @param dbase eleemnt of list created by \code{\link{SS_output}} passed from
#' \code{\link{SSplotSexRatio}}
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
#' of total height of plot
#' @param yupper upper limit on ymax (applied before addition of ybuffer)
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
  function(dbase,
           sampsizeround=1, maxrows=6, maxcols=6,
           rows=1, cols=1, fixdims=TRUE, main="",cex.main=1,
           xlab="", ylab="Female:Male Ratio", horiz_lab="default", xbuffer=c(.1,.1),
           ybuffer=c(0,0.15), yupper=NULL, axis1=NULL,
           axis2=NULL, ptscex=1,
           ptscol=gray(.5), linescol=1, lty=1, lwd=2, nlegends=3,
           legtext=list("yr","sampsize","effN"),
           legx="default",legy="default",
           legadjx="default", legadjy="default", legsize=c(1.2,1.0),
           legfont=c(2,1), ipage=0, multifig_oma=c(5,5,5,2)+.1, ...)
{
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
  for(yr.temp in yrvec){
    for(bin in unique(dbase$Bin)){
      female <- dbase[dbase$Gender==1 & dbase$Bin==bin & dbase$Yr==yr.temp,]
      male <- dbase[dbase$Gender==2 & dbase$Bin==bin &dbase$Yr==yr.temp,]
      nm <- nrow(male); nf <- nrow(female)
      ## Four cases depending on which data were observed. If only one sex
      ## was observed, do some special things. If none, we'll skip it
      ## below. Otherwise do the calculations.
      se.ratio <- NA
      if(nm == 1 & nf == 0){
        effN <- male$effN; N <- male$N; e <- NA; o <- 0
      } else if(nm == 0 & nf == 1) {
        effN <- female$effN; N <- female$N; e <- NA; o <- Inf
      } else if(nrow(female)==1 & nrow(male)==1){
        ## Calculate the ratio if data exists for both. Use delta method for
        ## multinomial estimators to get approximate SE for the ratio of the
        ## two (assuming expcted value of estimator is the MLE). See Casella
        ## and Berger p245.  SS should divide the observations by n but redo
        ## just to be sure.
        effN <- female$effN; N <- female$N;
        e <- female$Exp/male$Exp
        o <- female$Obs/male$Obs
        pf <- female$Obs
        pm <- male$Obs
        se.ratio <-
          sqrt((pf/pm)^2*( (1-pf)/(pf*N) + (1-pm)/(pm*N) +2/N ))
      } else {
        o <- e <- se.ratio <- NA
      }
      ##  print(c(yr.temp, bin, e, o, se.ratio, effN, N))
      ## if(yr.temp==1997 & bin==5) browser()
      df.list[[k]] <- data.frame(Yr=yr.temp, Bin=bin, Exp=e, Obs=o,
                                 se.ratio=se.ratio, effN=effN, N=N)
      k <- k+1
    }
  }
  df <- do.call(rbind, df.list)
  # "df$" shouldn't be required in within function but added to make package check
  # warning go away. Feel free to change if there's a better solution.
  # Warning was "make_multifig_sexratio: no visible binding for global variable 'Obs'"
  df <- within(df, {
               lwr <- df$Obs - 1*df$se.ratio
               upr <- df$Obs + 1*df$se.ratio}) 
  ## Calculate ranges of plots
  xrange <- range(df$Bin, na.rm=TRUE)
  if(nrow(df[!is.na(df$se.ratio),])==0){
    warning(paste("No SE of ratio found, defaulting to ymax of 4"))
    yrange <- c(0,4)
  } else {
    yrange <- c(0, max(c(df$Exp, df$Obs+0*df$se.ratio), na.rm=TRUE))
  }
  if(!is.null(yupper)) yrange <- c(0,yupper)
  xrange_big <- xrange+c(-1,1)*xbuffer*diff(xrange)
  yrange_big <- yrange+c(-1,1)*ybuffer*diff(yrange)
  ## I replaced <number>/0 above with Inf so replace those and any points
  ## outside the region to be "x" on the top of the plot
  which.toobig <- which(df$Obs > yrange_big[2])
  which.toosmall <- which(df$Obs==0)
  df$Obs[which.toobig] <- yrange_big[2]
  ## Use different pch for those that are truncated to the plot
  df$pch2 <- rep(16, nrow(df))
  df$pch2[which.toobig] <- 4
  df$pch2[which.toosmall] <- 4
  ## browser()
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
    abline(h=1,col="grey") # grey line at 0
    df2 <- na.omit(df[df$Yr == yr_i,])
    df2 <- df2[order(df2$Bin),]
    points(x=df2$Bin, y=df2$Obs ,ylim=yrange_big, xlim=xrange_big, pch=df2$pch2,
           col=ptscol, cex=ptscex)
    segments(x0=df2$Bin, y0=df2$lwr, y1=df2$upr, col=ptscol)
    lines(df2$Bin, y=df2$Exp, lwd=lwd, lty=lty, col=linescol)
    ## add legends
    usr <- par("usr") # get dimensions of panel
    for(i in 1:nlegends){
      legtext_i <- legtext[[i]] # grab element of list
      if(legtext_i=="yr"){
        text_i <- yr_i
      } else if(legtext_i=="effN" & nrow(df2)>0){
        ## all rows should be same so grab first
        text_i <- paste0('effN=', round(df2$effN[1], sampsizeround))
      } else if(legtext_i == "N" & nrow(df2)>0){
        text_i <- paste0('N=', round(df2$N[1], sampsizeround)) # all rows should be same so grab first
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
