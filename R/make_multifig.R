#' Create multi-figure plots.
#'
#' Function created as an alternative to lattice package for multi-figure plots
#' of composition data and fits from Stock Synthesis output.
#'
#'
#' @param ptsx vector of x values for points or bars
#' @param ptsy vector of y values for points or bars of same length as ptsx
#' @param yr vector of category values (years) of same length as ptsx
#' @param linesx optional vector of x values for lines
#' @param linesy optional vector of y values for lines
#' @param ptsSD optional vector of standard deviations used to plot error bars
#' on top of each point under the assumption of normally distributed error
#' @param sampsize optional sample size vector of same length as ptsx
#' @param effN optional effective sample size vector of same length as ptsx
#' @param showsampsize show sample size values on plot?
#' @param showeffN show effective sample size values on plot?
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
#' @param size vector of bubbles sizes if making a bubble plot
#' @param cexZ1 Character expansion (cex) for point associated with value of 1.
#' @param bublegend Add legend with example bubble sizes to bubble plots.
#' @param maxsize maximum size of bubbles
#' @param do.sqrt scale bubbles based on sqrt of size vector. see ?bubble3 for
#' more info.
#' @param minnbubble number of unique x values before adding buffer. see
#' ?bubble3 for more info.
#' @param allopen should all bubbles be open? see ?bubble3 for more info.
#' @param horiz_lab axis labels set horizontal all the time (TRUE), never
#' (FALSE) or only when relatively short ("default")
#' @param xbuffer extra space around points on the left and right as fraction
#' of total width of plot
#' @param ybuffer extra space around points on the bottom and top as fraction
#' of total height of plot
#' @param yupper upper limit on ymax (applied before addition of ybuffer)
#' @param ymin0 fix minimum y-value at 0?
#' @param axis1 position of bottom axis values
#' @param axis2 position of left size axis values
#' @param linepos should lines be added on top of points (linepos=1) or behind
#' (linepos=2)?
#' @param type type of line/points used for observed values (see 'type' in
#' ?plot for details) on top of a grey polygon. Default is "o" for overplotting
#' points on lines.
#' @param polygons should polygons be added to the
#' (turning off is required for sex-ratio plot)
#' @param bars should the ptsx/ptsy values be bars instead of points
#' (TRUE/FALSE) NOT CURRENTLY FUNCTIONAL
#' @param barwidth width of bars in barplot, default method chooses based on
#' quick and dirty formula also, current method of plot(...type='h') could be
#' replaced with better approach
#' @param ptscex character expansion factor for points (default=1)
#' @param ptscol color for points/bars
#' @param ptscol2 color for negative value points in bubble plots
#' @param colvec Vector of length 3 with colors for females, males, unsexed fish
#' @param linescol color for lines
#' @param lty line type
#' @param lwd line width
#' @param pch point character type
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
#' @param venusmars Label females and males with venus and mars symbols?
#' @param sampsizeline show line for input sample sizes on top of conditional
#' age-at-length plots (TRUE/FALSE/scalar, still in development)
#' @param effNline show line for effective sample sizes on top of conditional
#' age-at-length plots (TRUE/FALSE/scalar, still in development)
#' @param sampsizemean mean input sample size value (used when sampsizeline=TRUE)
#' @param effNmean mean effective sample size value (used when effNline=TRUE)
#' @param ipage which page of plots when covering more than will fit within
#' maxrows by maxcols.
#' @param scalebins Rescale expected and observed proportions by dividing by
#' bin width for models where bins have different widths? Caution!: May not
#' work correctly in all cases.
#' @param sexvec vector of sex codes if more than one present (otherwise NULL)
#' @param multifig_colpolygon vector of polygon fill colors of length 3
#' (for females, males, and unsexed fish). Can be input to SS_plots and will be
#' passed to this function via the ... argument.
#' @param multifig_oma vector of outer margins. Can be input to SS_plots and will be
#' passed to this function via the ... argument.
#' @param \dots additional arguments (NOT YET IMPLEMENTED).
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}},\code{\link{SSplotComps}}
make_multifig <-
  function(ptsx, ptsy, yr, linesx=0, linesy=0, ptsSD=0,
           sampsize=0, effN=0, showsampsize=TRUE, showeffN=TRUE, sampsizeround=1,
           maxrows=6, maxcols=6, rows=1, cols=1, fixdims=TRUE, main="",cex.main=1,
           xlab="",ylab="",size=1,cexZ1=1.5,bublegend=TRUE,
           maxsize=NULL,do.sqrt=TRUE,minnbubble=8,allopen=TRUE,
           horiz_lab="default",xbuffer=c(.1,.1),ybuffer=c(0,0.15),
           yupper=NULL, ymin0=TRUE,
           axis1=NULL,axis2=NULL,linepos=1,type="o",
           polygons=TRUE,
           bars=FALSE,barwidth="default",ptscex=1,ptscol=1,ptscol2=1,
           colvec=c(rgb(1,0,0,.7),rgb(0,0,1,.7),rgb(.1,.1,.1,.7)),
           linescol=c(rgb(0,.8,0,.7),rgb(1,0,0,.7),rgb(0,0,1,.7)),
           lty=1,lwd=2,pch=1,
           nlegends=3,legtext=list("yr","sampsize","effN"),legx="default",legy="default",
           legadjx="default",legadjy="default",legsize=c(1.2,1.0),legfont=c(2,1),
           venusmars=TRUE,
           sampsizeline=FALSE,effNline=FALSE,sampsizemean=NULL,effNmean=NULL,
           ipage=0,scalebins=FALSE,sexvec=NULL,
           multifig_colpolygon=c("grey60","grey80","grey70"),
           multifig_oma=c(5,5,5,2)+.1,...)
{
  # switch to determine whether to show males below 0 line in same plot
  twosex <- TRUE
  if(is.null(sexvec)){
    twosex <- FALSE
  }
  # if all observations are the same sex then don't waste space below 0 line
  if(length(unique(sexvec))==1){
    twosex <- FALSE
  }
  male_mult <- 1
  if(twosex){
    male_mult <- -1
  }

#print(paste("twosex: ",twosex)  )
  # define dimensions
  yrvec <- sort(unique(yr))
  npanels <- length(yrvec)
  nvals <- length(yr)

  nrows <- min(ceiling(sqrt(npanels)), maxrows)
  ncols <- min(ceiling(npanels/nrows), maxcols)

  if(fixdims){
    nrows <- maxrows
    ncols <- maxcols
  }

  npages <- ceiling(npanels/nrows/ncols) # how many pages of plots
  # doSD is TRUE/FALSE switch for whether to add error bars on points
  doSD <- length(ptsSD)==length(ptsx) & max(ptsSD) > 0
  # turn off polygons for any plots with uncertainty
  # such as mean length at age
  if(doSD){
    polygons <- FALSE
  }

  # if no input on lines, then turn linepos to 0
  if(length(linesx)==1 | length(linesy)==1){
    linepos <- 0
    linesx <- ptsx
    linesy <- ptsy
  }
  anyscaled <- FALSE

  # quick and dirty formula to get width of bars (if used) based on
  #	  number of columns and maximum number of bars within a in panel
  if(bars & barwidth=="default") barwidth <- 400/max(table(yr)+2)/ncols

  # make size vector have full length
  if(length(size)==1){
    size <- rep(size,length(yr))
  }
  # determinant on whether this is a bubble plot for
  # conditional age-at-length data
  bub <- diff(range(size,na.rm=TRUE))!=0

  # get axis limits
  xrange <- range(c(ptsx,linesx,ptsx,linesx))
  if(ymin0){
    yrange <- c(0,max(ptsy,linesy))
  }else{
    yrange <- range(c(ptsy,linesy,ptsy,linesy))
  }
  # reduce range to <= yupper (no impact if yupper=NULL)
  yrange <- c(min(yrange[1],yupper),
              min(yrange[2],yupper))

  xrange_big <- xrange+c(-1,1)*xbuffer*diff(xrange)
  yrange_big <- yrange+c(-1,1)*ybuffer*diff(yrange)
  if(twosex & !bub){
    yrange_big <- range(-yrange,yrange)+c(-1,1)*ybuffer*diff(yrange)
  }

  # get axis labels
  yaxs_lab <- pretty(yrange)
  maxchar <- max(nchar(yaxs_lab))
  if(horiz_lab=="default"){
    horiz_lab <- maxchar<6 # should y-axis label be horizontal?
  }
  if(is.null(axis1)){
    axis1 <- pretty(xrange)
  }
  if(is.null(axis2)){
    axis2 <- pretty(yrange)
  }
  if(length(sampsize)==1){
    sampsize <- 0
  }
  if(length(effN)==1){
    effN <- 0
  }

  # create multifigure layout, set inner margins all to 0 and add outer margins
  # old graphics parameter settings
  par_old <- par()
  # new settings
  par(mfcol=c(nrows,ncols),mar=rep(0,4),oma=multifig_oma)

  panelrange <- 1:npanels
  if(npages > 1 & ipage!=0){
    panelrange <- intersect(panelrange, 1:(nrows*ncols) + nrows*ncols*(ipage-1))
  }
  for(ipanel in panelrange){
    # subset values for a given year
    yr_i <- yrvec[ipanel]
    sexvec_i <- sexvec[yr==yr_i]
    # separate vectors for females and males shown in the same plot
    ptsx_i0 <- ptsx[yr==yr_i & sexvec==0]
    ptsx_i1 <- ptsx[yr==yr_i & sexvec==1]
    ptsx_i2 <- ptsx[yr==yr_i & sexvec==2]
    # again for y-values
    ptsy_i0 <- ptsy[yr==yr_i & sexvec==0]
    ptsy_i1 <- ptsy[yr==yr_i & sexvec==1]
    ptsy_i2 <- ptsy[yr==yr_i & sexvec==2]*male_mult

    #### not sure why this was previously needed, taking it out for now
    ## # change negative values to NA
    ## ptsy_i0[ptsy_i0 < 0] <- NA
    ## ptsy_i1[ptsy_i1 < 0] <- NA
    ## ptsy_i2[ptsy_i2 < 0] <- NA

    # standard deviations
    # for use in mean length or weight at age plots
    if(doSD){
      ptsSD_i0 <- ptsSD[yr==yr_i & sexvec==0]
      ptsSD_i1 <- ptsSD[yr==yr_i & sexvec==1]
      ptsSD_i2 <- ptsSD[yr==yr_i & sexvec==2]
    }
    # x-values for lines
    linesx_i0 <- linesx[yr==yr_i & sexvec==0]
    linesx_i1 <- linesx[yr==yr_i & sexvec==1]
    linesx_i2 <- linesx[yr==yr_i & sexvec==2]
    # y-values for lines
    linesy_i0 <- linesy[yr==yr_i & sexvec==0]
    linesy_i1 <- linesy[yr==yr_i & sexvec==1]
    linesy_i2 <- linesy[yr==yr_i & sexvec==2]*male_mult
    # sort based on order of x-values (perhaps not needed)
    linesy_i0 <- linesy_i0[order(linesx_i0)]
    linesx_i0 <- sort(linesx_i0)
    linesy_i1 <- linesy_i1[order(linesx_i1)]
    linesx_i1 <- sort(linesx_i1)
    linesy_i2 <- linesy_i2[order(linesx_i2)]
    linesx_i2 <- sort(linesx_i2)
    # subset size (z) values
    z_i0 <- size[yr==yr_i & sexvec==0]
    z_i1 <- size[yr==yr_i & sexvec==1]
    z_i2 <- size[yr==yr_i & sexvec==2]

    # optional rescaling of bins for line plots
    #!! (not yet applied to males in 2-sex plots)
    scaled <- FALSE
    if(scalebins){
      bins <- sort(unique(ptsx_i1))
      binwidths <- diff(bins)
      if(diff(range(binwidths))>0){
        warning("NOTE: scaling comps based on variable bin widths\n",
                "hasn't yet been adapted to 2-sex plots")
        if(FALSE){
          binwidths <- c(binwidths,tail(binwidths,1))
          allbinwidths <- apply(as.matrix(ptsx_i1),1,
                                function(x){(binwidths)[bins==x]})
          ptsy_i1   <- ptsy_i1/allbinwidths
          linesy_i1 <- linesy_i1/allbinwidths
          scaled <- TRUE
        }
      }
      if(scaled){
        # change y-axis label if comps are scaled
        anyscaled <- TRUE
        if(ylab=="Proportion"){
          ylab <- "Proportion / bin width"
        }
      }
    }

    # make plot
    plot(0,type="n", axes=FALSE, xlab="",ylab="", xlim=xrange_big,
         ylim=yrange_big, xaxs="i", yaxs=ifelse(bars,"i","r"))
    abline(h=0,col="grey") # grey line at 0
    if(linepos==2){ # add lines behind points
      lines(linesx_i0, linesy_i0, col=linescol[1], lwd=lwd, lty=lty)
      lines(linesx_i1, linesy_i1, col=linescol[2], lwd=lwd, lty=lty)
      lines(linesx_i2, linesy_i2, col=linescol[3], lwd=lwd, lty=lty)
    }
    if(bub){ # if size input is provided then use bubble function
      # bubble plot for unsexed fish
      if(length(z_i0)>0){
        bubble3(x=ptsx_i0, y=ptsy_i0, z=z_i0,
                col=rep(colvec[3], length(z_i0)),
                cexZ1=cexZ1, legend.yadj=1.5,
                legend=bublegend, legendloc='topright',
                maxsize=maxsize, minnbubble=minnbubble,
                allopen=allopen, add=TRUE)
      }
      # bubble plot for females fish
      if(length(z_i1)>0){
        bubble3(x=ptsx_i1, y=ptsy_i1, z=z_i1,
                col=rep(colvec[1], length(z_i1)),
                cexZ1=cexZ1, legend.yadj=1.5,
                legend=bublegend, legendloc='topright',
                maxsize=maxsize, minnbubble=minnbubble,
                allopen=allopen, add=TRUE)
      }
      # bubble plot for males fish
      if(length(z_i2)>0){
        # note: ptsy_i2 may be negative for other plots, so taking
        #       absolute values for conditional age-at-length bubble plots
        bubble3(x=ptsx_i2, y=abs(ptsy_i2), z=z_i2,
                col=rep(colvec[2], length(z_i2)),
                cexZ1=cexZ1, legend.yadj=1.5,
                legend=bublegend, legendloc='topright',
                maxsize=maxsize, minnbubble=minnbubble,
                allopen=allopen, add=TRUE)
      }
      # add optional lines to bubble plots showing
      # (adjusted) input sample size
      # IAN T.: these need to be generalized to deal
      #         with different sexes
      if(linepos==0) effNline <- 0
      if(effNline>0 && length(effN)>0){
        effN_i1         <- effN[yr==yr_i]
        effN_i1_vec     <- unlist(lapply(split(effN_i1,ptsy_i1),unique))
        ptsy_i1_vec     <- sort(unique(ptsy_i1))
        lines(effNline*effN_i1_vec,ptsy_i1_vec,col='green3')
        if(!is.null(effNmean)){
          lines(rep(effNline*effNmean, length(ptsy_i1_vec)),
                ptsy_i1_vec, col='green3', lty=2)
        }
      }
      # add optional lines showing effective sample size
      if(sampsizeline>0 && length(sampsize)>0){
        sampsize_i1     <- sampsize[yr==yr_i]
        sampsize_i1_vec <- unlist(lapply(split(sampsize_i1,ptsy_i1),unique))
        ptsy_i1_vec     <- sort(unique(ptsy_i1))

        lines(sampsizeline*sampsize_i1_vec,ptsy_i1_vec,col=2)
        if(!is.null(sampsizemean)){
          lines(rep(sampsizeline*sampsizemean, length(ptsy_i1_vec)),
                ptsy_i1_vec, col=2, lty=3)
        }
      }
    }else{
      # make polygons (unless turned off) and points
      # make polygons
      if(length(ptsx_i0)>0){
        # polygon for unsexed fish
        if(polygons){
          polygon(c(ptsx_i0[1], ptsx_i0, tail(ptsx_i0, 1)), c(0, ptsy_i0, 0),
                  col=multifig_colpolygon[3])
        }
        # line with solid points on top for unsexed fish
        points(ptsx_i0, ptsy_i0, type=type, lwd=1, pch=16, cex=0.7, col=ptscol)
      }
      if(length(ptsx_i1)>0){
        # polygon for females
        if(polygons){
          polygon(c(ptsx_i1[1], ptsx_i1, tail(ptsx_i1, 1)), c(0, ptsy_i1, 0),
                  col=multifig_colpolygon[1])
        }
        # lines with solid points on top for females
        points(ptsx_i1, ptsy_i1, type=type, lwd=1, pch=16, cex=0.7, col=ptscol)
      }
      if(length(ptsx_i2)>0){
        # polygon for males (possibly below 0 line
        if(polygons){
          polygon(c(ptsx_i2[1], ptsx_i2, tail(ptsx_i2, 1)), c(0, ptsy_i2, 0),
                  col=multifig_colpolygon[2])  # polygon
        }
        # lines with solid points on top for males
        points(ptsx_i2, ptsy_i2, type=type, lwd=1, pch=16, cex=0.7, col=ptscol)
      }

      # adding uncertainty for mean length or weight at age plots
      if(doSD){
        old_warn <- options()$warn   # previous settings for warnings
        options(warn=-1)             # turn off "zero-length arrow" warning
        # make arrows showing uncertainty for unsexed fish
        if(length(ptsx_i0)>0){
          arrows(x0=ptsx_i0,y0=qnorm(p=0.05,mean=ptsy_i0,sd=ptsSD_i0),
                 x1=ptsx_i0,y1=qnorm(p=0.95,mean=ptsy_i0,sd=ptsSD_i0),
                 length=0.01, angle=90, code=3, col=ptscol)
        }
        # make arrows showing uncertainty for females
        if(length(ptsx_i1)>0){
          arrows(x0=ptsx_i1,y0=qnorm(p=0.05,mean=ptsy_i1,sd=ptsSD_i1),
                 x1=ptsx_i1,y1=qnorm(p=0.95,mean=ptsy_i1,sd=ptsSD_i1),
                 length=0.01, angle=90, code=3, col=ptscol)
        }
        # make arrows showing uncertainty for males
        if(length(ptsx_i2)>0){
          arrows(x0=ptsx_i2,y0=-qnorm(p=0.05,mean=ptsy_i2,sd=ptsSD_i2),
                 x1=ptsx_i2,y1=-qnorm(p=0.95,mean=ptsy_i2,sd=ptsSD_i2),
                 length=0.01, angle=90, code=3, col=ptscol)
        }
        options(warn=old_warn)  #returning to old value
      }
    }
    if(linepos==1){ # add lines on top of points
      lines(linesx_i0, linesy_i0, col=linescol[1], lwd=lwd, lty=lty)
      lines(linesx_i1, linesy_i1, col=linescol[2], lwd=lwd, lty=lty)
      lines(linesx_i2, linesy_i2, col=linescol[3], lwd=lwd, lty=lty)
    }

    # add legends
    usr <- par("usr") # get dimensions of panel
    for(i in 1:nlegends){
      text_i <- ""
      text_i2 <- ""
      legtext_i <- legtext[[i]] # grab element of list
      # elements of list can be "default" to make equal to yr
      # or vector of length 1, npanels, or the full length of the input vectors
      if(length(legtext_i)==1){
        if(legtext_i=="yr"){ text_i <- yr_i }	 # values in "yr" input
        for(sex in sort(unique(sexvec_i))){
          if(legtext_i=="sampsize" & showsampsize){	      # sample sizes
            vals <- unique(sampsize[sexvec==sex & yr==yr_i])
            if(length(vals)>1){
              warning("sampsize values are not all equal",
                      "--choosing the first value: ",vals[1], "\n",
                      "  yr=",yr_i,", and all sampsize values: ",
                      paste(vals,collapse=","),sep="")
              vals <- vals[1]
            }
            text_i <- paste("N=",round(vals,sampsizeround),sep="")
            if(twosex & sex==2){
              text_i2 <- paste("N=",round(vals,sampsizeround),sep="")
            }
          }
          if(legtext_i=="effN" & showeffN){          # effective sample sizes
            vals <- unique(effN[sexvec==sex & yr==yr_i])
            if(length(vals)>1){
              warning("effN values are not all equal",
                      "--choosing the first value: ",vals[1], "\n",
                      "  yr=",yr_i,", and all effN values: ",
                      paste(vals,collapse=","),sep="")
              vals <- vals[1]
            }
            text_i <- paste("effN=",round(vals,sampsizeround),sep="")
            if(twosex & sex==2){
              text_i2 <- paste("effN=",round(vals,sampsizeround),sep="")
            }
          }
        }
      }
      ## if(length(legtext_i)==npanels){
      ##   text_i <- legtext_i[ipanel]      # one input value per panel
      ## }
      if(length(legtext_i)==nvals){
        text_i <- legtext_i[yr==yr_i][1] # one input value per element
      }
      if(length(legtext_i)==1){
        text_i <- text_i		 # yr, sampsize, or effN
      }
      # location of legend
      if(legx[1]=="default"){
        # default is left side for first plot, right thereafter
        textx <- ifelse(i==1, usr[1], usr[2])
      }else{ textx <- legx[i] }
      if(legy[1]=="default"){
        texty  <- usr[4] # default is top for all plots
        texty2 <- usr[3] # default is bottom legends associated with males
      }else{
        texty  <- legy[i]
        texty2 <- -legy[i] # this setting probably won't work too well
      }
      if(legadjx[1]=="default"){
        # default x-value is left side for first legend, right thereafter
        adjx <- ifelse(i==1, -.1, 1.0)
      }else{
        adjx <- legadjx[i]
      }
      if(legadjy[1]=="default"){
        # default y-value is top for first 2 legends, below thereafter
        adjy <- ifelse(i<3, 1.3, 1.3 + 1.3*(i-2))
      }else{ adjy <- legadjy[i] }

      # add legend text
      text(x=textx, y=texty, labels=text_i, adj=c(adjx, adjy),
           cex=legsize[i], font=legfont[i])
      # add legend for males (if different from one already added)
      if(text_i2!=text_i & text_i2!=""){
        text(x=textx, y=texty2, labels=text_i, adj=c(adjx, -adjy),
             cex=legsize[i], font=legfont[i])
      }

      # add venus and mars symbols if there are male or female values
      if(twosex & !bub & venusmars){
        pu <- par('usr')
        xval <- pu[2]
        if(length(ptsx_i0)>0){
          text(xval, 0.5*yrange[2], "\\VE+\\MA", vfont=c("serif","plain"),
               cex=2, col=linescol[1],pos=2)
        }
        if(length(ptsx_i1)>0){
          text(xval, 0.5*yrange[2], "\\VE", vfont=c("serif","plain"),
               cex=2, col=linescol[2],pos=2)
        }
        if(length(ptsx_i2)>0){
          text(xval,-0.5*yrange[2], "\\MA", vfont=c("serif","plain"),
               cex=2, col=linescol[3],pos=2)
        }
      }
    }

    # add axes in left and lower outer margins
    mfg <- par("mfg")
    # axis on bottom panels and final panel
    if(mfg[1]==mfg[3] | ipanel==npanels) axis(side=1,at=axis1)
    if(mfg[2]==1){
      # axis on left side panels
      axis(side=2,at=axis2,las=horiz_lab)
      if(twosex){
        # axis for negative values on left side panels
        axis(side=2, at=-axis2[axis2>0], labels=format(axis2[axis2>0]),
             las=horiz_lab)
        ## # axis for negative values on left side panels
        ## axis(side=2,at=-axis2,las=horiz_lab)
      }
    }
    box() # add box around panel

    # if this is the first panel of a given page, then do a few things
    if(npanels==1 | ipanel %% (nrows*ncols) == 1){
      # add title after plotting first panel on each page of panels
      fixcex <- 1 # compensates for automatic adjustment caused by par(mfcol)
      if(max(nrows,ncols)==2){
        fixcex <- 1/0.83
      }
      if(max(nrows,ncols)>2){
        fixcex <- 1/0.66
      }
      if(npanels>1){
        title(main=main, line=c(2,0,3,3), outer=TRUE, cex.main=cex.main*fixcex)
        title(xlab=xlab, outer=TRUE, cex.lab=fixcex)
        title(ylab=ylab, line=ifelse(horiz_lab,max(3,2+.4*maxchar),3.5),
              outer=TRUE, cex.lab=fixcex)
      }else{
        title(main=main, xlab=xlab, ylab=ylab, outer=TRUE, cex.main=cex.main)
      }
    }
  }
  # restore previous graphics parameter settings
  par(mfcol=par_old$mfcol, mar=par_old$mar, oma=par_old$oma)
  #par(mfcol=c(rows,cols), mar=c(5,4,4,2)+.1, oma=rep(0,4))

  if(anyscaled){
    cat("Note: compositions have been rescaled by dividing by binwidth\n")
  }
  # return information on what was plotted
  return(list(npages=npages, npanels=npanels, ipage=ipage))
} # end embedded function: make_multifig
