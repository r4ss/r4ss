#' Create a bubble plot.
#' 
#' Bubble plot based on function vaguely based on \code{bubble} by Edzer
#' Pebesma in gstat package. By default, positive values have closed bubbles
#' and negative values have open bubbles.
#' 
#' 
#' @param x Vector of x-values.
#' @param y Vector of y-values.
#' @param z Vector of bubble sizes, where positive sizes will be plotted as
#' closed bubbles and negative as open unless \code{allopen==TRUE}.
#' @param col Color for bubbles.
#' @param cexZ1 Character expansion (cex) value for a proportion of 1.0.
#' @param maxsize Size of largest bubble. Prefered option is now an expansion
#' factor for a bubble with z=1 (see \code{cexZ1} above).
#' @param do.sqrt Should size be based on the area? (Diameter proportional to
#' sqrt(z)). Default=TRUE.
#' @param bg.open background color for open bubbles (border will equal 'col')
#' @param legend Add a legend to the plot?
#' @param legendloc Location for legend (default='top')
#' @param legend.z If a legend is added, what z values will be shown. Default
#' is c(-3,-2,-1,.1,1,2,3) for Pearson-like quantities and a smaller range for
#' proportions that are all less than 1.
#' @param legend.yadj If a legend is added, how much should the y-axis be
#' expanded to make space for it.
#' @param main Title of plot. Default="".
#' @param cex.main Charecter expansion for title. Default=1.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param minnbubble Minimum number of unique x values below which extra space
#' is added to horizontal axis (to make plot look better). Default = 8.
#' @param xlim Optional limits on x-range.
#' @param ylim Optional limits on y-range.
#' @param axis1 Show the horizontal axis on plot? Option allows turning off for
#' use in multi-figure plots.
#' @param xlimextra Extra space (see minnbubble above). Default = 1.
#' @param add Add bubbles to existing plot? Default=FALSE.
#' @param las Style of axis labels (see ?par for more info).
#' @param allopen Should all bubbles be open (instead of just negative values)?
#' @author Ian Stewart and Ian Taylor
#' @keywords aplot hplot
bubble3 <- function (x,y,z,col=1,cexZ1=5,maxsize=NULL,do.sqrt=TRUE,
                     bg.open=gray(0.95,0.3),
                     legend=TRUE,legendloc='top',
                     legend.z="default",legend.yadj=1.1,
                     main="",cex.main=1,xlab="",ylab="",minnbubble=3,
                     xlim=NULL,ylim=NULL,axis1=TRUE,xlimextra=1,
                     add=FALSE,las=1,allopen=TRUE)
  {
    # This function is vaguely based on bubble() from gstat.
    # Not sure anymore what happened to bubble2.
    if(diff(range(length(x),length(y),length(z)))>0)
      stop("x, y, and z should all be equal in length")
    # filter NA values
    x <- x[!is.na(z)]
    y <- y[!is.na(z)]
    z <- z[!is.na(z)]

    n <- length(x)
    if(n==0) return()

    az <- abs(z)
    if(legend.z[1]=="default"){
      # set sequence of points to use in legend
      maxaz <- max(az,na.rm=TRUE)
      if(maxaz>1)  legend.z <- c(.1,1:3) # something like Pearsons
      if(maxaz>5)  legend.z <- c(.1,pretty(c(1,maxaz),n=2)) # big Pearsons
      if(maxaz>10) legend.z <- pretty(c(0,maxaz)) # something like numbers
      if(maxaz<=1) legend.z <- c(0.01,.1*pretty(c(1,10*maxaz),n=2)) # something like proportions
      # exclude zero
      legend.z <- setdiff(legend.z,0)
      # if legend is too long, cut in half
      if(length(legend.z)>3) legend.z <- legend.z[seq(1,length(legend.z),2)]
      # add negatives
      if(any(z<0)){
        legend.z <- c(-rev(legend.z[-1]),legend.z)
      }
    }
    legend.n <- length(legend.z)
    legend.z2 <- legend.z

    # scale by square root if desired
    if (do.sqrt){
      legend.z2 <- sqrt(abs(legend.z))
      az <- sqrt(az)
    }

    # set scale
    if(!is.null(maxsize)) cexZ1 <- maxsize/max(az)
    
    cex <- cexZ1*az
    legend.cex <- cexZ1*legend.z2
    
    # if xlim is not specified, then set to the range, or range plus padding
    if(is.null(xlim)){
      xlim <- range(x)
      if(length(unique(x))<minnbubble) xlim=xlim+c(-1,1)*xlimextra
    }
    #### old way using plot character to control open/closed
    ## # set plot character
    ## pch <- rep(NA,n)
    ## pch[z>0] <- 16
    ## pch[z<0] <- 1
    ## legend.pch <- rep(NA,legend.n)
    ## legend.pch[legend.z>0] <- 16
    ## legend.pch[legend.z<0] <- 1

    ## # if only open points to be shown
    ## if(allopen){
    ##   legend.z <- legend.z[legend.z>0]
    ##   legend.pch <- 1
    ##   pch[!is.na(pch)] <- 1
    ## }

    #### new way using background color
    # set plot character
    pch <- rep(21,n)
    pch[is.na(z) | z==0] <- NA
    # set background color equal to open color for all points 
    bg <- rep(bg.open, n)
    if(!allopen){
      # replace background color with foreground color for closed points
      # (if not all open)
      bg[z>0] <- col[z>0]
    }
    legend.pch <- rep(21, legend.n)
    legend.bg <- rep(bg.open, legend.n)
    legend.bg[legend.z>0] <- col[1] # error occured when full vector was used

    # if only open points to be shown
    if(allopen){
      legend.z <- legend.z[legend.z>0]
      legend.bg <- bg.open
      legend.pch <- 21
      #pch[!is.na(pch)] <- 1
    }

    # make empty plot (if needed)
    if(!add){
      if(is.null(ylim)) ylim <- range(y)
      ylim[2] <- legend.yadj*ylim[2]
      plot(x,y,type="n",xlim=xlim,ylim=ylim,main=main,cex.main=cex.main,
           xlab=xlab,ylab=ylab,axes=FALSE)
      xvec <- unique(x)
      if(axis1) axis(1,at=floor(unique(x))) # only printing integer values for years
      axis2vals <- sort(unique(y))
      if(length(axis2vals)>20) axis2vals <- pretty(axis2vals)
      axis(2,at=axis2vals,las=las)
      box()
    }
    # add points
    points(x, y, pch=pch, cex=cex, col=col, bg=bg)
    # do things for legend
    if(legend & all(par()$mfg[1:2]==1)){
      # set labels
      legend.lab <- format(legend.z, scientific=FALSE,drop0trailing=TRUE)
      # add legend
      legend(x=legendloc,legend=legend.lab,pch=legend.pch,col=col,pt.bg=legend.bg,
             pt.cex=legend.cex,ncol=legend.n,bty='n')
      ## next line for debugging legends
      # print(data.frame(legendloc,legend.z,legend.pch,col,legend.cex,legend.n))
    }
  }
