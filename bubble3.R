bubble3 <- function (x,y,z,col=1,cexZ1=5,maxsize=NULL,do.sqrt=TRUE,
                     legend=TRUE,legend.z="default",legend.yadj=1.1,
                     main="",cex.main=1,xlab="",ylab="",minnbubble=8,
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
    if(legend && legend.z[1]=="default"){
      # set sequence of points to use in legend
      maxaz <- max(az,na.rm=TRUE)
      if(maxaz>1)  legend.z <- c(.1,1:3) # something like Pearsons
      if(maxaz>5)  legend.z <- c(.1,seq(1,maxaz,2)) # big Pearsons
      if(maxaz>10) legend.z <- pretty(c(0,maxaz)) # something like numbers
      if(maxaz<=1) legend.z <- c(0.01,0.1*(1:floor(10*maxaz))) # something like proportions
      if(any(z<0)) legend.z <- c(-rev(legend.z[-1]),legend.z) # add negatives
    }
    legend.n <- length(legend.z)
    legend.z2 <- legend.z
    if (do.sqrt){
      az <- sqrt(az)
      legend.z2 <- sqrt(abs(legend.z))
    }
    if(!is.null(maxsize)) cexZ1 <- maxsize/max(az)

    cex <- cexZ1*az
    legend.cex <- cexZ1*legend.z2
    
    # if xlim is not specified, then set to the range, or range plus padding
    if(is.null(xlim)){
      xlim <- range(x)
      if(length(unique(x))<minnbubble) xlim=xlim+c(-1,1)*xlimextra
    }
    pch <- rep(NA,n)
    pch[z>0] <- 16
    pch[z<0] <- 1
    legend.pch <- rep(NA,legend.n)
    legend.pch[legend.z>0] <- 16
    legend.pch[legend.z<0] <- 1
    if(allopen){
      pch[!is.na(pch)] <- 1
      legend.z <- legend.z[legend.z>0]
      legend.pch <- 1
    }
    if(!add){
      if(is.null(ylim)) ylim <- range(y)
      ylim[2] <- legend.yadj*ylim[2]
      plot(x,y,type="n",xlim=xlim,ylim=ylim,main=main,cex.main=cex.main,
           xlab=xlab,ylab=ylab,axes=FALSE)
      xvec <- unique(x)
      if(axis1) axis(1,at=floor(unique(x))) # only printing integer values for years
      axis(2,at=sort(unique(y)),las=las)
      box()
    }
    points(x,y,pch=pch,cex=cex,col=col)
    if(legend){
      legend('top',legend=legend.z,pch=legend.pch,col=col,
             pt.cex=legend.cex,ncol=legend.n,bty='n')
    }
  }
