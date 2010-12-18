bubble3 <- function (x,y,z,col=c(1,1),maxsize=3,do.sqrt=TRUE,
      	       main="",cex.main=1,xlab="",ylab="",minnbubble=8,
      	       xlimextra=1,add=FALSE,las=1,allopen=TRUE)
{
    # vaguely based on bubble() from gstat
    az <- abs(z)
    if (do.sqrt) az <- sqrt(az)
    cex <- maxsize * az/max(az)
    z.col <- ifelse(z < 0, col[1], col[2])
    xlim <- range(x)
    if(length(unique(x))<minnbubble) xlim=xlim+c(-1,1)*xlimextra
    pch <- z
    pch[pch==0] <- NA
    pch[pch>0] <- 16
    pch[pch<0] <- 1
    if(allopen) pch[!is.na(pch)] <- 1
    if(!add){
      plot(x,y,type="n",xlim=xlim,main=main,cex.main=cex.main,
           xlab=xlab,ylab=ylab,axes=FALSE)
      xvec <- unique(x)
      axis(1,at=floor(unique(x))) # only printing integer values for years (or whatever)
      axis(2,las=las)
      box()
    }
    points(x,y,pch=pch,cex=cex,col=z.col)
  }
