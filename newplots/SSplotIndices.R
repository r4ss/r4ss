SSplotIndices <-
  function(replist,subplots=1:4,fleetnames="default",
           cex.main=1,smooth=TRUE,
           add=FALSE,plot=TRUE,print=FALSE,
           fleets="all",
           labels=c("Year",        #1
             "Index",              #2
             "Observed index",     #3
             "Expected index",     #4
             "Log index",          #5
             "Log observed index", #6
             "Log expected index"),#7
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,
           plotdir="default",
           verbose=TRUE)
{
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  cpue        <- replist$cpue
  FleetNames  <- replist$FleetNames
  if(fleetnames[1]=="default") fleetnames <- FleetNames

  for(i in unique(cpue$Fleet)){
    cpueuse <- cpue[cpue$Obs > 0 & cpue$Fleet==i,]
    x <- cpueuse$Yr
    y <- cpueuse$Obs
    z <- cpueuse$Exp
    uiw <- qlnorm(.975,meanlog=log(y),sdlog=cpueuse$SE) - y
    liw <- y - qlnorm(.025,meanlog=log(y),sdlog=cpueuse$SE)
    npoints <- length(z)
    main=paste(labels[2], i,sep=" ")
    cpuefun1 <- function(){
      plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab=labels[1],ylo=0,col="red",ylab=labels[2],main=main,cex.main=cex.main,lty=1)
      abline(h=0,col="grey")
      lines(x,z,lwd=2,col="blue")
    }
    cpuefun2 <- function(){
      plot(y,z,xlab=labels[3],main=main,cex.main=cex.main,ylim=c(0,max(z)),xlim=c(0,max(y)),col="blue",pch=19,ylab=labels[4])
      abline(h=0,col="grey")
      lines(x=c(0,max(z)),y=c(0,max(z)))
      if(smooth && npoints > 6 && diff(range(y))>0){
        psmooth <- loess(z~y,degree=1)
        lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
    }
    if(plot){
      if(1 %in% subplots) cpuefun1()
      if(2 %in% subplots) cpuefun2()
    }
    if(print){
      if(1 %in% subplots){
        png(file=paste(plotdir,"13_cpuefit",i,".png",sep=""))
        cpuefun1()
        dev.off()
      }
      if(2 %in% subplots){
        png(file=paste(plotdir,"13_cpuecheck",i,".png",sep=""))
        cpuefun2()
        dev.off()
      }
    }

    # same plots again in log space
    main <- paste(labels[5], i,sep=" ")
    uiw <- qnorm(.975,mean=log(y),sd=cpueuse$SE) - log(y)
    liw <- log(y) - qnorm(.025,mean=log(y),sd=cpueuse$SE)
    cpuefun3 <- function(){
      plotCI(x=x,y=log(y),z=log(z),sfrac=0.001,uiw=uiw,liw=liw,xlab=labels[1],col="red",ylab=labels[5],main=main,cex.main=cex.main,lty=1)
      lines(x,log(z),lwd=2,col="blue")
    }
    cpuefun4 <- function(){
      plot(log(y),log(z),xlab=labels[6],main=main,cex.main=cex.main,col="blue",pch=19,ylab=labels[7])
      lines(x=range(log(z)),y=range(log(z)))
      if(smooth && npoints > 6 && diff(range(y))>0){
        psmooth <- loess(log(z)~log(y),degree=1)
        lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col="red",lty="dashed")}
    }
    if(plot){
      if(3 %in% subplots) cpuefun3()
      if(4 %in% subplots) cpuefun4()
    }
    if(print){
      if(3 %in% subplots){
        png(file=paste(plotdir,"13_logcpuefit",i,".png",sep=""))
        cpuefun3()
        dev.off()
      }
      if(4 %in% subplots){
        png(file=paste(plotdir,"13_logcpuecheck",i,".png",sep=""))
        cpuefun4()
        dev.off()
      }
    }
  } # nfleets
  if(verbose) print("Finished plot 13: CPUE plots",quote=FALSE)
  flush.console()
} # end function
