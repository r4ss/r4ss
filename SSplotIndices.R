SSplotIndices <-
function(replist,subplots=1:7,
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
           "Standardized index"),#8
         col1="default",
         col2="default",
         col3="blue",
         col4="red",
         legend=TRUE,
         legendloc="topright",
         seasnames=NULL,
         pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
         plotdir="default",
         verbose=TRUE)
{
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  cpue        <- replist$cpue
  if(is.null(dim(cpue))){
    cat("no CPUE data in this model\n")
    return()
  }
  FleetNames  <- replist$FleetNames
  nfleets     <- replist$nfleets
  nseasons    <- replist$nseasons
  cpue$YrSeas <- cpue$Yr + (cpue$Seas - 0.5)/nseasons
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
    colvec1 <- "red"
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


  if(datplot){
   allcpue <- data.frame(NA)
   names(allcpue) <- "Index"
   allcpue$year <- NA
   allcpue$value <- NA
   allcpue$stdvalue <- NA}

  # loop over fleets
  for(ifleet in fleetvec){
    Fleet <- fleetnames[ifleet]
    cpueuse <- cpue[cpue$Obs > 0 & cpue$FleetNum==ifleet,]
    cpueuse <- cpueuse[order(cpueuse$YrSeas),]
    x <- cpueuse$YrSeas
    y <- cpueuse$Obs
    z <- cpueuse$Exp
    if(usecol) s <- cpueuse$Seas else s <- 1 # only use colorvector if more than 1 season
    if(datplot){
      cpueuse$Index <- rep(ifleet,length(cpueuse$YrSeas))
      cpueuse$stdvalue <- cpueuse$Obs/mean(cpueuse$Obs)
      tempcpue <- cbind(cpueuse$Index,cpueuse$YrSeas,cpueuse$Obs,cpueuse$stdvalue)
      colnames(tempcpue) <- c("Index","year","value","stdvalue")
      allcpue <- rbind(allcpue,tempcpue)
    }
    uiw <- qlnorm(.975,meanlog=log(y),sdlog=cpueuse$SE) - y
    liw <- y - qlnorm(.025,meanlog=log(y),sdlog=cpueuse$SE)
    npoints <- length(z)
    main=paste(labels[2], Fleet,sep=" ")

    addlegend <- function(pch, colvec){
      names <- paste(seasnames,"observations")
    }

    cpuefun1 <- function(addexpected=TRUE){
      # plot of time-series of observed and expected (if requested)
      plotCI(x=x,y=y,z=z,sfrac=0.001,uiw=uiw,liw=liw,xlab=labels[1],ylo=0,col=colvec1[s],ylab=labels[2],main=main,cex.main=cex.main,lty=1)
      abline(h=0,col="grey")
      if(addexpected) lines(x,z,lwd=2,col=col3)
      if(legend & length(colvec1)>1) legend(x=legendloc, legend=seasnames, pch=1, col=colvec1)
    }
    cpuefun2 <- function(){
      # plot of observed vs. expected with smoother
      plot(y,z,xlab=labels[3],main=main,cex.main=cex.main,ylim=c(0,max(z)),xlim=c(0,max(y)),col=colvec2[s],pch=19,ylab=labels[4])
      abline(h=0,col="grey")
      lines(x=c(0,max(z)),y=c(0,max(z)))
      if(smooth && npoints > 6 && diff(range(y))>0)
      {
        psmooth <- loess(z~y,degree=1)
        lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col=col4,lty="dashed")
      }
      if(legend & length(colvec2)>1) legend(x=legendloc, legend=seasnames, pch=16, col=colvec2)
    }

    if(plot){
      if(1 %in% subplots & datplot) cpuefun1(addexpected=FALSE)
      if(2 %in% subplots) cpuefun1()
      if(3 %in% subplots) cpuefun2()
    }
    if(print){
      if(1 %in% subplots){
        pngfun(file=paste(plotdir,"13_cpuedata",Fleet,".png",sep=""))
        cpuefun1(addexpected=FALSE)
        dev.off()
      }
      if(2 %in% subplots){
        pngfun(file=paste(plotdir,"13_cpuefit",Fleet,".png",sep=""))
        cpuefun1()
        dev.off()
      }
      if(3 %in% subplots){
        pngfun(file=paste(plotdir,"13_cpuecheck",Fleet,".png",sep=""))
        cpuefun2()
        dev.off()
      }
    }

    # same plots again in log space
    main <- paste(labels[5], Fleet, sep=" ")
    uiw <- qnorm(.975,mean=log(y),sd=cpueuse$SE) - log(y)
    liw <- log(y) - qnorm(.025,mean=log(y),sd=cpueuse$SE)
    cpuefun3 <- function(addexpected=TRUE){
      # plot of time-series of log(observed) and log(expected) (if requested)
      plotCI(x=x,y=log(y),z=log(z),sfrac=0.001,uiw=uiw,liw=liw,xlab=labels[1],col=colvec1[s],ylab=labels[5],main=main,cex.main=cex.main,lty=1)
      if(addexpected) lines(x,log(z),lwd=2,col=col3)
      if(length(colvec1)>1) legend(x=legendloc, legend=seasnames, pch=1, col=colvec1)
    }
    cpuefun4 <- function(){
      # plot of log(observed) vs. log(expected) with smoother
      plot(log(y),log(z),xlab=labels[6],main=main,cex.main=cex.main,col=colvec2[s],pch=19,ylab=labels[7])
      lines(x=range(log(z)),y=range(log(z)))
      if(smooth && npoints > 6 && diff(range(y))>0){
        psmooth <- loess(log(z)~log(y),degree=1)
        lines(psmooth$x[order(psmooth$x)],psmooth$fit[order(psmooth$x)],lwd=1.2,col=col4,lty="dashed")}
      if(length(colvec2)>1) legend(x=legendloc, legend=seasnames, pch=16, col=colvec2)
    }
    if(plot){
      if(4 %in% subplots) cpuefun3(addexpected=FALSE)
      if(5 %in% subplots) cpuefun3()
      if(6 %in% subplots) cpuefun4()
    }
    if(print){
      if(4 %in% subplots & datplot){
        pngfun(file=paste(plotdir,"13_logcpuedata",Fleet,".png",sep=""))
        cpuefun3(addexpected=FALSE)
        dev.off()
      }
      if(5 %in% subplots){
        pngfun(file=paste(plotdir,"13_logcpuefit",Fleet,".png",sep=""))
        cpuefun3()
        dev.off()
      }
      if(6 %in% subplots){
        pngfun(file=paste(plotdir,"13_logcpuecheck",Fleet,".png",sep=""))
        cpuefun4()
        dev.off()
      }
    }
  } # nfleets

  ### New the standardized plot of all CPUE indices
  if(datplot==T){
   cpuefun5 <- function(){
    main="All cpue plot"
    xlim <- c(min(allcpue$year,na.rm=T)-1,max(allcpue$year,na.rm=T)+1)
    ylim <- c(range(allcpue$stdvalue,na.rm=T))
    usecols <- rich.colors.short(max(allcpue$Index,na.rm=T))
    if(max(allcpue$Index,na.rm=T) >= 2) usecols <- rich.colors.short(max(allcpue$Index,na.rm=T)+1)[-1]
     plot(x=allcpue$year[allcpue$Index %in% c(fleetvec[1])],y=allcpue$stdvalue[allcpue$Index %in% c(fleetvec[1])],
          xlab=labels[1],main=main,cex.main=cex.main,col=usecols[1],pch=19,ylab=labels[8],xlim=xlim,ylim=ylim)
     lines(x=allcpue$year[allcpue$Index %in% c(fleetvec[1])],y=allcpue$stdvalue[allcpue$Index %in% c(fleetvec[1])],
           col=usecols[1],lwd=0.4,lty="dashed")
     fleetvec2 <- fleetvec[fleetvec != fleetvec[1]]
     for(ifleet in fleetvec2){
      points(x=allcpue$year[allcpue$Index %in% c(ifleet)],y=allcpue$stdvalue[allcpue$Index %in% c(ifleet)],
      pch=19,col=usecols[ifleet])
      lines(x=allcpue$year[allcpue$Index %in% c(ifleet)],y=allcpue$stdvalue[allcpue$Index %in% c(ifleet)],
            col=usecols[ifleet],lwd=0.4,lty="dashed")
      }
   } # end cpuefun5
  if(plot & (7 %in% subplots)){cpuefun5()}
  if(print & (7 %in% subplots)){
   pngfun(file=paste(plotdir,"13_standcpueall",".png",sep=""))
   cpuefun5()
   dev.off()}
  } # end datplot

  if(verbose) cat("Finished plot 13: CPUE plots\n")
  flush.console()

} # end function
