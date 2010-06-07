SSplotBiology <-
  function(replist, plot=T,print=F,add=F,subplots=1:8,
           col1="red",col2="blue",
           legendloc="topleft",cex.main=1,
           cohortlines=c(),
           plotdir="default",
           labels=c("Length (cm)",              #1
             "Age (yr)",                        #2
             "Maturity",                        #3
             "Mean weight (kg) in last year",   #4
             "Spawning output",                 #5
             "Length (cm, middle of the year)", #6
             "Natural mortality",               #7
             "Female weight (kg)",              #8
             "Female length (cm)",              #9
             "Default fecundity label"),        #10
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12)
{
  pngfun <- function(file) png(file=file,width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)

  ians_blues <- c("white","grey","lightblue","skyblue","steelblue1","slateblue",topo.colors(6),"blue","blue2","blue3","blue4","black")
  ians_contour <- c("white",rep("blue",100))

  #### plot function 1
  # mean weight, maturity, fecundity, spawning output

  # get objects from replist
  growdat      <- replist$endgrowth
  biology      <- replist$biology
  FecType      <- replist$FecType
  FecPar1name  <- replist$FecPar1name
  FecPar2name  <- replist$FecPar2name
  FecPar1      <- replist$FecPar1
  FecPar2      <- replist$FecPar2
  parameters   <- replist$parameters
  nsexes       <- replist$nsexes
  nseasons     <- replist$nseasons
  mainmorphs   <- replist$mainmorphs
  accuage      <- replist$accuage
  growthseries <- replist$growthseries

  if(plotdir=="default") plotdir <- replist$inputs$dir
  # check dimensions
  if(length(mainmorphs)>nsexes){
    print("!Error with morph indexing in SSplotbiology function.",quote=F)
    print(" Code is not set up to handle multiple growth patterns or birth seasons.",quote=F)
  }
  xlab <- labels[1]
  x <- biology$Mean_Size

  # determine fecundity type
  # define labels and x-variable
  if(FecType==1){
    fec_ylab <- "Eggs per kg"
    fec_xlab <- labels[8]
    FecX <- biology$Wt_len_F
  }
  if(FecType==2){
    fec_ylab <- "Eggs per kg"
    fec_xlab <- labels[9]
    FecX <- biology$Mean_Size
  }
  if(FecType==3){
    fec_ylab <- "Eggs per kg"
    fec_xlab <- labels[8]
    FecX <- biology$Wt_len_F
  }
  if(labels[10]!="Default fecundity label") fec_ylab <- labels[10]

  if(FecType==1) FecY <- FecPar1 + FecPar2*FecX
  if(FecType==2) FecY <- FecPar1*FecX^FecPar2
  if(FecType==3) FecY <- FecPar1*FecX^FecPar2

  # Mid year mean length at age with 95% range of lengths (by sex if applicable)
  growdatF <- growdat[growdat$Gender==1 & growdat$Morph==mainmorphs[1],]
  growdatF$Sd_Size <- growdatF$SD_Mid
  growdatF$high <- growdatF$Len_Mid + 1.96*growdatF$Sd_Size
  growdatF$low <- growdatF$Len_Mid - 1.96*growdatF$Sd_Size
  if(nsexes > 1){
    growdatM <- growdat[growdat$Gender==2 & growdat$Morph==mainmorphs[2],]
    xm <- growdatM$Age
    growdatM$Sd_Size <- growdatM$SD_Mid
    growdatM$high <- growdatM$Len_Mid + 1.96*growdatM$Sd_Size
    growdatM$low <- growdatM$Len_Mid - 1.96*growdatM$Sd_Size
  }

  gfunc1 <- function(add=F){ # weight
    if(!add){
      ymax <- max(biology$Wt_len_F)
      if(nsexes>1) ymax <- max(ymax, biology$Wt_len_M)
      plot(x,x,ylim=c(0,1.1*ymax),xlab=xlab,ylab=labels[4],type="n")
      abline(h=0,col="grey")
    }
    lines(x,biology$Wt_len_F,type="o",col=col1)
    if(nsexes > 1){
      lines(x,biology$Wt_len_M,type="o",col=col2)
      if(!add) legend(legendloc,bty="n", c("Females","Males"), lty=1, col = c(col1,col2))
    }
  }
  gfunc2 <- function(add=F){ # maturity
    if(min(biology$Mat_len)<1){ # if length based
      if(!add) plot(x,biology$Mat_len,xlab=labels[1],ylab=labels[3],type="o",col=col1)
      if(add) lines(x,biology$Mat_len,type="o",col=col1)
    }else{ # else is age based
      if(!add) plot(growdatF$Age, growdatF$Age_Mat,xlab=labels[2],ylab=labels[3],type="o",col=col1)
      if(add) lines(growdatF$Age, growdatF$Age_Mat,type="o",col=col1)
    }
    if(!add) abline(h=0,col="grey")
  }
  gfunc3 <- function(add=F){ # fecundity
    ymax <- 1.1*max(FecY)
    if(!add){
      plot(FecX, FecY, xlab=fec_xlab, ylab=fec_ylab, ylim=c(0,ymax), col=col2, pch=19)
      lines(FecX, rep(FecPar1, length(FecX)), col=col1)
      text(mean(range(FecX)), FecPar1-0.05*ymax,"Egg output proportional to spawning biomass")
    }else{
      points(FecX, FecY,col=col2,pch=19)
    }
  }
  gfunc4 <- function(add=F){ # spawning output
    if(!add){
      plot(x,biology$Spawn,xlab=labels[1],ylab=labels[5],type="o",col=col1)
      abline(h=0,col="grey")
    }else{
      lines(x,biology$Spawn,type="o",col=col1)
    }
  }
  if(plot){ # plot to screen or to PDF file
    if(1 %in% subplots) gfunc1()
    if(2 %in% subplots) gfunc2()
    if(3 %in% subplots) gfunc3()
    if(4 %in% subplots) gfunc4()}
  if(print){ # print to PNG files
    if(1 %in% subplots){
      pngfun(file=paste(plotdir,"01_weightatsize.png",sep=""))
      gfunc1()
      dev.off()}
    if(2 %in% subplots){
      pngfun(file=paste(plotdir,"01_maturity.png",sep=""))
      gfunc2()
      dev.off()}
    if(3 %in% subplots){
      pngfun(file=paste(plotdir,"01_fecundity.png",sep=""))
      gfunc3()
      dev.off()}
    if(4 %in% subplots){
      pngfun(file=paste(plotdir,"01_spawningoutput.png",sep=""))
      gfunc4()
      dev.off()}
  }

  ymax <- max(growdatF$high)
  if(nsexes > 1) ymax <- max(ymax,growdatM$high)
  x <- growdatF$Age
  main <- "Ending year expected growth"
  # if(nseasons > 1){main <- paste(main," season 1",sep="")}

  gfunc5 <- function(add=F) # growth
  {
    if(!add){
      plot(x,growdatF$Len_Mid,col=col1,lwd=2,ylim=c(0,ymax),type="n",
           ylab=labels[6],xlab=labels[2],main=main,cex.main=cex.main)
      abline(h=0,col="grey")
    }
    lines(x,growdatF$Len_Mid,col=col1,lwd=2)
    lines(x,growdatF$high,col=col1,lwd=1,lty="dashed")
    lines(x,growdatF$low,col=col1,lwd=1,lty="dashed")
    if(nsexes > 1)
    {
      lines(xm,growdatM$Len_Mid,col=col2,lwd=2)
      lines(xm,growdatM$high,col=col2,lwd=1,lty="dashed")
      lines(xm,growdatM$low,col=col2,lwd=1,lty="dashed")
      grid()
      legend(legendloc,bty="n", c("Females","Males"), lty=1, col = c(col1,col2))
    }
  }
  if(plot & 5 %in% subplots) gfunc5()
  if(print & 5 %in% subplots){
    pngfun(file=paste(plotdir,"01_sizeatage.png",sep=""))
    gfunc5()
    dev.off()}

  # Natural mortality (if time or sex varying)
  M <- growdatF$M
  if(min(M)!=max(M) & 6 %in% subplots)
  {
    ymax <- max(M)
    mfunc <- function()
    {
      if(!add){
        plot(growdatF$Age,M,col=col1,lwd=2,ylim=c(0,ymax),type="n",ylab=labels[7],xlab=labels[2])
        abline(h=0,col="grey")
      }
      lines(growdatF$Age,M,col=col1,lwd=2,type="o")
      if(nsexes > 1){
        growdatM <- growdat[growdat$Morph==mainmorphs[2],]
        lines(growdatM$Age,growdatM$M,col=col2,lwd=2,type="o")
      }
    }
    if(plot & 6 %in% subplots) mfunc()
    if(print & 6 %in% subplots){
      pngfun(file=paste(plotdir,"01_natmort.png",sep=""))
      mfunc()
      dev.off()}
  }

  # Time-varying growth (formerly plot #2)
  if(nseasons > 1){
    if(verbose) print("No check for time-varying growth in multi-season models yet",quote=F)
  }else{ # temporarily disable multi-season plotting of time-varying growth
    if(is.null(growthseries))
    {
      print("! Warning: no time-varying growth info because 'detailed age-structured reports' turned off in starter file.",quote=F)
    }else{
      if(replist$growthvaries) # if growth is time varying
      for(i in 1:nsexes)
      {
        growdatuse <- growthseries[growthseries$Morph==mainmorphs[i],]
        x <- 0:accuage
        y <- growdatuse$Yr
        z <- as.matrix(growdatuse[,-(1:4)])
        time <- FALSE
        for(t in 1:ncol(z)) if(max(z[,t])!=min(z[,t])) time <- T
        if(time)
        {
          if(length(cohortlines)>0) print("Growth is time-varying, which is not yet implemented in cohortlines added to length comps.",quote=F)
          z <- t(z)
          if(i==1){main <- "Female time-varying growth"}
          if(nsexes==1){main <- "Time-varying growth"}
          if(i==2){main <- "Male time-varying growth"}
          if(nseasons > 1){main <- paste(main," season 1",sep="")}
          if(plot){
            if(7 %in% subplots) persp(x,y,z,col="white",xlab=labels[2],ylab="",zlab=labels[1],expand=0.5,box=T,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
            if(8 %in% subplots) contour(x,y,z,nlevels=12,xlab=labels[2],main=main,cex.main=cex.main,col=ians_contour,lwd=2)}
          if(print){
            if(7 %in% subplots){
              pngfun(file=paste(plotdir,"02_timevarygrowthsurf",i,"sex",m,".png",sep=""))
              persp(x,y,z,col="white",xlab=labels[2],ylab="",zlab=labels[1],expand=0.5,box=T,main=main,cex.main=cex.main,ticktype="detailed",phi=35,theta=-10)
              dev.off()
            }
            if(8 %in% subplots){
              pngfun(file=paste(plotdir,"02_timevarygrowthcontour",i,"sex",m,".png",sep=""))
              contour(x,y,z,nlevels=12,xlab=labels[2],main=main,cex.main=cex.main,col=ians_contour,lwd=2)
              dev.off()
            }
          } # end print
        } # end if time-varying
      } # end loop over sexes
    } # end of if data available for time varying growth
  }# end disable of time-varying growth for multi-season models
}
