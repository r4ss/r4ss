SSplotBiology <-
  function(replist, plot=TRUE,print=FALSE,add=FALSE,subplots=1:10,seas=1,
           col1="red",col2="blue",
           legendloc="topleft",
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
             "Fecundity",                       #10
             "Default fecundity label"),        #11
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           verbose=TRUE)
{
  pngfun <- function(file,caption=NA){
    png(filename=file,width=pwidth,height=pheight,
        units=punits,res=res,pointsize=ptsize)
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
    return(plotinfo)
  }
  plotinfo <- NULL
  
  ians_blues <- c("white","grey","lightblue","skyblue","steelblue1","slateblue",
                  topo.colors(6),"blue","blue2","blue3","blue4","black")
  ians_contour <- c("white",rep("blue",100))

  #### plot function 1
  # mean weight, maturity, fecundity, spawning output

  # get objects from replist
  nseasons     <- replist$nseasons
  growdat      <- replist$endgrowth[replist$endgrowth$Seas==seas,]
  growthCVtype <- replist$growthCVtype
  biology      <- replist$biology
  startyr      <- replist$startyr
  FecType      <- replist$FecType
  FecPar1name  <- replist$FecPar1name
  FecPar2name  <- replist$FecPar2name
  FecPar1      <- replist$FecPar1
  FecPar2      <- replist$FecPar2
  parameters   <- replist$parameters
  nsexes       <- replist$nsexes
  mainmorphs   <- replist$mainmorphs
  accuage      <- replist$accuage
  startyr      <- replist$startyr
  endyr        <- replist$endyr
  growthvaries <- replist$growthvaries
  growthseries <- replist$growthseries
  ageselex     <- replist$ageselex
  MGparmAdj    <- replist$MGparmAdj
  wtatage      <- replist$wtatage
  if(!is.null(replist$wtatage_switch)) wtatage_switch  <- replist$wtatage_switch
  else stop("SSplotBiology function doesn't match SS_output function. Update one or both functions.")
  
  if(wtatage_switch) cat("Note: this model uses the emperical weight-at-age input.\n",
                         "     Therefore many of the parametric biology quantities which are plotted\n",
                         "     are not used in the model.\n")

  if(!seas %in% 1:nseasons) stop("'seas' input should be within 1:nseasons")
  # trying to fix error when spawning not in season 1:
  ## if(nrow(growdat[growdat$Gender==1 & growdat$Morph==mainmorphs[1],])==0){
  ##   seas <- replist$spawnseas
  ##   growdat      <- replist$endgrowth[replist$endgrowth$Seas==seas,]
  ##   cat("Note: growth will be shown for spawning season =",seas,"\n")
  ## }
  if(nseasons>1) labels[6] <- gsub("middle of the year", paste("middle of season",seas), labels[6])
  
  if(plotdir=="default") plotdir <- replist$inputs$dir
  # check dimensions
  if(length(mainmorphs)>nsexes){
    cat("!Error with morph indexing in SSplotBiology function.\n",
        " Code is not set up to handle multiple growth patterns or birth seasons.\n")
  }
  xlab <- labels[1]
  x <- biology$Mean_Size

  ## # stuff from selectivity that is not used
  ## FecundAtAge <- ageselex[ageselex$factor=="Fecund", names(ageselex)%in%0:accuage]
  ## WtAtAge <- ageselex[ageselex$factor=="bodywt", names(ageselex)%in%0:accuage]
 
  # determine fecundity type
  # define labels and x-variable
  if(FecType==1){
    fec_ylab <- "Eggs per kg"
    fec_xlab <- labels[8]
    FecX <- biology$Wt_len_F
    FecY <- FecPar1 + FecPar2*FecX
  }
  if(labels[11]!="Default fecundity label") fec_ylab <- labels[11]

  # Midle of season 1 (or specified season) mean length at age with 95% range of lengths (by sex if applicable)
  growdatF <- growdat[growdat$Gender==1 & growdat$Morph==mainmorphs[1],]
  growdatF$Sd_Size <- growdatF$SD_Mid
  if(growthCVtype=="logSD=f(A)"){ # lognormal distribution of length at age
    growdatF$high <- qlnorm(0.975, meanlog=log(growdatF$Len_Mid), sdlog=growdatF$Sd_Size)
    growdatF$low  <- qlnorm(0.025, meanlog=log(growdatF$Len_Mid), sdlog=growdatF$Sd_Size)
  }else{                        # normal distribution of length at age
    growdatF$high <- qnorm(0.975, mean=growdatF$Len_Mid, sd=growdatF$Sd_Size)
    growdatF$low  <- qnorm(0.025, mean=growdatF$Len_Mid, sd=growdatF$Sd_Size)
  }
  
  if(nsexes > 1){ # do males if 2-sex model
    growdatM <- growdat[growdat$Gender==2 & growdat$Morph==mainmorphs[2],]
    xm <- growdatM$Age
    growdatM$Sd_Size <- growdatM$SD_Mid
    if(growthCVtype=="logSD=f(A)"){ # lognormal distribution of length at age
      growdatM$high <- qlnorm(0.975, meanlog=log(growdatM$Len_Mid), sdlog=growdatM$Sd_Size)
      growdatM$low  <- qlnorm(0.025, meanlog=log(growdatM$Len_Mid), sdlog=growdatM$Sd_Size)
    }else{                        # normal distribution of length at age
      growdatM$high <- qnorm(0.975, mean=growdatM$Len_Mid, sd=growdatM$Sd_Size)
      growdatM$low  <- qnorm(0.025, mean=growdatM$Len_Mid, sd=growdatM$Sd_Size)
    }
  }

  weight_plot <- function(){ # weight
    if(!wtatage_switch){ # if empirical weight-at-age is not used
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
    }else{
      # if empirical weight-at-age IS used
      wtmat <- wtatage[wtatage$fleet==-1 & wtatage$seas==seas & wtatage$gender==1,-(2:6)]
      if(all(wtmat[1,]==wtmat[2,])) wtmat <- wtmat[-1,] # remove redundant first row
      persp(x=abs(wtmat$yr),
            y=0:accuage,
            z=as.matrix(wtmat[,-1]),
            theta=70,phi=30,xlab="Year",ylab="Age",zlab="Weight",
            main="Empirical weight at age in middle of the year")
      
      makeimage(wtmat, main="Empirical weight at age in middle of the year")
    }
  }
  maturity_plot <- function(){ # maturity
    if(!wtatage_switch){ # if empirical weight-at-age is not used
      if(min(biology$Mat_len)<1){ # if length based
        if(!add) plot(x,biology$Mat_len,xlab=labels[1],ylab=labels[3],type="o",col=col1)
        if(add) lines(x,biology$Mat_len,type="o",col=col1)
      }else{ # else is age based
        if(!add) plot(growdatF$Age, growdatF$Age_Mat,xlab=labels[2],ylab=labels[3],type="o",col=col1)
        if(add) lines(growdatF$Age, growdatF$Age_Mat,type="o",col=col1)
      }
      if(!add) abline(h=0,col="grey")
    }else{
      # if empirical weight-at-age IS used
      fecmat <- wtatage[wtatage$fleet==-2 & wtatage$seas==seas & wtatage$gender==1,-(2:6)]
      if(all(fecmat[1,]==fecmat[2,])) fecmat <- fecmat[-1,] # remove redundant first row
      persp(x=abs(fecmat[,1]),
            y=0:accuage,
            z=as.matrix(fecmat[,-1]),
            theta=70,phi=30,xlab="Year",ylab="Age",zlab="",
            main="Maturity x fecundity")

      makeimage(fecmat, main="Maturity x fecundity")

    }
  }

  makeimage <- function(mat,main=""){

    yrvec <- abs(mat$yr)

    ##### this stuff was used to add a row of mean values
    ## if(is.null(meanvec)){
    ##   meanvec <- mat[,1]
    ##   mat <- mat[,-1]
    ## }
    ## yrvec2 <- c(-2:-1+min(yrvec), yrvec)
    ## mat2 <- cbind(meanvec,NA,mat)
    yrvec2 <- yrvec
    mat2 <- mat[,-1]

    par(mar=c(4.2,4.2,4,1)+.1)

    ## print(length(0:accuage))
    ## print(length(yrvec2))
    ## print(dim(mat2))
    
    image(x=0:accuage,y=yrvec2,z=t(mat2),axes=F,xlab='Age',ylab='Year',
          col=rainbow(60)[1:50], breaks=seq(0,4,length=51),main=main)
    # add text
    zdataframe <- expand.grid(yr=yrvec2,age=0:accuage)
    zdataframe <- expand.grid(age=0:accuage,yr=yrvec2)
    zdataframe$z <- c(t(mat2))
    zdataframe$font <- 1
    
    ztext <- format(round(zdataframe$z,2))
    ztext[ztext=="  NA"] <- ""
    ztext[ztext=="   NA"] <- ""
    text(x=zdataframe$age,y=zdataframe$yr,label=ztext,font=zdataframe$font,cex=.7)

    # finish plot
    axis(1,at=0:accuage,cex.axis=.7);
    axis(2,at=yrvec2,las=1,cex.axis=.7)
    box()
  }



  
  gfunc3a <- function(){ # fecundity from model parameters
    ymax <- 1.1*max(FecY)
    if(!add){
      plot(FecX, FecY, xlab=fec_xlab, ylab=fec_ylab, ylim=c(0,ymax), col=col2, pch=19)
      lines(FecX, rep(FecPar1, length(FecX)), col=col1)
      text(mean(range(FecX)), FecPar1-0.05*ymax,"Egg output proportional to spawning biomass")
    }else{
      points(FecX, FecY,col=col2,pch=19)
    }
  }
  fecundityOK <- all(!is.na(biology$Fecundity))
  gfunc3b <- function(){ # fecundity at weight from BIOLOGY section
    ymax <- 1.1*max(biology$Fecundity)
    if(!add){
      plot(biology$Wt_len_F, biology$Fecundity, xlab=labels[8], ylab=labels[10],
           ylim=c(0,ymax), col=col1, type='o')
      abline(h=0,col="grey")
    }else{
      points(biology$Mean_Size, biology$Fecundity, col=col1, type='o')
    }
  }
  gfunc3c <- function(){ # fecundity at length from BIOLOGY section
    ymax <- 1.1*max(biology$Fecundity)
    if(!add){
      plot(biology$Mean_Size, biology$Fecundity, xlab=labels[9], ylab=labels[10],
           ylim=c(0,ymax), col=col1, type='o')
      abline(h=0,col="grey")
    }else{
      points(biology$Mean_Size, biology$Fecundity, col=col1, type='o')
    }
  }
  gfunc4 <- function(){ # spawning output
    if(!add){
      plot(x,biology$Spawn,xlab=labels[1],ylab=labels[5],type="o",col=col1)
      abline(h=0,col="grey")
    }else{
      lines(x,biology$Spawn,type="o",col=col1)
    }
  }
  if(plot){ # plot to screen or to PDF file
    if(1 %in% subplots) weight_plot()
    if(2 %in% subplots) maturity_plot()
    if(3 %in% subplots & FecType==1) gfunc3a()
    if(4 %in% subplots & fecundityOK) gfunc3b()
    if(5 %in% subplots & fecundityOK) gfunc3c()
    if(6 %in% subplots) gfunc4()
  }
  if(print){ # print to PNG files
    if(1 %in% subplots){
      file <- paste(plotdir,"/bio1_weightatsize.png",sep="")
      caption <- "Weight-length relationship"
      plotinfo <- pngfun(file=file, caption=caption)
      weight_plot()
      dev.off()
    }
    if(2 %in% subplots){
      file <- paste(plotdir,"/bio2_maturity.png",sep="")
      caption <- paste("Maturity at",ifelse(min(biology$Mat_len)<1,"length","age"))
      plotinfo <- pngfun(file=file, caption=caption)
      maturity_plot()
      dev.off()
    }
    if(3 %in% subplots & FecType==1){
      file <- paste(plotdir,"/bio3_fecundity.png",sep="")
      caption <- "Fecundity"
      plotinfo <- pngfun(file=file, caption=caption)
      gfunc3a()
      dev.off()
    }
    if(4 %in% subplots & fecundityOK){
      file <- paste(plotdir,"/bio4_fecundity_wt.png",sep="")
      caption <- "Fecundity as a function of weight"
      plotinfo <- pngfun(file=file, caption=caption)
      gfunc3b()
      dev.off()
    }
    if(5 %in% subplots & fecundityOK){
      file <- paste(plotdir,"/bio5_fecundity_len.png",sep="")
      caption <- "Fecundity as a function of length"
      plotinfo <- pngfun(file=file, caption=caption)
      gfunc3c()
      dev.off()
    }
    if(6 %in% subplots){
      file <- paste(plotdir,"/bio6_spawningoutput.png",sep="")
      caption <- "Spawning output at length"
      plotinfo <- pngfun(file=file, caption=caption)
      gfunc4()
      dev.off()
    }
  }

  ymax <- max(growdatF$high)
  if(nsexes > 1) ymax <- max(ymax,growdatM$high)
  x <- growdatF$Age
  main <- "Ending year expected growth"
  # if(nseasons > 1){main <- paste(main," season 1",sep="")}

  gfunc5 <- function() # growth
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
      legend(legendloc,bty="n", c("Females","Males"), lty=1, col = c(col1,col2))
    }
    grid()
  }
  if(plot & 7 %in% subplots) gfunc5()
  if(print & 7 %in% subplots){
    file <- paste(plotdir,"/bio7_sizeatage.png",sep="")
    caption <- "Length at age"
    plotinfo <- pngfun(file=file, caption=caption)
    gfunc5()
    dev.off()
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
  }

  # Natural mortality (if time varying)
  M <- growdatF$M # female mortality in the ending year
  M2 <- MGparmAdj[,c(1,grep("NatM",names(MGparmAdj)))]
  # if dimension is NULL there were no time-varying natural mortality parameters
  if(!is.null(ncol(M2))){ 
    M2f <- M2[,c(1,grep("Fem",names(M2)))]
    if(min(M)!=max(M) & 8 %in% subplots){
      ymax <- max(M)
      mfunc <- function(){
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
      if(plot & 8 %in% subplots) mfunc()
      if(print & 8 %in% subplots){
        file <- paste(plotdir,"/bio8_natmort.png",sep="")
        caption <- "Natural mortality"
        plotinfo <- pngfun(file=file, caption=caption) 
        mfunc()
        dev.off()
      }
    }
  }

  # Time-varying growth (formerly plot #2)
  if(is.null(growthvaries)){
    if(verbose) cat("No check for time-varying growth for this type of model (not sure why)\n")
  }else{ # temporarily disable multi-season plotting of time-varying growth
    if(is.null(growthseries))
    {
      cat("! Warning: no time-varying growth info because\n",
          "          'detailed age-structured reports' turned off in starter file.\n")
    }else{
      if(growthvaries) # if growth is time varying
      for(i in 1:nsexes)
      {
        growdatuse <- growthseries[growthseries$Yr >= startyr-2 &
                                   growthseries$Morph==mainmorphs[i],]
        x <- 0:accuage
        y <- growdatuse$Yr
        z <- as.matrix(growdatuse[,-(1:4)])
        time <- FALSE
        for(t in 1:ncol(z)) if(max(z[,t])!=min(z[,t])) time <- TRUE
        if(time)
        {
          z <- t(z)
          if(i==1){main <- "Female time-varying growth"}
          if(nsexes==1){main <- "Time-varying growth"}
          if(i==2){main <- "Male time-varying growth"}
          if(nseasons > 1){main <- paste(main," season 1",sep="")}
          if(plot){
            if(9 %in% subplots)
              persp(x,y,z,col="white",xlab=labels[2],ylab="",zlab=labels[1],expand=0.5,
                    box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",
                    phi=35,theta=-10)
            if(10 %in% subplots)
              contour(x,y,z,nlevels=12,xlab=labels[2],
                      main=main,cex.main=cex.main,col=ians_contour,lwd=2)}
          if(print){
            if(9 %in% subplots){
              file <- paste(plotdir,"/bio9_timevarygrowthsurf_sex",i,".png",sep="")
              caption <- "Perspective plot of time-varying growth"
              plotinfo <- pngfun(file=file, caption=caption)
              persp(x,y,z,col="white",xlab=labels[2],ylab="",zlab=labels[1],expand=0.5,
                    box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",
                    phi=35,theta=-10)
              dev.off()
            }
            if(10 %in% subplots){
              file <- paste(plotdir,"/bio10_timevarygrowthcontour_sex",i,".png",sep="")
              caption <- "Contour plot of time-varying growth"
              plotinfo <- pngfun(file=file, caption=caption)
              contour(x,y,z,nlevels=12,xlab=labels[2],
                      main=main,cex.main=cex.main,col=ians_contour,lwd=2)
              dev.off()
            }
          } # end print
        } # end if time-varying
      } # end loop over sexes
    } # end of if data available for time varying growth
  }# end disable of time-varying growth for multi-season models
  if(!is.null(plotinfo)) plotinfo$category <- "Bio"
  return(invisible(plotinfo))
}
