#' Plot biology related quantities.
#' 
#' Plot biology related quantities from Stock Synthesis model output, including
#' mean weight, maturity, fecundity, and spawning output.
#' 
#' 
#' @param replist List created by \code{SS_output}
#' @param plot Plot to active plot device?
#' @param print Print to PNG files?
#' @param add add to existing plot
#' @param subplots vector controlling which subplots to create
#' @param seas which season to plot (obviously only works in seasonal models,
#' but maybe not fully implemented even then)
#' @param colvec vector of length 3 with colors for various points/lines
#' @param shadealpha Transparency parameter used to make default shadecol
#' values (see ?rgb for more info)
#' @param legendloc Location of legend (see ?legend for more info)
#' @param plotdir Directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param labels Vector of labels for plots (titles and axis labels)
#' @param pwidth Width of plot written to PNG file
#' @param pheight Height of plot written to PNG file
#' @param punits Units for PNG file
#' @param res Resolution for PNG file
#' @param ptsize Point size for PNG file
#' @param cex.main Character expansion for plot titles
#' @param verbose Return updates of function progress to the R GUI?
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SS_output}}
#' @keywords aplot hplot
SSplotBiology <-
function(replist, plot=TRUE,print=FALSE,add=FALSE,subplots=1:11,seas=1,
         colvec=c("red","blue","grey20"),shadealpha=0.1,
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
             "Default fecundity label",         #11
             "Year"),                           #12
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
  # convert colvec to semi-transparent colors for shading polygons
  shadecolvec <- rep(NA,length(colvec))
  for(icol in 1:length(colvec)){
    tmp <- col2rgb(colvec[icol])/255
    shadecolvec[icol] <- rgb(red=tmp[1], green=tmp[2], blue=tmp[3],
                             alpha=shadealpha)
  }
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
      lines(x,biology$Wt_len_F,type="o",col=colvec[1])
      if(nsexes > 1){
        lines(x,biology$Wt_len_M,type="o",col=colvec[2])
        if(!add) legend(legendloc,bty="n", c("Females","Males"), lty=1, col = c(colvec[1],colvec[2]))
      }
    }else{
      # if empirical weight-at-age IS used
      wtmat <- wtatage[wtatage$fleet==-1 & wtatage$seas==seas & wtatage$gender==1,-(2:6)]
      if(all(wtmat[1,]==wtmat[2,])) wtmat <- wtmat[-1,] # remove redundant first row
      main <- "Empirical weight at age in middle of the year"
      if(nsexes > 1){main <- "Female Empirical weight at age in middle of the year"}
      persp(x=abs(wtmat$yr),
            y=0:accuage,
            z=as.matrix(wtmat[,-1]),
            theta=70,phi=30,xlab="Year",ylab="Age",zlab="Weight",
            main=main)
      makeimage(wtmat, main=main)

      if(nsexes > 1){
      wtmat <- wtatage[wtatage$fleet==-1 & wtatage$seas==seas & wtatage$gender==2,-(2:6)]
      if(all(wtmat[1,]==wtmat[2,])) wtmat <- wtmat[-1,] # remove redundant first row
      persp(x=abs(wtmat$yr),
            y=0:accuage,
            z=as.matrix(wtmat[,-1]),
            theta=70,phi=30,xlab="Year",ylab="Age",zlab="Weight",
            main="Male Empirical weight at age in middle of the year")
      makeimage(wtmat, main="Male Empirical weight at age in middle of the year")
      }
    }
  }
  maturity_plot <- function(){ # maturity
    if(!wtatage_switch){ # if empirical weight-at-age is not used
      if(min(biology$Mat_len)<1){ # if length based
        if(!add) plot(x,biology$Mat_len,xlab=labels[1],ylab=labels[3],type="o",col=colvec[1])
        if(add) lines(x,biology$Mat_len,type="o",col=colvec[1])
      }else{ # else is age based
        if(!add) plot(growdatF$Age, growdatF$Age_Mat,xlab=labels[2],ylab=labels[3],type="o",col=colvec[1])
        if(add) lines(growdatF$Age, growdatF$Age_Mat,type="o",col=colvec[1])
      }
      if(!add) abline(h=0,col="grey")
    }else{
      #print(seas)      
      # if empirical weight-at-age IS used
      fecmat <- wtatage[wtatage$fleet==-2 & wtatage$gender==1,]
      # figure out which seasons have fecundity values (maybe always only one?)
      fecseasons <- sort(unique(fecmat$seas))
      seas_label <- NULL
#print(fecseasons)      
      for(iseas in fecseasons){
        fecmat_seas <- fecmat[fecmat$seas==iseas, -(2:6)]
#print(head(fecmat_seas))        
        # label the season only if a multi-season model
        # also testing for length of fecseasons, but that's probably redundant
        if(nseasons>1 | length(fecseasons) > 1){
          seas_label <- paste("in season",iseas)
        }
        main <- paste("Maturity x fecundity",seas_label)
        #print(head(fecmat))
        if(all(fecmat_seas[1,]==fecmat_seas[2,])) fecmat_seas <- fecmat_seas[-1,] # remove redundant first row
        persp(x=abs(fecmat_seas[,1]),
              y=0:accuage,
              z=as.matrix(fecmat_seas[,-1]),
              theta=70,phi=30,xlab="Year",ylab="Age",zlab="",
              main=main)
      }
      makeimage(fecmat_seas, main=main)

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
    
    lastbin <- max(mat2)

    image(x=0:accuage,y=yrvec2,z=t(mat2),axes=F,xlab='Age',ylab='Year',
          col=rainbow(60)[1:50], breaks=seq(0,lastbin,length=51),main=main)
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
      plot(FecX, FecY, xlab=fec_xlab, ylab=fec_ylab, ylim=c(0,ymax), col=colvec[2], pch=19)
      lines(FecX, rep(FecPar1, length(FecX)), col=colvec[1])
      text(mean(range(FecX)), FecPar1-0.05*ymax,"Egg output proportional to spawning biomass")
    }else{
      points(FecX, FecY,col=colvec[2],pch=19)
    }
  }
  fecundityOK <- all(!is.na(biology$Fecundity))
  gfunc3b <- function(){ # fecundity at weight from BIOLOGY section
    ymax <- 1.1*max(biology$Fecundity)
    if(!add){
      plot(biology$Wt_len_F, biology$Fecundity, xlab=labels[8], ylab=labels[10],
           ylim=c(0,ymax), col=colvec[1], type='o')
      abline(h=0,col="grey")
    }else{
      points(biology$Mean_Size, biology$Fecundity, col=colvec[1], type='o')
    }
  }
  gfunc3c <- function(){ # fecundity at length from BIOLOGY section
    ymax <- 1.1*max(biology$Fecundity)
    if(!add){
      plot(biology$Mean_Size, biology$Fecundity, xlab=labels[9], ylab=labels[10],
           ylim=c(0,ymax), col=colvec[1], type='o')
      abline(h=0,col="grey")
    }else{
      points(biology$Mean_Size, biology$Fecundity, col=colvec[1], type='o')
    }
  }
  gfunc4 <- function(){ # spawning output
    if(!add){
      plot(x,biology$Spawn,xlab=labels[1],ylab=labels[5],type="o",col=colvec[1])
      abline(h=0,col="grey")
    }else{
      lines(x,biology$Spawn,type="o",col=colvec[1])
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
  main <- "Ending year expected growth (with 95% intervals)"
  # if(nseasons > 1){main <- paste(main," season 1",sep="")}

  growth_curve_fn <- function() # growth
  {
    if(!add){
      plot(x,growdatF$Len_Mid,col=colvec[1],lwd=2,ylim=c(0,ymax),type="n",
           ylab=labels[6],xlab=labels[2],main=main,cex.main=cex.main)
      abline(h=0,col="grey")
    }
    col_index1 <- 3 # default is grey for single-sex model
    lty <- 1
    if(nsexes > 1){
      col_index1 <- 1 # change line for females to red
    }
    polygon(c(x, rev(x)), c(growdatF$low, rev(growdatF$high)),
            border=NA, col=shadecolvec[col_index1])
    lines(x,growdatF$Len_Mid,col=colvec[col_index1],lwd=2,lty=1)
    lines(x,growdatF$high,col=colvec[col_index1],lwd=1,lty='12')
    lines(x,growdatF$low,col=colvec[col_index1],lwd=1,lty='12')
    if(nsexes > 1){
      polygon(c(xm, rev(xm)), c(growdatM$low, rev(growdatM$high)),
              border=NA, col=shadecolvec[2])
      lines(xm,growdatM$Len_Mid,col=colvec[2],lwd=2,lty=2)
      lines(xm,growdatM$high,col=colvec[2],lwd=1,lty='13')
      lines(xm,growdatM$low,col=colvec[2],lwd=1,lty='13')
      legend(legendloc,bty="n", c("Females","Males"), lty=c(1,2), lwd=2,
             col=c(colvec[1],colvec[2]))
    }
    grid()
  }
  if(plot & 7 %in% subplots) growth_curve_fn()
  if(print & 7 %in% subplots){
    file <- paste(plotdir,"/bio7_sizeatage.png",sep="")
    caption <- "Length at age (dashed lines are 95% intervals)"
    plotinfo <- pngfun(file=file, caption=caption)
    growth_curve_fn()
    dev.off()
    plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
  }
  
  # Natural mortality (if age-dependent -- need to add time-varying M plot)
  MatAge <- growdatF$M # female mortality in the ending year
  # not sure what role M2 is playing here
  M2 <- MGparmAdj[,c(1,grep("NatM",names(MGparmAdj)))]
  # not sure when you could have ncol(M2) = NULL
  if(!is.null(ncol(M2))){ 
    M2f <- M2[,c(1,grep("Fem",names(M2)))]
    if(min(MatAge)!=max(MatAge) & 8 %in% subplots){
      ymax <- max(MatAge)
      mfunc <- function(){
        if(!add){
          plot(growdatF$Age,MatAge,col=colvec[1],lwd=2,ylim=c(0,ymax),type="n",ylab=labels[7],xlab=labels[2])
          abline(h=0,col="grey")
        }
        lines(growdatF$Age,MatAge,col=colvec[1],lwd=2,type="o")
        if(nsexes > 1){
          growdatM <- growdat[growdat$Morph==mainmorphs[2],]
          lines(growdatM$Age,growdatM$M,col=colvec[2],lwd=2,type="o")
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

  # plot time-series of any time-varying quantities
  if(11 %in% subplots){
    # general function to work for any parameter
    timeVaryingParmFunc <- function(parmlabel){
      plot(MGparmAdj$Year, MGparmAdj[[parmlabel]],
           xlab=labels[12], ylab=parmlabel, type="l", lwd=3, col=colvec[2])
    }
    # check to make sure MGparmAdj looks as expected
    # (maybe had different or conditional format in old SS versions)
    if(!is.null(ncol(MGparmAdj)) && ncol(MGparmAdj)>1){
      # loop over columns looking for time-varying parameters
      for(icol in 2:ncol(MGparmAdj)){
        parmlabel <- names(MGparmAdj)[icol]
        parmvals  <- MGparmAdj[,icol]
        # check for changes
        if(length(unique(parmvals)) > 1){
          # make plot
          if(plot) timeVaryingParmFunc(parmlabel)
          if(print){
            file <- paste(plotdir, "/bio11_time-varying_", parmlabel, ".png", sep="")
            caption <- "Time-varying mortality and growth parameters"
            plotinfo <- pngfun(file=file, caption=caption) 
            timeVaryingParmFunc(parmlabel)
            dev.off()
          }
        }
      }
    }
  }

  # add category and return plotinfo
  if(!is.null(plotinfo)) plotinfo$category <- "Bio"
  return(invisible(plotinfo))
}
