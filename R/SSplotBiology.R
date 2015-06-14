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
#' @param pwidth Width of plot
#' @param pheight Height of plot
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
function(replist, plot=TRUE,print=FALSE,add=FALSE,subplots=1:14,seas=1,
         colvec=c("red","blue","grey20"),shadealpha=0.1,
         legendloc="topleft",
         plotdir="default",
         labels=c("Length (cm)",              #1
             "Age (yr)",                        #2
             "Maturity",                        #3
             "Mean weight (kg) in last year",   #4
             "Spawning output",                 #5
             "Length (cm, beginning of the year)", #6
             "Natural mortality",               #7
             "Female weight (kg)",              #8
             "Female length (cm)",              #9
             "Fecundity",                       #10
             "Default fecundity label",         #11
             "Year"),                           #12
         pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
         verbose=TRUE)
{
  #### previous order of plots:
  # subplot 1: weight-length
  # subplot 2: maturity
  # subplot 3: gfunc3a - fecundity from model parameters
  # subplot 4: gfunc3b - fecundity at weight from BIOLOGY section
  # subplot 5: gfunc3c - fecundity at length from BIOLOGY section
  # subplot 6: gfunc4  - spawning output
  # subplot 7: growth_curve_fn - growth curve
  # subplot 8: mfunc   - Natural mortality (if age-dependent)
  # subplot 9:  [no function] - Time-varying growth persp
  # subplot 10: [no function] - Time-varying growth contour
  # subplot 11: timeVaryingParmFunc - plot time-series of any time-varying quantities

  #### new (24-Oct-14) order of plots:
  # subplot 1: growth_curve_fn - growth curve only
  # subplot 2: growth_curve_plus_fn - growth curve with CV and SD
  # subplot 3: growth_curve_plus_fn - growth curve with maturity and weight
  # subplot 1: weight-length
  # subplot 2: maturity
  # subplot 3: gfunc3a - fecundity from model parameters
  # subplot 4: gfunc3b - fecundity at weight from BIOLOGY section
  # subplot 5: gfunc3c - fecundity at length from BIOLOGY section
  # subplot 6: gfunc4  - spawning output
  # subplot 11: mfunc   - Natural mortality (if age-dependent)
  # subplot 12: [no function] - Time-varying growth persp
  # subplot 13: [no function] - Time-varying growth contour
  # subplot 14: timeVaryingParmFunc - plot time-series of any time-varying quantities
  # EXTRA PLOTS NOT PRODUCED BY DEFAULT
  # subplot 101: diagram with labels showing female growth curve
  # subplot 102: diagram with labels showing female growth curve & male offsets
  # subplot 103: diagram with labels showing female CV = f(A) (offset type 2)
  # subplot 104: diagram with labels showing female CV = f(A) & male offset (type 2)
  # subplot 105: diagram with labels showing female CV = f(A) (offset type 3)
  # subplot 106: diagram with labels showing female CV = f(A) & male offset (type 3)

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
  # calculate CVs from SD and mean length
  growdat$CV_Beg <- growdat$SD_Beg/growdat$Len_Beg
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
  Growth_Parameters <- replist$Growth_Parameters

  # get any derived quantities related to growth curve uncertainty
  Grow_std <- replist$derived_quants[grep("Grow_std_", replist$derived_quants$LABEL),]
  if(nrow(Grow_std)==0){
    Grow_std <- NULL
  }else{
    # convert things like "Grow_std_1_Fem_A_25" into
    # "pattern 1, female, age 25"
    Grow_std$pattern <- NA
    Grow_std$sex_char <- NA
    Grow_std$sex <- NA
    Grow_std$age <- NA
    for(irow in 1:nrow(Grow_std)){
      tmp <- strsplit(Grow_std$LABEL[irow], split="_")[[1]]
      Grow_std$pattern[irow] <- as.numeric(tmp[3])
      Grow_std$sex_char[irow] <- tmp[4]
      Grow_std$age[irow] <- as.numeric(tmp[6])
    }
    Grow_std$sex[Grow_std$sex_char=="Fem"] <- 1
    Grow_std$sex[Grow_std$sex_char=="Mal"] <- 2
    #### now it should look something like this:
    ##                                   LABEL Value   StdDev pattern sex_char sex age
    ## Grow_std_1_Fem_A_5   Grow_std_1_Fem_A_5     0 1.772300       1      Fem   1   5
    ## Grow_std_1_Fem_A_10 Grow_std_1_Fem_A_10     0 1.039320       1      Fem   1  10
  }

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
  if(nseasons>1){
    labels[6] <- gsub("beginning of the year",
                      paste("beginning of season",seas), labels[6])
  }

  if(plotdir=="default"){
    plotdir <- replist$inputs$dir
  }
  # check dimensions
  if(length(mainmorphs)>nsexes){
    cat("!Error with morph indexing in SSplotBiology function.\n",
        " Code is not set up to handle multiple growth patterns or birth seasons.\n")
  }

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

  # Begle of season 1 (or specified season) mean length at age with 95% range of lengths (by sex if applicable)
  growdatF <- growdat[growdat$Gender==1 & growdat$Morph==mainmorphs[1],]
  growdatF$Sd_Size <- growdatF$SD_Beg
  if(growthCVtype=="logSD=f(A)"){ # lognormal distribution of length at age
    growdatF$high <- qlnorm(0.975, meanlog=log(growdatF$Len_Beg), sdlog=growdatF$Sd_Size)
    growdatF$low  <- qlnorm(0.025, meanlog=log(growdatF$Len_Beg), sdlog=growdatF$Sd_Size)
  }else{                        # normal distribution of length at age
    growdatF$high <- qnorm(0.975, mean=growdatF$Len_Beg, sd=growdatF$Sd_Size)
    growdatF$low  <- qnorm(0.025, mean=growdatF$Len_Beg, sd=growdatF$Sd_Size)
  }

  if(nsexes > 1){ # do males if 2-sex model
    growdatM <- growdat[growdat$Gender==2 & growdat$Morph==mainmorphs[2],]
    # IAN T. this should probably be generalized
    xm <- growdatM$Age_Beg

    growdatM$Sd_Size <- growdatM$SD_Beg
    if(growthCVtype=="logSD=f(A)"){ # lognormal distribution of length at age
      growdatM$high <- qlnorm(0.975, meanlog=log(growdatM$Len_Beg), sdlog=growdatM$Sd_Size)
      growdatM$low  <- qlnorm(0.025, meanlog=log(growdatM$Len_Beg), sdlog=growdatM$Sd_Size)
    }else{                        # normal distribution of length at age
      growdatM$high <- qnorm(0.975, mean=growdatM$Len_Beg, sd=growdatM$Sd_Size)
      growdatM$low  <- qnorm(0.025, mean=growdatM$Len_Beg, sd=growdatM$Sd_Size)
    }
  }
  weight_plot <- function(){ # weight
    x <- biology$Mean_Size
    if(!wtatage_switch){ # if empirical weight-at-age is not used
      if(!add){
        ymax <- max(biology$Wt_len_F)
        if(nsexes>1) ymax <- max(ymax, biology$Wt_len_M)
        plot(x,x,ylim=c(0,1.1*ymax), xlab=labels[1], ylab=labels[4], type="n")
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
      x <- biology$Mean_Size
      if(min(biology$Mat_len)<1){ # if length based
        if(!add) plot(x,biology$Mat_len,xlab=labels[1],ylab=labels[3],type="o",col=colvec[1])
        if(add) lines(x,biology$Mat_len,type="o",col=colvec[1])
      }else{ # else is age based
        if(!add){
          plot(growdatF$Age_Beg, growdatF$Age_Mat, xlab=labels[2],
               ylab=labels[3], type="o", col=colvec[1])
        }else{
          lines(growdatF$Age_Beg, growdatF$Age_Mat,
                type="o",col=colvec[1])
        }
      }
      if(!add) abline(h=0,col="grey")
    }else{
      #print(seas)
      # if empirical weight-at-age IS used
      fecmat <- wtatage[wtatage$fleet==-2 & wtatage$gender==1,]
      # figure out which seasons have fecundity values (maybe always only one?)
      fecseasons <- sort(unique(fecmat$seas))
      seas_label <- NULL
      for(iseas in fecseasons){
        fecmat_seas <- fecmat[fecmat$seas==iseas, -(2:6)]
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
    x <- biology$Mean_Size
    if(!add){
      plot(x,biology$Spawn,xlab=labels[1],ylab=labels[5],type="o",col=colvec[1])
      abline(h=0,col="grey")
    }else{
      lines(x,biology$Spawn,type="o",col=colvec[1])
    }
  }

  ymax <- max(biology$Mean_Size)
  x <- growdatF$Age_Beg
    
  main <- "Ending year expected growth (with 95% intervals)"
  # if(nseasons > 1){main <- paste(main," season 1",sep="")}
  col_index1 <- 3 # default is grey for single-sex model
  if(nsexes > 1){
    col_index1 <- 1 # change line for females to red
  }

  growth_curve_fn <- function(add_labels=TRUE, add_uncertainty=TRUE) # growth
  {
    x <- growdatF$Age_Beg
    # make empty plot unless this is being added to existing figure
    if(!add){
      plot(x, growdatF$Len_Beg, ylim=c(0,1.1*ymax), type="n",
           xlab="", ylab="", axes=FALSE, xaxs='i', yaxs='i')
      abline(h=0,col="grey")
      if(add_labels){
        title(main=main, xlab=labels[2], ylab=labels[6], cex.main=cex.main)
        axis(1)
        axis(2, las=1)
      }
    }
    lty <- 1
    polygon(c(x, rev(x)), c(growdatF$low, rev(growdatF$high)),
            border=NA, col=shadecolvec[col_index1])
    lines(x,growdatF$Len_Beg,col=colvec[col_index1],lwd=2,lty=1)
    lines(x,growdatF$high,col=colvec[col_index1],lwd=1,lty='12')
    lines(x,growdatF$low,col=colvec[col_index1],lwd=1,lty='12')
    # add uncertainty intervals around growth curve
    if(!is.null(Grow_std) & add_uncertainty){
      Grow_std.f <- Grow_std[Grow_std$sex==1,]
      if(!is.null(Grow_std.f)){
        for(irow in 1:nrow(Grow_std.f)){
          std.age <- Grow_std.f$age[irow]
          # this might not work for log-normal length at age
          mean <- growdatF$Len_Beg[growdatF$Age_Beg==std.age]
          age.mid <- growdatF$Age_Beg[growdatF$Age_Beg==std.age]
          high <- qnorm(0.975, mean=mean, sd=Grow_std.f$StdDev[irow])
          low  <- qnorm(0.025, mean=mean, sd=Grow_std.f$StdDev[irow])
          arrows(x0=age.mid, x1=age.mid,
                 y0=low,     y1=high,
                 length=0.04, angle=90, code=3, col=colvec[col_index1])
        }
      }
    }
    # add males if they are present in the model
    if(nsexes > 1){
      polygon(c(xm, rev(xm)), c(growdatM$low, rev(growdatM$high)),
              border=NA, col=shadecolvec[2])
      lines(xm,growdatM$Len_Beg,col=colvec[2],lwd=2,lty=2)
      lines(xm,growdatM$high,col=colvec[2],lwd=1,lty='13')
      lines(xm,growdatM$low,col=colvec[2],lwd=1,lty='13')
      # add uncertainty intervals around growth curve for males
      if(!is.null(Grow_std) & add_uncertainty){
        Grow_std.m <- Grow_std[Grow_std$sex==2,]
        if(!is.null(Grow_std.m)){
          for(irow in 1:nrow(Grow_std.m)){
            std.age <- Grow_std.m$age[irow]
            # this might not work for long-normal length at age
            mean <- growdatM$Len_Beg[growdatM$Age_Beg==std.age]
            age.mid <- 0.2 + growdatM$Age_Beg[growdatM$Age_Beg==std.age]
            high <- qnorm(0.975, mean=mean, sd=Grow_std.m$StdDev[irow])
            low  <- qnorm(0.025, mean=mean, sd=Grow_std.m$StdDev[irow])
            arrows(x0=age.mid, x1=age.mid,
                   y0=low,     y1=high,
                   length=0.04, angle=90, code=3, col=colvec[2])
          }
        }
      }
    }
    if(!add){
      grid()
      box()
    }
    if(nsexes > 1){
      legend(legendloc,bty="n", c("Females","Males"), lty=c(1,2), lwd=2,
             col=c(colvec[1],colvec[2]))
    }
  }
  if(plot & 1 %in% subplots) growth_curve_fn()
  if(print & 1 %in% subplots){
    file <- paste(plotdir,"/bio1_sizeatage.png",sep="")
    caption <- paste("Length at age in the beginning of the year (or season) in the ending",
                     "year of the model. Shaded area indicates 95% distribution of",
                     "length at age around estimated growth curve.")
    if(!is.null(Grow_std)){
      caption <- paste(caption,
                       "Vertical intervals around growth curve indicate",
                       "estimated 95% uncertainty intervals in estimated mean growth.")
    }
    plotinfo <- pngfun(file=file, caption=caption)
    growth_curve_fn()
    dev.off()
    #plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
  }


  growth_curve_plus_fn <- function(add_labels=TRUE, option=1){
    # function to add panels to growth curve with info on variability in
    # length at age or info on maturity, weight, and fecundity

    # save current parameter settings
    par_old <- par()

    # create 4-panel layout
    layout(mat=matrix(1:4,byrow=TRUE,ncol=2),widths=c(2,1),heights=c(2,1))
    par(mar=c(1,1,1,1),oma=c(5,5,5,4))
    # plot growth curve
    growth_curve_fn(add_labels=FALSE)
    axis(2, las=1)
    # add age axis on top
    axis(3)
    # add some labels
    mtext(side=2, line=2.5, labels[6], las=0)
    mtext(side=3, line=2.5, labels[2], las=0)
    box()

    if(option==1){
      lab1 <- "SD_Beg"
      lab1long <- "SD of lengths"
      lab2 <- "CV_Beg"
      lab2long <- "CV of lengths"
      lab1max <- 1.1*max(growdat[[lab1]])
      lab2max <- 1.05*max(growdat[[lab2]])
      # temporary stuff for growth workshop
      if(exists("lab1max",where=1)){
        lab1max <- get("lab1max",pos=1)
      }
      if(exists("lab2max",where=1)){
        lab2max <- get("lab2max",pos=1)
      }
      # end temporary stuff
      lab1_axis_vec <- NULL
    }
    if(option==2){
      lab1 <- "Len_Mat"
      lab1long <- "Frac. mature"
      lab2 <- "Wt_Beg"
      lab2long <- "Mean weight"
      lab1max <- 1
      lab2max <- max(c(biology$Wt_len_F, biology$Wt_len_M))
      lab1_axis_vec <- c(0, 0.5, 1)
    }
    # calculate scaling factor between CVs and SDs to share each panel
    lab2_to_lab1_scale <- lab1max/lab2max
    lab2_axis_vec <- pretty(c(0,lab2max))
    # make empty panel in top-right location
    plot(0, type='n',
         xaxs='i', xlim=c(0,1.0*lab1max),
         yaxs='i', ylim=par()$usr[3:4],
         axes=FALSE)
    if(option==1){
      # if plotting SD and CV, then get it from age-based data
      # add line for lab1 vs. Len
      lines(growdatF[[lab1]], growdatF$Len_Beg, col=colvec[col_index1],
            lwd=1, lty='12')
      # add line for lab2 vs. Len
      lines(growdatF[[lab2]]*lab2_to_lab1_scale, growdatF$Len_Beg,
            col=colvec[col_index1], lwd=3)
      if(nsexes > 1){ # add lines for males
        if(option==1){
          # add line for lab1 vs. Age (unless it's maturity, which is not defined for males)
          lines(growdatM[[lab1]], growdatM$Len_Beg, col=colvec[2],
                lwd=1, lty='13')
        }
        # add line for lab2 vs. Len
        lines(growdatM[[lab2]]*lab2_to_lab1_scale, growdatM$Len_Beg, col=colvec[2],
              lwd=3, lty=2)
      }
    }
    if(option==2){
      # if plotting maturity and fecundity, then get this panel from length-based data
      lines(biology$Wt_len_F*lab2_to_lab1_scale, biology$Mean_Size,
            col=colvec[col_index1], lwd=3)
      lines(biology$Mat_len, biology$Mean_Size, col=colvec[col_index1], lty='12')
      if(nsexes > 1){
        lines(biology$Wt_len_M*lab2_to_lab1_scale, biology$Mean_Size,
              col=colvec[2], lwd=3, lty=2)
      }
    }
    # add axes and labels
    mtext(side=4, line=2.5, labels[6], las=0)
    mtext(side=1, line=2.5, lab1long, las=0)
    mtext(side=3, line=2.5, lab2long, las=0)
    axis(1, at=lab1_axis_vec, las=1)
    axis(3, at=lab2_axis_vec*lab2_to_lab1_scale, labels=lab2_axis_vec, las=1)
    axis(4, las=1)
    box()


    # make empty plot in lower-left position
    plot(0, type='n',
         xaxs='i', xlim=range(growdat$Age_Beg),
         yaxs='i', ylim=c(0,1.0*lab1max),
         axes=FALSE)
    # add line for lab1 vs. Age
    lines(growdatF$Age_Beg, growdatF[[lab1]], col=colvec[col_index1],
          lwd=1, lty='12')
    # add line for lab2 vs. Age
    lines(growdatF$Age_Beg, growdatF[[lab2]]*lab2_to_lab1_scale, col=colvec[col_index1],
          lwd=3)
    if(nsexes > 1){ # add lines for males
      if(option==1){
        # add line for lab1 vs. Age (unless it's maturity, which is not defined for males)
        lines(growdatM$Age_Beg, growdatM[[lab1]], col=colvec[2],
              lwd=1, lty='13')
      }
      # add line for lab2 vs. Age
      lines(growdatM$Age_Beg, growdatM[[lab2]]*lab2_to_lab1_scale, col=colvec[2],
            lwd=3, lty=2)
    }
    # add axes and labels
    mtext(side=1, line=2.5, labels[2], las=0)
    mtext(side=4, line=2.5, lab1long, las=0)
    mtext(side=2, line=2.5, lab2long, las=0)
    axis(1, las=1)
    axis(2, at=lab2_axis_vec*lab2_to_lab1_scale, labels=lab2_axis_vec, las=1)
    axis(4, at=lab1_axis_vec, las=1)
    box()

    # restore default single panel settings
    par(mfcol=par_old$mfcol, mar=par_old$mar, oma=par_old$oma)
  }
  # make plots of growth curve with CV and SD of length
  if(plot & 2 %in% subplots){
    growth_curve_plus_fn(option=1)
  }
  if(print & 2 %in% subplots){
    file <- paste(plotdir,"/bio2_sizeatage_plus_CV_and_SD.png",sep="")
    caption <- paste("Length at age (top-left panel) with",
                     "CV (thick line) and SD (thin line) of",
                     "length at age shown in top-right and lower-left panels")
    plotinfo <- pngfun(file=file, caption=caption)
    growth_curve_plus_fn(option=1)
    dev.off()
    #plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
  }

  # make plots of growth curve with weight-length curve and maturity
  if(plot & 3 %in% subplots){
    growth_curve_plus_fn(option=2)
  }
  if(print & 3 %in% subplots){
    file <- paste(plotdir,"/bio3_sizeatage_plus_WT_and_MAT.png",sep="")
    caption <- paste("Length at age (top-left panel) with",
                     "weight (thick line) and maturity (thin line)",
                     "shown in top-right and lower-left panels")
    plotinfo <- pngfun(file=file, caption=caption)
    growth_curve_plus_fn(option=2)
    dev.off()
    #plotinfo <- rbind(plotinfo,data.frame(file=file,caption=caption))
  }


  # function for illustrating parameterization of growth curves
  growth_curve_labeled_fn <- function(option=1) # growth
  {
    if(is.null(Growth_Parameters)){
      cat("Need updated SS_output function to get Growth_Parameters output\n")
      return()
    }
    # save current parameter settings
    par_old <- par()
    par(mar=c(4,4,1,4))
    AminF <- Growth_Parameters$A1[1]
    AmaxF <- Growth_Parameters$A2[1]
    AminM <- Growth_Parameters$A1[2]
    AmaxM <- Growth_Parameters$A2[2]
    L_at_AminF <- Growth_Parameters$L_a_A1[1]
    L_at_AmaxF <- Growth_Parameters$L_a_A2[1]
    L_at_AminM <- Growth_Parameters$L_a_A1[2]
    L_at_AmaxM <- Growth_Parameters$L_a_A2[2]
    LinfF <- Growth_Parameters$Linf[1]
    LinfM <- Growth_Parameters$Linf[2]
    ymax <- max(biology$Mean_Size)
    plot(0, type="n",
         xlim=c(0,1+max(growdatF$Age_Beg)),
         ylim=c(0,1.1*ymax),
         xlab="", ylab="", axes=FALSE, xaxs='i', yaxs='i')
    abline(h=0,col="grey")
    #title(main=main, xlab=labels[2], ylab=labels[6], cex.main=cex.main)
    axis(1, at=c(0,AminF,AmaxF,accuage),
         labels=expression(0,italic(A[1]),italic(A[2]),italic(N[ages])))
    axis(2, at=c(0, L_at_AminF, L_at_AmaxF, LinfF),
         labels=expression(0, italic(L[A[1]]), italic(L[A[2]]),
           italic(L[infinity])), las=1)
    # add growth curve itself
    lines(growdatF$Age_Beg,growdatF$Len_Beg,
          col=1,lwd=3,lty=1)
    # add growth curve for males
    if(nsexes > 1 & option==2){
      lines(growdatM$Age_Beg,growdatM$Len_Beg,
            col=1,lwd=2,lty=2)
    }
    if(option==1){
      # lines connecting axes to curve
      lines(c(AminF,AminF,0),c(0,L_at_AminF,L_at_AminF),lty=3)
      lines(c(AmaxF,AmaxF,0),c(0,L_at_AmaxF,L_at_AmaxF),lty=3)
      abline(h=LinfF,lty=3)
    }
    if(option==2){
      # lines connecting two curves
      lines(c(0,AminF), c(L_at_AminF,L_at_AminF),lty=3)
      lines(c(0,AmaxF), c(L_at_AmaxF,L_at_AmaxF),lty=3)
      lines(c(accuage+10,AmaxM), c(L_at_AmaxM,L_at_AmaxM),
            lty=3)
      lines(c(accuage+10,AminM), c(L_at_AminM,L_at_AminM),
            lty=3)
      arrows(x0=AminF,      x1=AminF,
             y0=L_at_AminF, y1=L_at_AminM,
            lwd=3,col=3,length=0.1)
      arrows(x0=AmaxF,      x1=AmaxF,
             y0=L_at_AmaxF, y1=L_at_AmaxM,
            lwd=3,col=3,length=0.1)
      text(AminF, mean(c(L_at_AminF,L_at_AminM)),
           "Male parameter 1\n(exponential offset)", col=3, adj=c(-.1,0))
      text(AmaxF, mean(c(L_at_AmaxF,L_at_AmaxM)),
           "Male parameter 2\n(exponential offset)", col=3, adj=c(-.1,0))
      axis(4, at=c(0, L_at_AminM, L_at_AmaxM),
           labels=expression(0, italic(L[A[1]]^male), italic(L[A[2]]^male)),
           las=1)
      abline(h=LinfF,lty=3)
    }
    box()
    # restore old parameter settings
    par(mfcol=par_old$mfcol, mar=par_old$mar, oma=par_old$oma)
  }
  if(plot & 101 %in% subplots) growth_curve_labeled_fn(option=1)
  if(print & 101 %in% subplots){
    file <- paste(plotdir,"/bio101_growth_illustration.png",sep="")
    caption <- "Illustration of growth parameters"
    plotinfo <- pngfun(file=file, caption=caption)
    growth_curve_labeled_fn(option=1)
    dev.off()
  }
  if(plot & 102 %in% subplots) growth_curve_labeled_fn(option=2)
  if(print & 102 %in% subplots){
    file <- paste(plotdir,"/bio102_growth_illustration2.png",sep="")
    caption <- "Illustration of growth parameters with male offsets"
    plotinfo <- pngfun(file=file, caption=caption)
    growth_curve_labeled_fn(option=2)
    dev.off()
  }


  # function for illustrating parameterization of CVs around growth curves
  CV_values_labeled_fn <- function(option=1) # growth
  {
    if(is.null(Growth_Parameters)){
      cat("Need updated SS_output function to get Growth_Parameters output\n")
      return()
    }
    # save current parameter settings
    par_old <- par()
    par(mar=c(4,4,1,4))
    AminF <- Growth_Parameters$A1[1]
    AmaxF <- Growth_Parameters$A2[1]
    AminM <- Growth_Parameters$A1[2]
    AmaxM <- Growth_Parameters$A2[2]
    CV_at_AminF <- Growth_Parameters$CVmin[1]
    CV_at_AmaxF <- Growth_Parameters$CVmax[1]
    CV_at_AminM <- Growth_Parameters$CVmin[2]
    CV_at_AmaxM <- Growth_Parameters$CVmax[2]
    ymax <- max(CV_at_AminF,CV_at_AmaxF,CV_at_AminM,CV_at_AmaxM)
    plot(0, type="n",
         xlim=c(0,1+max(growdatF$Age_Beg)),
         ylim=c(0,1.1*ymax),
         xlab="", ylab="", axes=FALSE, xaxs='i', yaxs='i')
    abline(h=0,col="grey")
    #title(main=main, xlab=labels[2], ylab=labels[6], cex.main=cex.main)
    axis(1, at=c(0,AminF,AmaxF,accuage),
         labels=expression(0,italic(A[1]),italic(A[2]),italic(N[ages])))
    axis(2, at=c(0, CV_at_AminF, CV_at_AmaxF),
         labels=expression(0, italic(CV[A[1]]), italic(CV[A[2]])), las=1)
    # add growth curve itself
    lines(growdatF$Age_Beg,growdatF$CV_Beg,
          col=1,lwd=3,lty=1)
    # add growth curve for males
    if(nsexes > 1 & option%in%c(2,4)){
      lines(growdatM$Age_Beg,growdatM$CV_Beg,
            col=1,lwd=2,lty=2)
    }
    if(option==1){
      # lines connecting axes to curve
      lines(c(AminF,AminF,0),c(0,CV_at_AminF,CV_at_AminF),lty=3)
      lines(c(AmaxF,AmaxF,0),c(0,CV_at_AmaxF,CV_at_AmaxF),lty=3)
    }
    if(option==2){
      # lines connecting two curves
      lines(c(0,AminF), c(CV_at_AminF,CV_at_AminF),lty=3)
      lines(c(0,AmaxF), c(CV_at_AmaxF,CV_at_AmaxF),lty=3)
      lines(c(accuage+10,AmaxM), c(CV_at_AmaxM,CV_at_AmaxM),
            lty=3)
      lines(c(accuage+10,AminM), c(CV_at_AminM,CV_at_AminM),
            lty=3)
      arrows(x0=AminF,      x1=AminF,
             y0=CV_at_AminF, y1=CV_at_AminM,
            lwd=3,col=3,length=0.1)
      arrows(x0=AmaxF,      x1=AmaxF,
             y0=CV_at_AmaxF, y1=CV_at_AmaxM,
            lwd=3,col=3,length=0.1)
      text(AminF, mean(c(CV_at_AminM)),
           "Male parameter 1\n(exponential offset)", col=3, adj=c(0,1.5))
      text(AmaxF, mean(c(CV_at_AmaxM)),
           "Male parameter 2\n(exponential offset)", col=3, adj=c(0,1.5))
      axis(4, at=c(0, CV_at_AminM, CV_at_AmaxM),
           labels=expression(0, italic(CV[A[1]]^male), italic(CV[A[2]]^male)),
           las=1)
    }
    if(option==3){
      # lines connecting axes to curve
      lines(c(AminF,AminF,0),c(0,CV_at_AminF,CV_at_AminF),lty=3)
      lines(c(AmaxF,AmaxF,0),c(0,CV_at_AmaxF,CV_at_AmaxF),lty=3)
      lines(c(0,AmaxF), c(CV_at_AminF,CV_at_AminF),lty=3)
      arrows(x0=AmaxF, x1=AmaxF,
             y0=CV_at_AminF, y1=CV_at_AmaxF,
             lwd=3, col=3, length=0.1)
      text(AmaxF, mean(c(CV_at_AminF,CV_at_AmaxF)),
           "Female parameter 2\n(exponential offset)", col=3, adj=c(-.1,0))
    }
    if(option==4){
      # lines connecting two curves
      arrows(x0=AmaxF, x1=AmaxF,
             y0=CV_at_AminF, y1=CV_at_AmaxF,
             lwd=3, col=3, length=0.1)
      text(AmaxF, mean(c(CV_at_AminF,CV_at_AmaxF)),
           "Female parameter 2\n(exponential offset)", col=3, adj=c(-.1,0))
      lines(c(0,AmaxF), c(CV_at_AminF,CV_at_AminF),lty=3)
      lines(c(0,AmaxF), c(CV_at_AmaxF,CV_at_AmaxF),lty=3)
      lines(c(accuage+10,AmaxM), c(CV_at_AmaxM,CV_at_AmaxM),
            lty=3)
      lines(c(accuage+10,AminM), c(CV_at_AminM,CV_at_AminM),
            lty=3)
      arrows(x0=AminF,      x1=AminF,
             y0=CV_at_AminF, y1=CV_at_AminM,
            lwd=3,col=3,length=0.1)
      arrows(x0=AmaxF,      x1=AmaxF,
             y0=CV_at_AminM, y1=CV_at_AmaxM,
            lwd=3,col=3,length=0.1)
      text(AminF, mean(c(CV_at_AminF,CV_at_AminM)),
           "Male parameter 1\n(exponential offset)", col=3, adj=c(0,2))
      text(AmaxF, mean(c(CV_at_AminM,CV_at_AmaxM)),
           "Male parameter 2\n(exponential offset)", col=3, adj=c(0,2))
      axis(4, at=c(0, CV_at_AminM, CV_at_AmaxM),
           labels=expression(0, italic(CV[A[1]]^male), italic(CV[A[2]]^male)),
           las=1)
    }
    box()
    # restore old parameter settings
    par(mfcol=par_old$mfcol, mar=par_old$mar, oma=par_old$oma)
  }
  if(plot & 103 %in% subplots) CV_values_labeled_fn(option=1)
  if(print & 103 %in% subplots){
    file <- paste(plotdir,"/bio103_CV_illustration.png",sep="")
    caption <- "Illustration of growth variability parameters"
    plotinfo <- pngfun(file=file, caption=caption)
    CV_values_labeled_fn(option=1)
    dev.off()
  }
  if(plot & 104 %in% subplots) CV_values_labeled_fn(option=2)
  if(print & 104 %in% subplots){
    file <- paste(plotdir,"/bio104_CV_illustration2.png",sep="")
    caption <- "Illustration of growth variability parameters with male offsets"
    plotinfo <- pngfun(file=file, caption=caption)
    CV_values_labeled_fn(option=2)
    dev.off()
  }
  if(plot & 105 %in% subplots) CV_values_labeled_fn(option=3)
  if(print & 105 %in% subplots){
    file <- paste(plotdir,"/bio105_CV_illustration.png",sep="")
    caption <- "Illustration of growth variability parameters for offset type 3"
    plotinfo <- pngfun(file=file, caption=caption)
    CV_values_labeled_fn(option=3)
    dev.off()
  }
  if(plot & 106 %in% subplots) CV_values_labeled_fn(option=4)
  if(print & 106 %in% subplots){
    file <- paste(plotdir,"/bio106_CV_illustration2.png",sep="")
    caption <- "Illustration of growth variability parameters with male offsets"
    plotinfo <- pngfun(file=file, caption=caption)
    CV_values_labeled_fn(option=4)
    dev.off()
  }


  x <- biology$Mean_Size

  if(plot){ # plot to screen or to PDF file
    if(4 %in% subplots) weight_plot()
    if(5 %in% subplots) maturity_plot()
    if(6 %in% subplots & FecType==1) gfunc3a()
    if(7 %in% subplots & fecundityOK) gfunc3b()
    if(8 %in% subplots & fecundityOK) gfunc3c()
    if(9 %in% subplots) gfunc4()
  }
  if(print){ # print to PNG files
    if(4 %in% subplots){
      file <- paste(plotdir,"/bio4_weightatsize.png",sep="")
      caption <- "Weight-length relationship"
      plotinfo <- pngfun(file=file, caption=caption)
      weight_plot()
      dev.off()
    }
    if(5 %in% subplots){
      file <- paste(plotdir,"/bio5_maturity.png",sep="")
      caption <- paste("Maturity at",ifelse(min(biology$Mat_len)<1,"length","age"))
      plotinfo <- pngfun(file=file, caption=caption)
      maturity_plot()
      dev.off()
    }
    if(6 %in% subplots & FecType==1){
      file <- paste(plotdir,"/bio6_fecundity.png",sep="")
      caption <- "Fecundity"
      plotinfo <- pngfun(file=file, caption=caption)
      gfunc3a()
      dev.off()
    }
    if(7 %in% subplots & fecundityOK){
      file <- paste(plotdir,"/bio7_fecundity_wt.png",sep="")
      caption <- "Fecundity as a function of weight"
      plotinfo <- pngfun(file=file, caption=caption)
      gfunc3b()
      dev.off()
    }
    if(8 %in% subplots & fecundityOK){
      file <- paste(plotdir,"/bio8_fecundity_len.png",sep="")
      caption <- "Fecundity as a function of length"
      plotinfo <- pngfun(file=file, caption=caption)
      gfunc3c()
      dev.off()
    }
    if(9 %in% subplots){
      file <- paste(plotdir,"/bio9_spawningoutput.png",sep="")
      caption <- "Spawning output at length"
      plotinfo <- pngfun(file=file, caption=caption)
      gfunc4()
      dev.off()
    }
  }

  # Natural mortality (if age-dependent -- need to add time-varying M plot)
  MatAge <- growdatF$M # female mortality in the ending year
  # not sure what role M2 is playing here
  M2 <- MGparmAdj[,c(1,grep("NatM",names(MGparmAdj)))]
  # not sure when you could have ncol(M2) = NULL
  if(!is.null(ncol(M2))){
    M2f <- M2[,c(1,grep("Fem",names(M2)))]
    if(min(MatAge)!=max(MatAge) & 11 %in% subplots){
      ymax <- max(MatAge)
      # function to plut natural mortality
      mfunc <- function(){
        if(!add){
          plot(growdatF$Age_Beg,MatAge,col=colvec[1],lwd=2,ylim=c(0,ymax),type="n",
               ylab=labels[7],xlab=labels[2])
          abline(h=0,col="grey")
        }
        lines(growdatF$Age_Beg, MatAge, col=colvec[1],lwd=2,type="o")
        if(nsexes > 1){
          growdatM <- growdat[growdat$Morph==mainmorphs[2],]
          lines(growdatM$Age_Beg,growdatM$M,col=colvec[2],lwd=2,type="o")
        }
      }
      # run function if requested to make plot
      if(plot & 11 %in% subplots){
        mfunc()
      }
      # run function if requested to write figure to PNG file
      if(print & 11 %in% subplots){
        file <- paste(plotdir,"/bio11_natmort.png",sep="")
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
            if(12 %in% subplots)
              persp(x,y,z,col="white",xlab=labels[2],ylab="",zlab=labels[1],expand=0.5,
                    box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",
                    phi=35,theta=-10)
            if(13 %in% subplots)
              contour(x,y,z,nlevels=12,xlab=labels[2],
                      main=main,cex.main=cex.main,col=ians_contour,lwd=2)}
          if(print){
            if(12 %in% subplots){
              file <- paste(plotdir,"/bio12_timevarygrowthsurf_sex",i,".png",sep="")
              caption <- "Perspective plot of time-varying growth"
              plotinfo <- pngfun(file=file, caption=caption)
              persp(x,y,z,col="white",xlab=labels[2],ylab="",zlab=labels[1],expand=0.5,
                    box=TRUE,main=main,cex.main=cex.main,ticktype="detailed",
                    phi=35,theta=-10)
              dev.off()
            }
            if(13 %in% subplots){
              file <- paste(plotdir,"/bio13_timevarygrowthcontour_sex",i,".png",sep="")
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
  if(14 %in% subplots){
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
            file <- paste(plotdir, "/bio14_time-varying_", parmlabel, ".png", sep="")
            # replace % sign which cause problems for filename
            file <- gsub(pattern="%", replacement="percent", x=file,
                         fixed=TRUE)
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
