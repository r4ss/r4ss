#' plot model comparisons
#' 
#' Creates a user-chosen set of plots comparing model output from a summary of
#' multiple models, where the collection was created using the
#' \code{SSsummarize} function.
#' 
#' 
#' @param summaryoutput List created by \code{SSsummarize}
#' @param subplots Vector of subplots to be created.
#' @param plot Plot to active plot device?
#' @param print Send plots to PNG files in directory specified by
#' \code{plotdir}?
#' @param png Has same result as \code{print}, included for consistency with
#' \code{SS_plots}.
#' @param pdf Write output to PDF file? Can't be used in conjunction with
#' \code{png} or \code{print}.
#' @param models Optional subset of the models described in
#' \code{summaryoutput}.  Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' @param endyrvec Optional single year or vector of years representing the
#' final year of values to show for each model. By default it is set to the
#' ending year specified in each model.
#' @param indexfleets Vector of fleets for each model for which to compare
#' indices of abundance. Only necessary if any model has more than one index.
#' @param indexUncertainty Show uncertainty intervals on index data?
#' Default=FALSE because if models have any extra standard deviations added,
#' these intervals may differ across models.
#' @param indexQlabel Add catchability to legend in plot of index fits
#' (TRUE/FALSE)?
#' @param indexQdigits Number of significant digits for catchability in legend
#' (if \code{indexQlabel=TRUE})
#' @param indexSEvec Optional replacement for the SE values in
#' summaryoutput$indices to deal with the issue of differing uncertainty by
#' models described above.
#' @param indexPlotEach TRUE plots the observed index for each model with
#' colors, or FALSE just plots observed once in black dots.
#' @param labels Vector of labels for plots (titles and axis labels)
#' @param col Optional vector of colors to be used for lines. Input 'default'
#' makes use of \code{rich.colors.short} function.
#' @param shadecol Optional vector of colors to be used for shading uncertainty
#' intervals. Input 'default' makes use of \code{rich.colors.short} function
#' with alpha transparency.
#' @param pch Optional vector of plot character values
#' @param lty Optional vector of line types
#' @param lwd Optional vector of line widths
#' @param spacepoints Number of years between points shown on top of lines (for
#' long timeseries, points every year get mashed together)
#' @param staggerpoints Number of years to stagger the first point (if
#' \code{spacepoints > 1}) for each line (so that adjacent lines have points in
#' different years)
#' @param xlim Optional x limits
#' @param ylimAdj Multiplier for ylim parameter. Allows additional white space
#' to fit legend if necessary. Default=1.
#' @param xaxs Choice of xaxs parameter (see ?par for more info)
#' @param yaxs Choice of yaxs parameter (see ?par for more info)
#' @param type Type parameter passed to points (default 'o' overplots points on
#' top of lines)
#' @param uncertainty Show plots with uncertainty intervals
#' @param shadealpha Transparency parameter used to make default shadecol
#' values (see ?rgb for more info)
#' @param legend Add a legend?
#' @param legendlabels Optional vector of labels to include in legend. Default
#' is 'model1','model2',etc.
#' @param legendloc Location of legend. See ?legend for more info.
#' @param legendorder Optional vector of model numbers that can be used to have
#' the legend display the model names in an order that is different than that
#' which is represented in the summary input object.
#' @param legendncol Number of columns for the legend.
#' @param btarg Target biomass value at which to show a line (set to 0 to
#' remove)
#' @param minbthresh Minimum biomass threshhold at which to show a line (set to
#' 0 to remove)
#' @param sprtarg Target value for SPR-ratio where line is drawn in the SPR
#' plots and phase plot.
#' @param pwidth Width of plot written to PNG file
#' @param pheight Height of plot written to PNG file
#' @param punits Units for PNG file
#' @param res Resolution for PNG file
#' @param ptsize Point size for PNG file
#' @param cex.main Character expansion for plot titles
#' @param plotdir Directory where PNG or PDF files will be written. By default
#' it will be the directory where the model was run.
#' @param densitynames Vector of names (or subset of names) of parameters or
#' derived quantities contained in summaryoutput$pars$Label or
#' summaryoutput$quants$Label for which to make density plots
#' @param densityxlabs Optional vector of x-axis labels to use in the density
#' plots (must be equal in length to the printed vector of quantities that
#' match the \code{densitynames} input)
#' @param densityscalex Scalar for upper x-limit in density plots (values below
#' 1 will cut off the right tail to provide better contrast among narrower
#' distributions
#' @param densityscaley Scalar for upper y-limit in density plots (values below
#' 1 will cut off top of highest peaks to provide better contrast among broader
#' distributions
#' @param densityadjust Multiplier on bandwidth of kernel in density function
#' used for smoothing MCMC posteriors. See 'adjust' in ?density for details.
#' @param densitysymbols Add symbols along lines in density plots. Quantiles
#' are \code{c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)}.
#' @param densitytails Shade tails outside of 95\% interval in density plots.
#' @param densitylwd Line width for density plots
#' @param fix0 Always include 0 in the density plots?
#' @param new Create new empty plot window
#' @param add Allows single plot to be added to existing figure. This needs to
#' be combined with specific 'subplots' input to make sure only one thing gets
#' added.
#' @param par list of graphics parameter values passed to the \code{par}
#' function
#' @param verbose Report progress to R GUI?
#' @param mcmcVec Vector of TRUE/FALSE values (or single value) indicating
#' whether input values are from MCMC or to use normal distribution around MLE
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}, \code{\link{SSsummarize}},
#' \code{\link{SS_output}}, \code{\link{SSgetoutput}}
#' @keywords hplot
SSplotComparisons <-
  function(summaryoutput,subplots=1:20,
           plot=TRUE,print=FALSE,png=print,pdf=FALSE,
           models="all",
           endyrvec="default",
           indexfleets=NULL,
           indexUncertainty=FALSE,
           indexQlabel=TRUE,
           indexQdigits=4,
           indexSEvec="default",
           indexPlotEach=FALSE,         #TRUE plots the observed index for each model with colors, or FALSE just plots observed once in black dots
           labels=c("Year",             #1
             "Spawning biomass (mt)",   #2
             "Spawning depletion",      #3
             "Age-0 recruits (1,000s)", #4
             "Recruitment deviations",  #5
             "Index",                   #6
             "Log index",               #7
             "SPR ratio",               #8 could be dynamic to match model value (e.g. "(1-SPR)/(1-SPR_40%)")
             "Density",                 #9
             "Management target",       #10
             "Minimum stock size threshold", #11 
             "Spawning output (eggs)",  #12
             "Harvest rate"),           #13
           col="default", shadecol="default",
           pch="default", lty=1, lwd=2,
           spacepoints=10,
           staggerpoints=1,
           xlim="default", ylimAdj=1,
           xaxs="r", yaxs="r",
           type="o", uncertainty=TRUE, shadealpha=0.1,
           legend=TRUE, legendlabels="default", legendloc="topright",
           legendorder="default",legendncol=1,
           sprtarg=NULL, btarg=NULL, minbthresh=NULL,
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           plotdir=NULL,
           densitynames=c("SPB_Virgin","R0"),
           densityxlabs="default",
           densityscalex=1,
           densityscaley=1,
           densityadjust=1,
           densitysymbols=TRUE,
           densitytails=TRUE,
           densitylwd=1,
           fix0=TRUE,
           new=TRUE,
           add=FALSE,
           par=list(mar=c(5,4,1,1)+.1),
           verbose=TRUE,
           mcmcVec="default")
{
  # subfunction to write png files
  pngfun <- function(file){
    png(filename=paste(plotdir,file,sep="/"),
        width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
    par(par)
  }
  
  if(png) print <- TRUE
  if(png & is.null(plotdir))
    stop("to print PNG files, you must supply a directory as 'plotdir'")

  # check for internal consistency
  if(pdf & png){
    stop("To use 'pdf', set 'print' or 'png' to FALSE.")
  }
  if(pdf){
    if(is.null(plotdir)) stop("to write to a PDF, you must supply a directory as 'plotdir'")
    pdffile <- paste(plotdir,"/SSplotComparisons_",format(Sys.time(),'%d-%b-%Y_%H.%M' ),".pdf",sep="")
    pdf(file=pdffile,width=pwidth,height=pheight)
    if(verbose) cat("PDF file with plots will be:",pdffile,'\n')
    par(par)
  }
  
  # subfunction to add legend
  legendfun <- function(legendlabels,cumulative=FALSE) {
    if(cumulative) legendloc="topleft"
    legend(legendloc, legend=legendlabels[legendorder],
           col=col[legendorder], lty=lty[legendorder],seg.len = 3,
           lwd=lwd[legendorder], pch=pch[legendorder], bty="n", ncol=legendncol)
  }
  
  rc <- function(n,alpha=1){
    # a subset of rich.colors by Arni Magnusson from the gregmisc package
    # a.k.a. rich.colors.short, but put directly in this function
    # to try to diagnose problem with transparency on one computer
    x <- seq(0, 1, length = n)
    r <- 1/(1 + exp(20 - 35 * x))
    g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
    b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
    rgb.m <- matrix(c(r, g, b), ncol = 3)
    rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha=alpha))
  }
  
  # get stuff from summary output
  n             <- summaryoutput$n
  nsexes        <- summaryoutput$nsexes
  startyrs      <- summaryoutput$startyrs
  endyrs        <- summaryoutput$endyrs
  pars          <- summaryoutput$pars
  parsSD        <- summaryoutput$parsSD
  parphases     <- summaryoutput$parphases
  quants        <- summaryoutput$quants
  quantsSD      <- summaryoutput$quantsSD
  SpawnBio      <- summaryoutput$SpawnBio
  SpawnBioLower <- summaryoutput$SpawnBioLower
  SpawnBioUpper <- summaryoutput$SpawnBioUpper
  Bratio        <- summaryoutput$Bratio
  BratioLower   <- summaryoutput$BratioLower
  BratioUpper   <- summaryoutput$BratioUpper
  SPRratio      <- summaryoutput$SPRratio
  SPRratioLower <- summaryoutput$SPRratioLower
  SPRratioUpper <- summaryoutput$SPRratioUpper
  recruits      <- summaryoutput$recruits
  recruitsLower <- summaryoutput$recruitsLower
  recruitsUpper <- summaryoutput$recruitsUpper
  recdevs       <- summaryoutput$recdevs
  recdevsLower  <- summaryoutput$recdevsLower
  recdevsUpper  <- summaryoutput$recdevsUpper
  indices       <- summaryoutput$indices
  mcmc          <- summaryoutput$mcmc               #a list of dataframes, 1 for each model with mcmc output
  lowerCI       <- summaryoutput$lowerCI
  upperCI       <- summaryoutput$upperCI
  SpawnOutputUnits <- summaryoutput$SpawnOutputUnits
  btargs         <- summaryoutput$btargs
  minbthreshs    <- summaryoutput$minbthreshs
  sprtargs       <- summaryoutput$sprtargs
  SPRratioLabels <- summaryoutput$SPRratioLabels

  # checking for the same reference points across models
  if(is.null(btarg)) {
    btarg <- unique(btargs)
    if(length(btarg)>1){
      cat("setting btarg = -999 because models don't have matching values\n")
      btarg <- -999
    }
  }
  if(is.null(minbthresh)) {
    minbthresh <- unique(minbthreshs)
    if(length(minbthresh)>1){
      cat("setting minbthresh = -999 because models don't have matching values\n")
      minbthresh <- -999
    }
  }
  if(is.null(sprtarg)) {
    sprtarg <- unique(sprtargs)
    if(length(sprtarg)>1){
      cat("setting sprtarg = -999 because models don't have matching values\n")
      sprtarg <- -999
    }
  }
  SPRratioLabel <- unique(SPRratioLabels)
  if(length(SPRratioLabel)>1){
    cat("setting SPRratioLabel = NA because models don't have matching labels\n")
    SPRratioLabel <- NA
  }
  
  if(uncertainty){
    if(all(is.na(quantsSD[,1:n]) | quantsSD[,1:n]==0)){
      cat("setting uncertainty to FALSE because no uncertainty includes in the model results\n")
      uncertainty <- FALSE
    }
  }
  # fix biomass for single-sex models
  if(any(nsexes==1)){
    if(verbose) cat("dividing SpawnBio by 2 for single-sex models:",(1:n)[nsexes==1],"\n")
    for(i in (1:n)[nsexes==1]){
      SpawnBio[,i]    <- SpawnBio[,i]/2
      SpawnBioLower[,i]  <- SpawnBioLower[,i]/2
      SpawnBioUpper[,i]  <- SpawnBioUpper[,i]/2
    }
  }

  # check number of models to be plotted
  if(models[1]=="all") models <- 1:n
  nlines <- length(models)
  if(mcmcVec[1]=="default") mcmcVec <- rep(FALSE,nlines)
  if(length(models)!=length(mcmcVec)) cat("WARNING: the number of models is not equal to the number of mcmcVec elements\n")

  if(!is.null(indexfleets) && length(indexfleets) < n){
    if(length(indexfleets)==1){
      indexfleets <- rep(indexfleets, n)
    }else{
      warning("'indexfleets' needs to have length either 1 or n=",n)
      indexfleets <- NULL
    }
  }
  # setup colors, points, and line types
  if(col[1]=="default" & nlines>3) col <- rc(nlines+1)[-1]
  if(col[1]=="default" & nlines<3) col <- rc(nlines)
  if(col[1]=="default" & nlines==3) col <- c("blue","red","green3")
  if(shadecol[1]=="default" & nlines>3) shadecol <- rc(nlines+1,alpha=shadealpha)[-1]
  if(shadecol[1]=="default" & nlines<3) shadecol <- rc(nlines,alpha=shadealpha)
  if(shadecol[1]=="default" & nlines==3) shadecol <- rgb(red=c(0,1,0),green=c(0,0,0.8),blue=c(1,0,0),alpha=shadealpha)

  if(pch[1]=="default") pch <- rep(1:25,10)[1:nlines]
  if(lty[1]=="default") lty <- 1:nlines

  if(length(col) < nlines) col <- rep(col,nlines)[1:nlines]
  if(length(pch) < nlines) pch <- rep(pch,nlines)[1:nlines]
  if(length(lty) < nlines) lty <- rep(lty,nlines)[1:nlines]
  if(length(lwd) < nlines) lwd <- rep(lwd,nlines)[1:nlines]

  if(!is.expression(legendlabels[1]) &&
     legendlabels[1]=="default") legendlabels <- paste("model",1:nlines)
  if(legendorder[1]=="default") legendorder <- 1:nlines

  # open new window if requested
  if(plot & new & !pdf){
    ### Note: the following line has been commented out because it was identified
    ###       by Brian Ripley as "against CRAN policies".
    #if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
    par(par)
  }

  # get MCMC results if requested
  for(iline in (1:nlines)[mcmcVec]){
    imodel <- models[iline]

    # reset values to NA for mcmc columns only
    cols <- imodel
    SpawnBioLower[,cols] <- SpawnBioUpper[,cols] <- SpawnBio[,cols] <- NA
    BratioLower[,cols] <- BratioUpper[,cols] <- Bratio[,cols] <- NA
    SPRratioLower[,cols] <- SPRratioUpper[,cols] <- SPRratio[,cols] <- NA
    recruitsLower[,cols] <- recruitsUpper[,cols] <- recruits[,cols] <- NA
    recdevsLower[,cols] <- recdevsUpper[,cols] <- recdevs[,cols] <- NA

    ### get MCMC for SpawnBio
    tmp <- grep("SPB",names(mcmc[[imodel]]))   #try it to see what you get
    if(length(tmp) > 0) {   #there are some mcmc values to use
      mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
      mcmclabs <- names(mcmc.tmp)
      lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
      med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
      upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
      if(nsexes[iline] == 1) {
        lower <- lower/2
        upper <- upper/2
        med <- med/2
      }
      SpawnBio[,imodel] <- med[match(SpawnBio$Label,mcmclabs)]
      SpawnBioLower[,imodel] <- lower[match(SpawnBioLower$Label,mcmclabs)]
      SpawnBioUpper[,imodel] <- upper[match(SpawnBioUpper$Label,mcmclabs)]
    }

    ### get MCMC for Bratio
    #for(iline in (1:nlines)[mcmcVec]){
    #  imodel <- models[iline]
      # get values from mcmc to replace
      tmp <- grep("Bratio",names(mcmc[[imodel]]))   #try it to see what you get
      if(length(tmp) > 0) {   #there are some mcmc values to use
        mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
        mcmclabs <- names(mcmc.tmp)
        lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
        med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
        upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
        Bratio[,imodel] <- med[match(Bratio$Label,mcmclabs)]
        BratioLower[,imodel] <- lower[match(BratioLower$Label,mcmclabs)]
        BratioUpper[,imodel] <- upper[match(BratioUpper$Label,mcmclabs)]
      }
    #}

    ### get MCMC for SPRratio
    #for(iline in (1:nlines)[mcmcVec]){
    #  imodel <- models[iline]
      # get values from mcmc to replace
      tmp <- grep("SPRratio",names(mcmc[[imodel]]))   #try it to see what you get
      if(length(tmp) > 0) {   #there are some mcmc values to use
        mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
        mcmclabs <- names(mcmc.tmp)
        lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
        med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
        upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
        SPRratio[,imodel] <- med[match(SPRratio$Label,mcmclabs)]
        SPRratioLower[,imodel] <- lower[match(SPRratioLower$Label,mcmclabs)]
        SPRratioUpper[,imodel] <- upper[match(SPRratioUpper$Label,mcmclabs)]
      }
    #}
  
    ### get MCMC for recruits
    # get values from mcmc to replace
    #for(iline in (1:nlines)[mcmcVec]){
    #  imodel <- models[iline]
      tmp <- grep("^Recr_",names(mcmc[[imodel]]))   #try it to see what you get
      tmp2 <- grep("Recr_Unfished",names(mcmc[[imodel]]))
      tmp <- setdiff(tmp,tmp2)
      if(length(tmp) > 0) { #there are some mcmc values to use
        mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
        mcmclabs <- names(mcmc.tmp)
        lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
        med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
        mean  <- apply(mcmc.tmp,2,mean)   #mean recruitment should be more comparable
        upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
        cat("note: using mean recruitment from MCMC instead of median, because it is more comparable to MLE\n")
        #recruits[,imodel] <- med[match(recruits$Label,mcmclabs)]
        recruits[,imodel] <- mean[match(recruits$Label,mcmclabs)]
        recruitsLower[,imodel] <- lower[match(recruitsLower$Label,mcmclabs)]
        recruitsUpper[,imodel] <- upper[match(recruitsUpper$Label,mcmclabs)]
      }
    #}

    ### get MCMC for recdevs
    #for(iline in (1:nlines)[mcmcVec]){
    #  imodel <- models[iline]
      # get values from mcmc to replace
      tmp <- unique(c(grep("_RecrDev_",names(mcmc[[imodel]])),
                      grep("_InitAge_",names(mcmc[[imodel]])),
                      grep("ForeRecr_",names(mcmc[[imodel]]))))
      if(length(tmp) > 0) { #there are some mcmc values to use
        mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
        mcmclabs <- names(mcmc.tmp)
        lower <- apply(mcmc.tmp,2,quantile,prob=lowerCI)   #hard-wired probability
        med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
        upper <- apply(mcmc.tmp,2,quantile,prob=upperCI)   #hard-wired probability
        recdevs[,imodel] <- med[match(recdevs$Label,mcmclabs)]
        recdevsLower[,imodel] <- lower[match(recdevsLower$Label,mcmclabs)]
        recdevsUpper[,imodel] <- upper[match(recdevsUpper$Label,mcmclabs)]
      }
    #}
  }

  if(endyrvec[1]=="default") endyrvec <- endyrs
  if(length(endyrvec)==1) endyrvec <- rep(endyrvec,nlines)
  if(!is.null(endyrvec)){
    for(iline in 1:nlines){
      endyr <- endyrvec[iline]
      imodel <- models[iline]
      SpawnBio[SpawnBio$Yr > endyr, imodel] <- NA
      SpawnBioLower[SpawnBio$Yr > endyr, imodel] <- NA
      SpawnBioUpper[SpawnBio$Yr > endyr, imodel] <- NA
      Bratio[Bratio$Yr > endyr, imodel] <- NA
      BratioLower[Bratio$Yr > endyr, imodel] <- NA
      BratioUpper[Bratio$Yr > endyr, imodel] <- NA
      SPRratio[SPRratio$Yr >= endyr, imodel] <- NA
      SPRratioLower[SPRratio$Yr >= endyr, imodel] <- NA
      SPRratioUpper[SPRratio$Yr >= endyr, imodel] <- NA
      recruits[recruits$Yr > endyr, imodel] <- NA
      recruitsLower[recruits$Yr > endyr, imodel] <- NA
      recruitsUpper[recruits$Yr > endyr, imodel] <- NA
      recdevs[recdevs$Yr > endyr, imodel] <- NA
      recdevsLower[recdevs$Yr > endyr, imodel] <- NA
      recdevsUpper[recdevs$Yr > endyr, imodel] <- NA
    }
  }
  
  
  addpoly <- function(yrvec, lower, upper){ # add shaded uncertainty intervals behind line
    lower[lower<0] <- 0 # max of value or 0
    for(iline in 1:nlines){
      imodel <- models[iline]
      good <- !is.na(lower[,imodel]) & !is.na(upper[,imodel])
      polygon(x=c(yrvec[good],rev(yrvec[good])),
              y=c(lower[good,imodel],rev(upper[good,imodel])),
              border=NA,col=shadecol[iline])
      lines(yrvec[good],lower[good,imodel],lty=3,col=col[iline])
      lines(yrvec[good],upper[good,imodel],lty=3,col=col[iline])
    }
  }

  equ <- -(1:2)
  
  plotSpawnBio <- function(uncertainty=TRUE){ # plot spawning biomass
    # get axis limits
    if(xlim[1]=="default"){
      xlim <- range(SpawnBio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    ylim <- ylimAdj*range(0, SpawnBio[,models], na.rm=TRUE)
    if(uncertainty) ylim <- range(ylim, SpawnBioUpper[,models], na.rm=TRUE)

    # set units on spawning biomass plot
    if(length(unique(SpawnOutputUnits))!=1)
      cat("Warning, some models may have different units for spawning output than others!\n")
    if(any(SpawnOutputUnits=="numbers")){
      ylab <- labels[12] # numbers
    }else{
      ylab <- labels[2] # biomass
    }

    # do some scaling of y-axis
    yunits <- 1
    if(ylim[2] > 1e3 & ylim[2] < 1e6){
      yunits <- 1e3
      ylab <- gsub("mt","x1000 mt",ylab)
      ylab <- gsub("eggs","x1000 eggs",ylab)
    }
    if(ylim[2] > 1e6){
      yunits <- 1e6
      ylab <- gsub("mt","million mt",ylab)
      ylab <- gsub("eggs","millions of eggs",ylab)
    }
    if(ylim[2] > 1e9){
      yunits <- 1e9
      ylab <- gsub("million","billion",ylab)
    }
    if(!add) plot(0,type="n",xlim=xlim,ylim=ylim,xlab=labels[1],ylab=ylab,xaxs=xaxs,yaxs=yaxs,axes=FALSE)
    if(uncertainty){
      # add shading for undertainty
      addpoly(yrvec=SpawnBio$Yr[-(1:2)], lower=SpawnBioLower[-(1:2),], upper=SpawnBioUpper[-(1:2),])
      xEqu <- SpawnBio$Yr[2] - (1:nlines)/nlines # equilibrium spawning biomass year by model
    }else{
      xEqu <- rep(SpawnBio$Yr[2], nlines)  # equilibrium spawning biomass year by model
    }
    # draw points and lines
    if(spacepoints %in% c(0,1,FALSE) ){ # don't spread out points
      matplot(SpawnBio$Yr[-(1:2)], SpawnBio[-(1:2), models],
              col=col,pch=pch,lty=lty,lwd=lwd,type=type,add=TRUE)
    }else{ # spread out points with interval equal to spacepoints and staggering equal to staggerpoints
      matplot(SpawnBio$Yr[-(1:2)], SpawnBio[-(1:2), models],
              col=col,lty=lty,lwd=lwd,type="l",add=TRUE)
      SpawnBio2 <- SpawnBio
      for(iline in 1:nlines){
        imodel <- models[iline]
        SpawnBio2[SpawnBio2$Yr%%spacepoints != (staggerpoints*iline)%%spacepoints, imodel] <- NA
      }
      matplot(SpawnBio2$Yr[-(1:2)], SpawnBio2[-(1:2), models],
              col=col,pch=pch,lwd=lwd,type="p",add=TRUE)
    }
    # add arrows for equilibrium values
    old_warn <- options()$warn      # previous setting
    options(warn=-1)                # turn off "zero-length arrow" warning
    if(uncertainty) arrows(x0=xEqu, y0=as.numeric(SpawnBioLower[1,models]),
                           x1=xEqu, y1=as.numeric(SpawnBioUpper[1,models]),
                           length=0.01, angle=90, code=3, col=col)
    options(warn=old_warn)  #returning to old value
    # add points at equilibrium values
    points(x=xEqu, SpawnBio[1, models], col=col, pch=pch, cex=1.2, lwd=lwd)
    abline(h=0,col="grey")
    if(legend) legendfun(legendlabels)

    # add axes
    if(!add) axis(1)
    yticks <- pretty(ylim)
    if(!add) axis(2,at=yticks,labels=format(yticks/yunits),las=1)
    box()
  }

  plotBratio <- function(uncertainty=TRUE){ # plot biomass ratio (may be identical to previous plot)
    # get axis limits
    if(xlim[1]=="default"){
      xlim <- range(Bratio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    ylim <- ylimAdj*range(0, Bratio[,models], na.rm=TRUE)
    if(uncertainty) ylim <- ylimAdj*range(ylim, BratioUpper[,models], na.rm=TRUE)

    # make plot
    if(!add) plot(0,type="n",xlim=xlim,ylim=ylim,xlab=labels[1],ylab=labels[3],
                  xaxs=xaxs,yaxs=yaxs,axes=FALSE)
    if(uncertainty) addpoly(Bratio$Yr, lower=BratioLower, upper=BratioUpper)

    if(spacepoints %in% c(0,1,FALSE) ){ # don't spread out points
      matplot(Bratio$Yr,Bratio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,add=TRUE)
    }else{ # spread out points with interval equal to spacepoints and staggering equal to staggerpoints
      matplot(Bratio$Yr,Bratio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type="l",add=TRUE)
      if(type!="l"){
        Bratio2 <- Bratio
        for(iline in 1:nlines){
          imodel <- models[iline]
          Bratio2[Bratio2$Yr%%spacepoints != (staggerpoints*iline)%%spacepoints, imodel] <- NA
        }
        matplot(Bratio2$Yr,Bratio2[,models],col=col,pch=pch,lty=lty,lwd=lwd,type="p",add=TRUE)
      }
    }

    abline(h=0,col="grey")
    abline(h=1,col="grey",lty=2)

    yticks <- pretty(par()$yaxp[1:2])
    if(btarg>0){
      abline(h=btarg,col="red",lty=2)
      text(min(Bratio$Yr)+4,btarg+0.03,labels[10],adj=0)
      yticks <- sort(c(btarg,yticks))
    }
    if(minbthresh>0){
      abline(h=minbthresh,col="red",lty=2)
      text(min(Bratio$Yr)+4,minbthresh+0.03,labels[11],adj=0)
      yticks <- sort(c(minbthresh,yticks))
    }
    if(!add){
      axis(1)
      axis(2,at=yticks, las=1)
    }
    if(legend) legendfun(legendlabels)
    box()
  }

  plotSPRratio <- function(uncertainty=TRUE){ # plot biomass ratio (may be identical to previous plot)
    # get axis limits
    if(xlim[1]=="default"){
      xlim <- range(SPRratio$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    ylim <- ylimAdj*range(0, SPRratio[,models], na.rm=TRUE)
    if(uncertainty) ylim <- ylimAdj*range(ylim, SPRratioUpper[,models], na.rm=TRUE)

    # make plot
    if(!add){
      newmar <- oldmar <- par()$mar
      newmar[4] <- newmar[2]
      par(mar=newmar)
      plot(0,type="n",xlim=xlim,ylim=ylim,xlab=labels[1],
           ylab="" ,xaxs=xaxs,yaxs=yaxs,las=1)
    }
    if(uncertainty) addpoly(SPRratio$Yr, lower=SPRratioLower, upper=SPRratioUpper)
    if(spacepoints %in% c(0,1,FALSE) ){ # don't spread out points
      matplot(SPRratio$Yr,SPRratio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,add=TRUE)
    }else{ # spread out points with interval equal to spacepoints and staggering equal to staggerpoints
      matplot(SPRratio$Yr,SPRratio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type="l",add=TRUE)
      if(type!="l"){
        SPRratio2 <- SPRratio
        for(iline in 1:nlines){
          imodel <- models[iline]
          SPRratio2[SPRratio2$Yr%%spacepoints != (staggerpoints*iline)%%spacepoints, imodel] <- NA
        }
        matplot(SPRratio2$Yr,SPRratio2[,models],col=col,pch=pch,lty=lty,lwd=lwd,type="p",add=TRUE)
      }
    }
    abline(h=0,col="grey")
    if(sprtarg>0){
      if(sprtarg==1){
        # draw line at ratio = 1
        abline(h=sprtarg,col="red",lty=2)
        text(SPRratio$Yr[1]+4,(sprtarg+0.03),labels[10],adj=0)
        mtext(side=2,line=3,labels[8])
      }else{
        # draw line at sprtarg
        yticks <- pretty(ylim)
        if(!is.na(SPRratioLabels) &&
           SPRratioLabel==paste("(1-SPR)/(1-SPR_",round(100*sprtarg),"%)",sep="")){
          abline(h=1,col="red",lty=2)
          text(SPRratio$Yr[1]+4,1+0.03,labels[10],adj=0)
          axis(4,at=yticks,labels=yticks*(1-sprtarg),las=1)
          mtext(side=4,line=3,"1 - SPR")
          mtext(side=2,line=3,SPRratioLabel)
        }
      }
    }else{
      mtext(side=2,line=3,SPRratioLabel)
    }      
    if(legend) legendfun(legendlabels)
    if(exists("oldmar")) par(mar=oldmar)
  }

  ## plotF <- function(uncertainty=TRUE){ # plot biomass ratio (may be identical to previous plot)
  ##   # get axis limits
  ##   if(xlim[1]=="default"){
  ##     xlim <- range(SPRratio$Yr)
  ##     if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
  ##   }
  ##   ylim <- ylimAdj*range(0, SPRratio[,models], na.rm=TRUE)
  ##   if(uncertainty) ylim <- ylimAdj*range(ylim, SPRratioUpper[,models], na.rm=TRUE)

  ##   # make plot
  ##   if(!add) plot(0,type="n",xlim=xlim,ylim=ylim,xlab=labels[1],
  ##                 ylab=labels[8] ,xaxs=xaxs,yaxs=yaxs,las=1)
  ##   if(uncertainty) addpoly(SPRratio$Yr, lower=SPRratioLower, upper=SPRratioUpper)
  ##   if(spacepoints %in% c(0,1,FALSE) ){ # don't spread out points
  ##     matplot(SPRratio$Yr,SPRratio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,add=TRUE)
  ##   }else{ # spread out points with interval equal to spacepoints and staggering equal to staggerpoints
  ##     matplot(SPRratio$Yr,SPRratio[,models],col=col,pch=pch,lty=lty,lwd=lwd,type="l",add=TRUE)
  ##     if(type!="l"){
  ##       SPRratio2 <- SPRratio
  ##       for(iline in 1:nlines){
  ##         imodel <- models[iline]
  ##         SPRratio2[SPRratio2$Yr%%spacepoints != (staggerpoints*iline)%%spacepoints, imodel] <- NA
  ##       }
  ##       matplot(SPRratio2$Yr,SPRratio2[,models],col=col,pch=pch,lty=lty,lwd=lwd,type="p",add=TRUE)
  ##     }
  ##   }
  ##   abline(h=0,col="grey")
  ##   abline(h=sprtarg,col="grey",lty=2)

  ##   if(btarg>0){
  ##     abline(h=0,col="grey")
  ##     abline(h=sprtarg,col="red",lty=2)
  ##     text(SPRratio$Yr[1]+4,(1+0.02),labels[10],adj=0)
  ##   }

  ##   if(legend) legendfun(legendlabels)
  ## }
  
  plotRecruits <- function(uncertainty=TRUE){ # plot recruitment
    # determine y-limits
    ylim <- ylimAdj*range(0,recruits[,models],na.rm=TRUE)
    if(uncertainty) ylim <- ylimAdj*range(ylim, recruits[,models], recruitsUpper[,models], na.rm=TRUE)

    # do some automatic scaling of the units
    ylab <- labels[4]
    yunits <- 1
    if(ylim[2] > 1e3 & ylim[2] < 1e6){ # if max recruits a million and a billion
      yunits <- 1e3
      ylab <- gsub("1,000s","millions",ylab)
    }
    if(ylim[2] > 1e6){ # if max is greater than a billion (e.g. pacific hake)
      yunits <- 1e6
      ylab <- gsub("1,000s","billions",ylab)
    }

    if(xlim[1]=="default"){
      xlim <- range(recruits$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }

    # plot lines showing recruitment
    if(spacepoints %in% c(0,1,FALSE) ){ # don't spread out points
      matplot(recruits$Yr[-(1:2)],recruits[-(1:2),models],col=col,pch=pch,lty=lty,lwd=lwd,type=type,
              xlim=xlim,ylim=ylim,
              xlab=labels[1],ylab=ylab,xaxs=xaxs,yaxs=yaxs,axes=FALSE,add=add)
    }else{ # spread out points with interval equal to spacepoints and staggering equal to staggerpoints
      matplot(recruits$Yr[-(1:2)],recruits[-(1:2),models],col=col,pch=pch,lty=lty,lwd=lwd,type="l",
              xlim=xlim,ylim=ylim,
              xlab=labels[1],ylab=ylab,xaxs=xaxs,yaxs=yaxs,axes=FALSE,add=add)
      if(type!="l"){
        recruits2 <- recruits
        for(iline in 1:nlines){
          imodel <- models[iline]
          recruits2[recruits2$Yr%%spacepoints != (staggerpoints*iline)%%spacepoints, imodel] <- NA
        }
        matplot(recruits2$Yr[-(1:2)],recruits2[-(1:2),models],col=col,pch=pch,lty=lty,lwd=lwd,type="p",
                xlim=xlim,ylim=ylim,
                xlab=labels[1],ylab=ylab,xaxs=xaxs,yaxs=yaxs,axes=FALSE,add=TRUE)
      }
    }

    # add points at equilibrium values
    points(x=rep(recruits$Yr[1],nlines), recruits[1, models], col=col, pch=pch, cex=1.2, lwd=lwd)    

    if(uncertainty){
      for(iline in 1:nlines){
        imodel <- models[iline]
        xvec <- recruits$Yr
        if(nlines>1) xvec <- xvec + 0.4*iline/nlines - 0.2
        old_warn <- options()$warn      # previous setting
        options(warn=-1)                # turn off "zero-length arrow" warning
        arrows(x0=xvec, y0=pmax(as.numeric(recruitsLower[,imodel]),0),
               x1=xvec, y1=as.numeric(recruitsUpper[,imodel]),
               length=0.01, angle=90, code=3, col=col[iline])
        options(warn=old_warn)  #returning to old value
      }
    }
    abline(h=0,col="grey")
    if(legend) legendfun(legendlabels)
    if(!add) axis(1)
    yticks <- pretty(ylim)
    if(!add) axis(2,at=yticks,labels=format(yticks/yunits),las=1)
    box()
  }

  plotRecDevs <- function(uncertainty=TRUE){ # plot recruit deviations
    # empty plot
    if(xlim[1]=="default"){
      xlim <- range(recdevs$Yr)
      if(!is.null(endyrvec) & all(endyrvec < max(xlim))) xlim[2] <- max(endyrvec)
    }
    ylim <- ylimAdj*range(recdevs[,models],na.rm=TRUE)
    if(uncertainty){
      if(all(is.na(recdevsLower[,models]))) return() # can't do uncertainty if no range present
      ylim <- ylimAdj*range(recdevsLower[,models],recdevsUpper[,models],na.rm=TRUE)
    }
    ylim <- range(-ylim,ylim) # make symmetric
                   
    if(!add) plot(0,xlim=xlim,ylim=ylim,
         type="n",xlab=labels[1],ylab=labels[5],xaxs=xaxs,yaxs=yaxs,las=1)
    abline(h=0,col="grey")

    if(uncertainty){
      for(iline in 1:nlines){
        imodel <- models[iline]
        xvec <- recdevs$Yr
        if(nlines>1) xvec <- xvec + 0.4*iline/nlines - 0.2
        arrows(x0=xvec, y0=as.numeric(recdevsLower[,imodel]),
               x1=xvec, y1=as.numeric(recdevsUpper[,imodel]),
               length=0.01, angle=90, code=3, col=col[iline])
      }
    }
    
    # loop over vector of models to add lines
    for(iline in 1:nlines){
      imodel <- models[iline]
      yvec <- recdevs[,imodel]
      xvec <- recdevs$Yr[!is.na(yvec)]
      yvec <- yvec[!is.na(yvec)]
      points(xvec,yvec,pch=pch[iline],lwd=lwd[iline],col=col[iline])
    }
    if(legend) legendfun(legendlabels)
  }


      ## xmax <- 1.1*max(reldep)
      ## ymax <- 1.1*max(1,relspr[!is.na(relspr)])
      ## ylab <- managementratiolabels[1,2]
      ## phasefunc <- function(){
      ##   if(!add) plot(reldep,relspr,xlab="B/Btarget",
      ##                 xlim=c(0,xmax),ylim=c(0,ymax),ylab=ylab,type="n")
      ##   lines(reldep,relspr,type="o",col=col2)
      ##   abline(h=0,col="grey")
      ##   abline(v=0,col="grey")
      ##   lines(reldep,relspr,type="o",col=col2)
      ##   points(reldep[length(reldep)],relspr[length(relspr)],col=col4,pch=19)
      ##   abline(h=1,col=col4,lty=2)
      ##   abline(v=1,col=col4,lty=2)}
  
  plotPhase <- function(uncertainty=TRUE){ # plot biomass ratio vs. SPRratio
    # get axis limits
    xlim <- range(0, ylimAdj*Bratio[,models], na.rm=TRUE)
    ylim <- range(0, ylimAdj*SPRratio[,models], na.rm=TRUE)

    # make plot
    if(!add) plot(0,type="n",xlim=xlim,ylim=ylim,xlab=labels[3],ylab=labels[8],
                  xaxs=xaxs,yaxs=yaxs,las=1)

    goodyrs <- intersect(Bratio$Yr, SPRratio$Yr)
    lastyr <- max(goodyrs)
    for(iline in 1:nlines){
      imodel <- models[iline]
      # no option get to stagger points in phase plots, only the last point is marked
      xvals <- Bratio[Bratio$Yr %in% goodyrs, imodel]
      yvals <- SPRratio[SPRratio$Yr %in% goodyrs, imodel]
      lines(xvals,
            yvals,
            col=col[iline],
            lty=lty[iline],lwd=lwd[iline],
            type='l') # no user control of type to add points
      # NA values and missing points will occur if final year is different
      points(tail(xvals,1), 
             tail(yvals,1),
             col=col[iline],
             pch=pch[iline],lwd=lwd[iline])
    }

    abline(h=1,v=1,col="grey",lty=2)

    if(btarg>0) abline(v=btarg,col="red",lty=2)
    if(sprtarg>0) abline(h=sprtarg,col="red",lty=2)

    if(legend) legendfun(legendlabels)
  }

  plotIndices <- function(log=FALSE){ # plot different fits to a single index of abundance

    # get a subset of index table including only 1 index per model
    # (hopefully matching each other)
    indices2 <- NULL
    for(iline in 1:nlines){
      imodel <- models[iline]
      subset1 <- indices$imodel==imodel & !is.na(indices$Like)
      subset2 <- indices$imodel==imodel
      if(length(unique(indices$FleetNum[subset2])) > 1){
        if(!is.null(indexfleets[imodel])){
          ifleet <- indexfleets[imodel]
          indices2 <- rbind(indices2,indices[subset2 & indices$FleetNum==ifleet,])
        }else{
          cat("some models have multiple indices, 'indexfleets' required\n  for all models in summaryoutput\n")
          return()
        }
      }else{
        indices2 <- rbind(indices2,indices[subset2,])
      }
    }
    # get quantities for plot
    yr <- indices2$Yr
    obs <- indices2$Obs
    exp <- indices2$Exp
    imodel <- indices2$imodel
    Q <- indices2$Calc_Q
    if(log){
      obs <- log(obs)
      exp <- log(exp)
      ylab=labels[7]
    }else{
      ylab=labels[6]
    }

    # get uncertainty intervals if requested
    if(indexUncertainty){
      if(indexPlotEach) {
        if(indexSEvec[1]=="default") indexSEvec <- indices2$SE    #there may be a little bit of confusion from using just the first element of indexSEvec
        y <- obs
        if(log){
            upper <- qnorm(.975,mean=y,sd=indexSEvec)
            lower <- qnorm(.025,mean=y,sd=indexSEvec)
        }else{
            upper <- qlnorm(.975,meanlog=log(y),sdlog=indexSEvec)
            lower <- qlnorm(.025,meanlog=log(y),sdlog=indexSEvec)
        }
      }else {      
        subset <- indices2$imodel==models[1]
        if(indexSEvec[1]=="default") indexSEvec <- indices2$SE[subset]
        y <- obs[subset]
        if(log){
            upper <- qnorm(.975,mean=y,sd=indexSEvec)
            lower <- qnorm(.025,mean=y,sd=indexSEvec)
        }else{
            upper <- qlnorm(.975,meanlog=log(y),sdlog=indexSEvec)
            lower <- qlnorm(.025,meanlog=log(y),sdlog=indexSEvec)
        }
      }
    }else{
      upper <- NULL
      lower <- NULL
    }
    
    # make plot
    ylim <- ylimAdj*range(obs[!is.na(indices2$Like)],exp,lower,upper)
    if(!log) ylim <- range(0,ylim) # 0 included if not in log space
    meanQ <- rep(NA,nlines)
    
    if(!add) plot(0,type="n",xlim=range(yr),ylim=ylim,xlab="Year",ylab=ylab,axes=FALSE)
    if(!log) abline(h=0,col="grey")
    Qtext <- rep("(Q =",nlines)
    for(iline in (1:nlines)[!mcmcVec]){
      imodel <- models[iline]
      subset <- indices2$imodel==imodel
      meanQ[iline] <- mean(Q[subset])
      if(any(Q[subset]!=mean(Q[subset]))) Qtext[iline] <- "(mean Q ="
      x <- yr[subset]
      y <- exp[subset]
      lines(x, y, pch=pch[iline], lwd=lwd[iline],
            lty=lty[iline], col=col[iline], type=type)
    }
    legendlabels2 <- legendlabels
    if(indexQlabel) legendlabels2 <- paste(legendlabels,Qtext,format(meanQ,digits=indexQdigits),")")
    if(legend) legendfun(legendlabels2)
    
    # get uncertainty intervals if requested
    # put observed values on top
    #subset <- indices2$imodel==1
    # points(yr[subset],obs[subset],pch=16,cex=1.5,type="o",lty=3) # connected by dashed lines
    if(indexPlotEach) {  #plot observed values for each model or just the first model
        for(iline in (1:nlines)[!mcmcVec]){
            imodel <- models[iline]
            subset <- indices2$imodel==imodel & !is.na(indices2$Like)
            if(indexUncertainty)
                arrows(x0=yr[subset], y0=lower[subset], x1=yr[subset], y1=upper[subset], length=0.01, angle=90, code=3, col=shadecol[iline])
            points(yr[subset],obs[subset],pch=16,cex=1.5,col=shadecol[iline])
        }
    }else {
        imodel <- models[which(endyrvec==max(endyrvec))[1]]
        subset <- indices2$imodel==imodel & !is.na(indices2$Like)
        if(indexUncertainty)
            arrows(x0=yr[subset], y0=lower[subset], x1=yr[subset], y1=upper[subset], length=0.01, angle=90, code=3, col=1)
        points(yr[subset],obs[subset],pch=16,cex=1.5)
    }

    if(!add) axis(1,at=yr)
    if(!add) axis(2)
    box()
  } # end plotIndices function
  
  plotDensities <- function(parname,xlab,denslwd,limit0=TRUE,cumulative=F){
    if(any(!mcmcVec)) { 
      vals <- rbind(pars[grep(parname,pars$Label,fixed=TRUE),],
                    quants[grep(parname,quants$Label,fixed=TRUE),])
      if(nrow(vals)!=1){
        cat("problem getting values for parameter:",parname,"\n")
        if(nrow(vals)==0) cat("no Labels matching in either parameters or derived quantities\n")
        if(nrow(vals)>0){
          cat("Too many matching Labels:")
          print(vals[,models])
        }
        return(NULL)  #previous versions had an else statement, but this will end the function here instead and saves indenting
      }
      valSDs <- rbind(parsSD[grep(parname,pars$Label,fixed=TRUE),],
                      quantsSD[grep(parname,quants$Label,fixed=TRUE),])
    }

    xmax <- xmin <- ymax <- NULL # placeholder for limits
    mcmcDens <- vector(mode="list",length=nlines)   #placeholder for the mcmc density estimates, if there are any
    # loop over models to set range
    good <- rep(TRUE,nlines) # indicator of which values to plot
    for(iline in 1:nlines){
      imodel <- models[iline]
      if(mcmcVec[iline]) {
        
        mcmcColumn <- grep(parname,colnames(mcmc[[imodel]]),fixed=TRUE)
        if(length(mcmcColumn)==0) {
            cat("No columns selected from MCMC for '",parname,"' in model ",imodel,".\n",sep="")
            good[iline] <- FALSE 
        }
        if(length(mcmcColumn)>1) {
            cat("Too many columns selected from MCMC for model ",imodel,":\n",sep="")
            print(names(mcmc[[imodel]])[mcmcColumn])
            cat("Please specify a unique label in the mcmc dataframe\nor specify mcmcVec=F for model",imodel,"or specify mcmcVec='default'.\n")
            good[iline] <- FALSE 
        }
        if(good[iline]){
          mcmcVals <- mcmc[[imodel]][,mcmcColumn]
          if(nsexes[imodel]==1 &&  grepl("SPB",parname)) {   #divide by 2 for female only spawning biomass
            mcmcVals <- mcmcVals/2
          }
          xmin <- min(xmin, quantile(mcmcVals,0.005))
          if(limit0) xmin <- max(0,xmin) # by default no plot can go below 0 
          if(fix0 & !grepl("R0",parname)) xmin <- 0 # include 0 if requested (except for log(R0) plots)
          xmax <- max(xmax, quantile(mcmcVals,0.995))
          z <- density(mcmcVals,cut=0,adjust=densityadjust)  #density estimate of mcmc sample (posterior)
          z$x <- z$x[c(1,1:length(z$x),length(z$x))]
          z$y <- c(0,z$y,0)           #just to make sure that a good looking polygon is created
          ymax <- max(ymax,max(z$y))  #update ymax
          mcmcDens[[iline]] <- z      #save the density estimate for later plotting
        }
      }else{
        parval <- vals[1,imodel]
        parSD <- valSDs[1,imodel]
        if(!is.numeric(parval)) parval <- -1     #do this in case models added without the parameter
        if(!is.na(parSD) && parSD>0){ # if non-zero SD available
          if(nsexes[imodel]==1 &&  grepl("SPB",parname)) {   #divide by 2 for female only spawning biomass
            parval <- parval/2
            parSD <- parSD/2
          }
          # update x range
          xmin <- min(xmin, qnorm(0.005,parval,parSD))
          if(limit0) xmin <- max(0,xmin) # by default no plot can go below 0 
          if(fix0 & !grepl("R0",parname)) xmin <- 0 # include 0 if requested (except for log(R0) plots)
          xmax <- max(xmax, qnorm(0.995,parval,parSD))
          # calculate density to get y range
          x <- seq(xmin,xmax,length=500)
          mle <- dnorm(x,parval,parSD)
          mlescale <- 1/(sum(mle)*mean(diff(x)))
          mle <- mle*mlescale
          # update ymax
          ymax <- max(ymax,max(mle))
        }else{ # if no SD, at least make sure interval includes MLE estimate
          xmin <- min(xmin, parval)
          xmax <- max(xmax, parval)
        }
      }
    }
    if(grepl("Bratio",parname)) xmin <- 0 # xmin=0 for depletion plots
    if(limit0) xmin <- max(0,xmin) # by default no plot can go below 0 
    if(fix0 & !grepl("R0",parname)) xmin <- 0 # include 0 if requested (except for log(R0) plots)
    
    # calculate x-limits and vector of values for densities
    xlim <- c(xmin,xmin+(xmax-xmin)*densityscalex)
    x <- seq(xmin,xmax,length=500)
    
    # calculate some scaling stuff
    xunits <- 1
    if(xmax > 1e3 & xmax < 1e6){
      xunits <- 1e3
      #xlab <- gsub("mt","x1000 mt",xlab)
      xlab2 <- "1000 mt"
    }
    if(xmax > 1e6){
      xunits <- 1e6
      #xlab <- gsub("mt","million mt",xlab)
      xlab2 <- "million mt"
    }
    # make empty plot
    if(is.null(ymax)){
      cat("  skipping plot of",parname,"because it seems to not be estimated in any model\n")
    }else{
      if(!add) {
        if(cumulative) {
            plot(0,type="n",xlim=xlim,axes=FALSE,xaxs="i",ylim=c(0,1),xlab=xlab,ylab="")        
        } else {
            plot(0,type="n",xlim=xlim,axes=FALSE,xaxs="i",ylim=c(0,1.1*ymax*densityscaley),xlab=xlab,ylab="")
        }
      }
      # add vertical lines for target and threshold depletion values
      if(grepl("Bratio",parname)){
        if(btarg>0){
          abline(v=btarg,col="red",lty=2)
          text(btarg+0.03,par()$usr[4],labels[10],adj=1.05,srt=90)
        }
        if(minbthresh>0){
          abline(v=minbthresh,col="red",lty=2)
          text(minbthresh+0.03,par()$usr[4],labels[11],adj=1.05,srt=90)
        }
      }
      
      symbolsQuants <- c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)
      # loop again to make plots
      for(iline in (1:nlines)[good]){
        imodel <- models[iline]
        if(mcmcVec[iline]) {
          mcmcColumn <- grep(parname,colnames(mcmc[[imodel]]),fixed=TRUE)
          mcmcVals <- mcmc[[imodel]][,mcmcColumn]
          if(nsexes[imodel]==1 &&  grepl("SPB",parname)) {   #divide by 2 for feamle only spawning biomass
            mcmcVals <- mcmcVals/2
          }
          x2 <- quantile(mcmcVals,symbolsQuants)   # for symbols on plot
          #find the positions in the density that are closest to these quantiles
          x <- mcmcDens[[iline]]$x
          if(!cumulative) {
            y <- mcmcDens[[iline]]$y
            yscale <- 1/(sum(y)*mean(diff(x)))
            y <- y*yscale
          } else {
            y <- cumsum(mcmcDens[[iline]]$y)/sum(mcmcDens[[iline]]$y)
          }
          y2 <- NULL
          for(ii in x2) {
            # find y-value associated with closest matching x-value
            # "min" was added for a rare case where two values were equally close
            y2 <- c(y2,min(y[abs(x-ii)==min(abs(x-ii))]))
          }
          
          if(!cumulative) {
              polygon(c(x[1],x,rev(x)[1]),c(0,y,0),col=shadecol[iline],border=NA)
          } else {
              polygon(c(x[1],x,rev(x)[1]),c(0,y,1),col=shadecol[iline],border=NA)
          }
          lines(x,y,col=col[iline],lwd=2)
          if(!cumulative) {
              if(densitysymbols) points(x2,y2,col=col[iline],pch=pch[iline])
              lines(rep(x2[median(1:length(x2))],2),c(0,y2[median(1:length(x2))]),col=col[iline]) #really hokey and assumes that the middle value of the vector is the median
          } else {
              if(densitysymbols) points(x2,symbolsQuants,col=col[iline],pch=pch[iline])
              lines(rep(median(mcmcVals),2),c(0,0.5),col=col[iline])
          }
        }else{
          parval <- vals[1,imodel]
          parSD <- valSDs[1,imodel]
          if(!is.na(parSD) && parSD>0){
            if(nsexes[imodel]==1 &&  grepl("SPB",parname)) {   #divide by 2 for feamle only spawning biomass
              parval <- parval/2
              parSD <- parSD/2
            }
            xmin <- min(xmin, qnorm(0.005,parval,parSD))
            if(limit0) xmin <- max(0,xmin) # by default no plot can go below 0 
            if(fix0 & !grepl("R0",parname)) xmin <- 0 # include 0 if requested (except for log(R0) plots)
            x <- seq(xmin,max(xmax,xlim),length=500)
            #x2 <- parval+(-2:2)*parSD # 1 and 2 SDs away from mean to plot symbols
            x2 <- qnorm(symbolsQuants,parval,parSD)
            if(cumulative) {
                y <- mle <- pnorm(x,parval,parSD)  # smooth line
                y2 <- mle2 <- pnorm(x2,parval,parSD) # symbols
            } else {
                mle <- dnorm(x,parval,parSD)  # smooth line
                mle2 <- dnorm(x2,parval,parSD) # symbols
                mlescale <- 1/(sum(mle)*mean(diff(x)))
                y <- mle <- mle*mlescale
                y2 <- mle2 <- mle2*mlescale
            }

            polygon(c(x[1],x,rev(x)[1]),c(0,mle,0),col=shadecol[iline],border=NA)
            lines(x,mle,col=col[iline],lwd=2)
            if(!cumulative) {
                if(densitysymbols) points(x2,mle2,col=col[iline],pch=pch[iline])
                lines(rep(parval,2),c(0,dnorm(parval,parval,parSD)*mlescale),col=col[iline],lwd=denslwd) #
            } else {
                if(densitysymbols) points(x2,symbolsQuants,col=col[iline],pch=pch[iline])
                lines(rep(parval,2),c(0,0.5),col=col[iline],lwd=denslwd) #
            }
          }else{
            abline(v=parval,col=col[iline],lwd=denslwd)
          }
        }
        # should be able to move more stuff into this section that applies to both MLE and MCMC
        if(densitytails){
          x.lower <- x[x<=x2[1]]
          y.lower <- y[x<=x2[1]]
          x.upper <- x[x>=rev(x2)[1]]
          y.upper <- y[x>=rev(x2)[1]]
          polygon(c(x.lower[1],x.lower,rev(x.lower)[1]),
                  c(0,y.lower,0),col=shadecol[iline],border=NA)
          polygon(c(x.upper[1],x.upper,rev(x.upper)[1]),
                  c(0,y.upper,0),col=shadecol[iline],border=NA)
        }
  
      }
      abline(h=0,col="grey")
      xticks <- pretty(xlim)
      if(!add) {
        axis(1,at=xticks,labels=format(xticks/xunits))
        theLine <- 1
        if(cumulative) {
            axis(2,at=symbolsQuants,labels=format(symbolsQuants),las=1,cex.axis=0.9)
            theLine <- 3
        }
        mtext(side=2,line=theLine,labels[9])
      }
      if(xunits!=1) cat("  note: x-axis for ",parname," has been divided by ",xunits," (so may be in units of ",xlab2,")\n",sep="")
      box()
      legendfun(legendlabels,cumulative)
    }
  } # end plotDensities function
  

  uncertaintyplots <- intersect(c(2,4,6,8,10,13),subplots)
  if(!uncertainty & length(uncertaintyplots)>0)
    cat("skipping plots with uncertainty:",paste(uncertaintyplots,collapse=","),"\n")
  # subplot 1: spawning biomass
  if(1 %in% subplots){
    if(verbose) cat("subplot 1: spawning biomass\n")
    if(plot) plotSpawnBio(uncertainty=FALSE)
    if(print){
      pngfun("compare1_spawnbio.png")
      plotSpawnBio(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 2: spawning biomass with uncertainty intervals
  if(2 %in% subplots){
    if(uncertainty){
      if(verbose) cat("subplot 2: spawning biomass with uncertainty intervals\n")
      if(plot) plotSpawnBio(uncertainty=uncertainty)
      if(print){
        pngfun("compare2_spawnbio_uncertainty.png")
        plotSpawnBio(uncertainty=uncertainty)
        dev.off()
      }
    }
  }

  # subplot 3: biomass ratio (hopefully equal to spawning depletion)
  if(3 %in% subplots){
    if(verbose) cat("subplot 3: biomass ratio (hopefully equal to spawning depletion)\n")
    if(plot) plotBratio(uncertainty=FALSE)
    if(print){
      pngfun("compare3_Bratio.png")
      plotBratio(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 4: biomass ratio with uncertainty
  if(4 %in% subplots){
    if(uncertainty){
      if(verbose) cat("subplot 4: biomass ratio with uncertainty\n")
      if(plot) plotBratio(uncertainty=uncertainty)
      if(print){
        pngfun("compare4_Bratio_uncertainty.png")
        plotBratio(uncertainty=uncertainty)
        dev.off()
      }
    }
  }

  # subplot 5: SPR ratio
  if(5 %in% subplots){
    if(verbose) cat("subplot 5: SPR ratio\n")
    if(plot) plotSPRratio(uncertainty=FALSE)
    if(print){
      pngfun("compare5_SPRratio.png")
      plotSPRratio(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 6: SPR ratio with uncertainty
  if(6 %in% subplots){
    if(uncertainty){
      if(verbose) cat("subplot 6: SPR ratio with uncertainty\n")
      if(plot) plotSPRratio(uncertainty=uncertainty)
      if(print){
        pngfun("compare6_SPRratio_uncertainty.png")
        plotSPRratio(uncertainty=uncertainty)
        dev.off()
      }
    }
  }

  # subplot 7: recruits
  if(7 %in% subplots){
    if(verbose) cat("subplot 7: recruits\n")
    if(plot) plotRecruits(uncertainty=FALSE)
    if(print){
      pngfun("compare7_recruits.png")
      plotRecruits(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 8: recruits with uncertainty
  if(8 %in% subplots){
    if(uncertainty){
      if(verbose) cat("subplot 8: recruits with uncertainty\n")
      if(plot) plotRecruits()
      if(print){
        pngfun("compare8_recruits_uncertainty.png")
        plotRecruits()
        dev.off()
      }
    }
  }
  
  # subplot 9: recruit devs
  if(9 %in% subplots){
    if(verbose) cat("subplot 9: recruit devs\n")
    if(plot) plotRecDevs(uncertainty=FALSE)
    if(print){
      pngfun("compare9_recdevs.png")
      plotRecDevs(uncertainty=FALSE)
      dev.off()
    }
  }

  # subplot 10: recruit devs with uncertainty
  if(10 %in% subplots){
    if(uncertainty){
      if(verbose) cat("subplot 10: recruit devs with uncertainty\n")
      if(plot) plotRecDevs()
      if(print){
        pngfun("compare10_recdevs_uncertainty.png")
        plotRecDevs()
        dev.off()
      }
    }
  }

  # filename extension for index plots
  val <- paste("_flt",unique(indexfleets),sep="")
  if(length(val)!=1) val <- NULL

  # subplot 11: index fits
  if(11 %in% subplots){
    if(verbose) cat("subplot 11: index fits\n")
    if(plot) plotIndices()
    if(print){
      pngfun(paste("compare11_indices",val,".png",sep=""))
      plotIndices()
      dev.off()
    }
  }

  # subplot 12: index fits on a log scale
  if(12 %in% subplots){
    if(verbose) cat("subplot 12: index fits on a log scale\n")
    if(plot) plotIndices(log=TRUE)
    if(print){
      pngfun(paste("compare12_indices_log",val,".png",sep=""))
      plotIndices(log=TRUE)
      dev.off()
    }
  }

  #### unfinished addition of phase plot comparisons
  ## # subplot 13: phase plot
  if(13 %in% subplots){
    if(verbose) cat("subplot 13: phase plot\n")
    if(plot) plotPhase()
    if(print){
      pngfun("compare13_phase_plot.png")
      plotPhase()
      dev.off()
    }
  }

  # subplot 14: densities
  if(14 %in% subplots){
    if(uncertainty){
      if(verbose) cat("subplot 14: densities\n")
      # look for all parameters or derived quantities matching the input list of names
      expandednames <- NULL
      for(i in 1:length(densitynames)){
        matchingnames <- c(pars$Label,quants$Label)[grep(densitynames[i],c(pars$Label,quants$Label),fixed=TRUE)]
        expandednames <- c(expandednames,matchingnames)
      }
      if(length(expandednames)==0){
        cat("  No parameter/quantity names matching 'densitynames' input.\n")
      }else{
        cat("  parameter/quantity names matching 'densitynames' input:\n")
        print(expandednames)
        ndensities <- length(expandednames)
        # make a table to store associated x-labels
        densitytable <- data.frame(name=expandednames,label=expandednames,stringsAsFactors=FALSE)
        if(length(densityxlabs)==ndensities & densityxlabs[1]!="default"){
          densitytable$label <- densityxlabs
          cat("  table of parameter/quantity labels with associated x-axis label:\n")
          print(densitytable)
        }else{
          if(densityxlabs[1]!="default"){
            cat("  length of 'densityxlabs' doesn't match the number of values matching 'densitynames'\n",
                "    parameter labels will be used instead\n")
          }
        }
        for(iplot in 1:ndensities){
          # find matching parameter
          name <- densitytable[iplot,1]
          xlab <- densitytable[iplot,2]
          #if(verbose) cat("  quantity name=",name,"\n",sep="")
          if(plot) {
            plotDensities(parname=name,xlab=xlab,denslwd=densitylwd)
          }
          if(print){
            pngfun(paste("compare14_densities_",name,".png",sep=""))
            plotDensities(parname=name,xlab=xlab,denslwd=densitylwd)
            dev.off()
          }
        }
      }
    }
  }

  # subplot 15: cumulative probability plots
  #  draws cumulative plots of the same parameters drawn in density plots
  #  uses some same objects and names as densityplots
  if(15 %in% subplots){
    if(uncertainty){
      if(verbose) cat("subplot 15: cumulative probability\n")
      # look for all parameters or derived quantities matching the input list of names
      expandednames <- NULL
      for(i in 1:length(densitynames)){
        matchingnames <- c(pars$Label,quants$Label)[grep(densitynames[i],c(pars$Label,quants$Label),fixed=TRUE)]
        expandednames <- c(expandednames,matchingnames)
      }
      if(length(expandednames)==0){
        cat("  No parameter/quantity names matching 'densitynames' input.\n")
      }else{
        cat("  parameter/quantity names matching 'densitynames' input:\n")
        print(expandednames)
        ndensities <- length(expandednames)
        # make a table to store associated x-labels
        densitytable <- data.frame(name=expandednames,label=expandednames,stringsAsFactors=FALSE)
        if(length(densityxlabs)==ndensities & densityxlabs[1]!="default"){
          densitytable$label <- densityxlabs
          cat("  table of parameter/quantity labels with associated x-axis label:\n")
          print(densitytable)
        }else{
          if(densityxlabs[1]!="default"){
            cat("  length of 'densityxlabs' doesn't match the number of values matching 'densitynames'\n",
                "    parameter labels will be used instead\n")
          }
        }
        for(iplot in 1:ndensities){
          # find matching parameter
          name <- densitytable[iplot,1]
          xlab <- densitytable[iplot,2]
          #if(verbose) cat("  quantity name=",name,"\n",sep="")
          if(plot) {
            plotDensities(parname=name,xlab=xlab,denslwd=densitylwd,cumulative=T)
          }
          if(print){
            pngfun(paste("compare14_densities_",name,".png",sep=""))
            plotDensities(parname=name,xlab=xlab,denslwd=densitylwd,cumulative=T)
            dev.off()
          }
        }
      }
    }
  }


  #### unfinished addition of growth comparisons
  ## # subplot 14: growth, females
  ## if(14 %in% subplots){
  ##   if(verbose) cat("subplot 14: growth, females\n")
  ##   if(plot) plotgrowth(sex='f')
  ##   if(print){
  ##     pngfun("compare14_growth_females.png")
  ##     plotgrowth(sex='f')
  ##     dev.off()
  ##   }
  ## }

  ## # subplot 15: growth, males
  ## if(15 %in% subplots){
  ##   if(verbose) cat("subplot 15: growth, males\n")
  ##   if(plot) plotgrowth(sex='m')
  ##   if(print){
  ##     pngfun("compare15_growth_males.png")
  ##     plotgrowth(sex='m')
  ##     dev.off()
  ##   }
  ## }
  
  

  if(pdf) dev.off()
}
