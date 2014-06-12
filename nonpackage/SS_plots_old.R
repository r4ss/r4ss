#' plot many quantities related to output from Stock Synthesis
#' 
#' Creates a user-chosen set of plots, including biological quantities, time
#' series, and fits to data.  Plots are sent to R GUI, single PDF file, or
#' multiple PNG files. This is now just a wrapper which calls on separate
#' functions to make all the plots.
#' 
#' 
#' @param replist list created by \code{SS_output}
#' @param plot Plot sets to be created, see list of plots below.  Use to
#' specify only those plot sets of interest, e.g., c(1,2,5,10). Plots for data
#' not available in the model run will automatically be skipped, whether called
#' or not. Default=1:24.
#' @param print Plot set to be printed to files?  PNG files are created in the
#' specified directory for one or more requested plots.  Each plot has a unique
#' name starting with the number of the set it comes from and attempting to be
#' mildly descriptive.  This argument is independent of "plot" in that plots
#' can be created on screen, or printed to file or both.  Manual rescaling
#' cannot be done after printing, so this option may not be the best choice for
#' each plot; saving files from the screen allows more control. Default=0.
#' @param pdf Send plots to PDF file instead of R GUI? Input \code{plot} must
#' be used and input \code{print} must be 0. Default=0.
#' @param printfolder Name of subfolder to create within the working directory
#' into which any PNG files specified by \code{print} will be saved. By default
#' the working directory is used with no subfolder.  Default="".
#' @param dir The directory in which any PNG files requested by \code{print}
#' are created. By default it will be the same directory that the report file
#' was read from by the \code{SS_output} function. Default="default".
#' @param fleets Either the string "all", or a vector of numerical values, like
#' c(1,3), listing fleets or surveys for which plots should be made. By
#' default, plots will be made for all fleets and surveys.  Default="all".
#' @param areas Either the string "all", or a vector of numerical values, like
#' c(1,3), listing areas for which plots should be made in a multi-area model.
#' By default, plots will be made for all areas (excepting cases where the
#' function has not yet been updated for multi-area models). Default="all".
#' @param fleetnames Either the string "default", or a vector of characters
#' strings to use for each fleet name. Default="default".
#' @param fleetcols Either the string "default", or a vector of colors to use
#' for each fleet.  Default="default".
#' @param fleetlty Vector of line types used for each fleet in some plots.
#' Default=1.
#' @param fleetpch Vector of point types used for each fleet in some plots.
#' Default=1.
#' @param lwd Line width for some plots. Default=1.
#' @param areacols Either the string "default", or a vector of colors to use
#' for each area. Default="default".
#' @param areanames Optional vector of names for each area used in titles.
#' Default="default".
#' @param verbose Return updates of function progress to the R GUI?  Default=T.
#' @param uncertainty Include values in plots showing estimates of uncertainty
#' (requires positive definite hessian in model and \code{covar}=T in
#' \code{SS_output})?  Default=T.
#' @param forecastplot Include forecast years in the plots? Obviously requires
#' forecast options to have been used in the model.  Default=T.
#' @param datplot Plot the data by itself? This is useful in document
#' preparation. Setting datplot=F is equivalent to leaving off plots 15 and 16.
#' Default=F.
#' @param Natageplot Plot the expected numbers at age bubble plots and mean-age
#' time series?  Default=T.
#' @param samplesizeplots Show sample size plots?  Default=T.
#' @param compresidplots Show residuals for composition plots?
#' @param sprtarg Specify the F/SPR proxy target. Default=0.4.
#' @param btarg Target depletion to be used in plots showing depletion. May be
#' omitted by setting to NA.  Default=0.4.
#' @param minbthresh Threshold depletion to be used in plots showing depletion.
#' May be omitted by setting to NA. Default=0.25.
#' @param pntscalar This scalar defines the maximum bubble size for balloon
#' plots; each plot scaled independently based on this maximum size and the
#' values plotted. Often some plots look better with one value and others with
#' a larger or smaller value. Default=2.6
#' @param minnbubble This defines the minimum number of years below which blank
#' years will be added to bubble plots to avoid cropping.  Default=8.
#' @param aalyear Years to plot multi-panel conditional age-at-length fits for
#' all length bins; must be in a "c(YYYY,YYYY)" format. Useful for checking the
#' fit of a dominant year class, critical time period, etc. Default=-1.
#' @param aalbin The length bin for which multi-panel plots of the fit to
#' conditional age-at-length data will be produced for all years.  Useful to
#' see if growth curves are ok, or to see the information on year classes move
#' through the conditional data. Default=-1.
#' @param aalresids Plot the full set of conditional age-at-length Pearson
#' residuals? Default=F.
#' @param maxneff The maximum value to include on plots of input and effective
#' sample size. Occasionally a calculation of effective N blows up to very
#' large numbers, rendering it impossible to observe the relationship for other
#' data. Default=5000.
#' @param cohortlines Optional vector of birth years for cohorts for which to
#' add growth curves to numbers at length bubble plots.  Default=c().
#' @param smooth Add loess smoother to observed vs. expected index plots and
#' input vs. effective sample size? Default=T.
#' @param showsampsize Display sample sizes on composition plots?  Default=T.
#' @param showeffN Display effective sample sizes on composition plots?
#' Default=T.
#' @param showlegend Display legends in various plots? Default=T.
#' @param pwidth Default width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param pheight Default height width of plots printed to files in units of
#' \code{punits}. Default=7.
#' @param punits Units for \code{pwidth} and \code{pheight}. Can be "px"
#' (pixels), "in" (inches), "cm" or "mm". Default="in".
#' @param ptsize Point size for plotted text in plots printed to files (see
#' help("png") in R for details). Default=12.
#' @param res Resolution of plots printed to files. Default=300.
#' @param cex.main Character expansion parameter for plot titles (not yet
#' implemented for all plots). Default=1.
#' @param selexlines Vector controling which lines should be shown on
#' selectivity plots if the model includes retention. Default=1:5.
#' @param rows Number of rows to use for single panel plots. Default=1.
#' @param cols Number of columns to use for single panel plots. Default=1.
#' @param maxrows Maximum number of rows to for multi-panel plots.  Default=6.
#' @param maxcols Maximum number of columns for multi-panel plots.  Default=6.
#' @param maxrows2 Maximum number of rows for conditional age at length
#' multi-panel plots. Default=2.
#' @param maxcols2 Maximum number of rows for conditional age at length
#' multi-panel plots. Default=4.
#' @param tagrows Number of rows for tagging-related plots. Default=3.
#' @param tagcols Number of columns for tagging-related plots.  Default=3.
#' @param fixdims Control whether multi-panel plots all have dimensions equal
#' to maxrows by maxcols, or resized within those limits to fit number of
#' plots. Default=T.
#' @param new Open a new window or add to existing plot windows.  Default=T.
#' @param SSplotDatMargin Size of right-hand margin in data plot (may be too
#' small if fleet names are long)
#' @param catchasnumbers Is catch input in numbers instead of biomass?
#' Default=F.
#' @param catchbars show catch by fleet as barplot instead of stacked polygons
#' (default=TRUE)
#' @param legendloc Location for all legends. Default="topleft".
#' @param minyr First year to show in time-series plots (changes xlim
#' parameters).
#' @param maxyr Last year to show in time-series plots (changes xlim
#' parameters).
#' @param scalebins Rescale expected and observed proportions in composition
#' plots by dividing by bin width for models where bins have different widths?
#' Caution!: May not work correctly in all cases.
#' @param \dots Additional arguments that will be passed to some subfunctions.
#' @author Ian Stewart, Ian Taylor
#' @seealso \code{\link{SS_output}}, \code{\link{SSplotBiology}},
#' \code{\link{SSplotCatch}}, \code{\link{SSplotComps}},
#' \code{\link{SSplotDiscard}}, \code{\link{SSplotIndices}},
#' \code{\link{SSplotMnwt}}, \code{\link{SSplotNumbers}},
#' \code{\link{SSplotRecdevs}}, \code{\link{SSplotSelex}},
#' \code{\link{SSplotSpawnrecruit}}, \code{\link{SSplotSPR}},
#' \code{\link{SSplotTags}}, \code{\link{SSplotTimeseries}},
#' \code{\link{SSplotYield}}
#' @references Walters, Hilborn, and Christensen, 2008, Surplus production
#' dynamics in declining and recovering fish populations. Can. J. Fish. Aquat.
#' Sci. 65: 2536-2551.
#' @keywords hplot
SS_plots_old <-
  function(
    replist=NULL, plot=1:27, print=0, pdf=FALSE, printfolder="plots", dir="default", fleets="all", areas="all",
    fleetnames="default", fleetcols="default", fleetlty=1, fleetpch=1, lwd=1, areacols="default", areanames="default",
    verbose=TRUE, uncertainty=TRUE, forecastplot=FALSE, datplot=FALSE, Natageplot=TRUE, samplesizeplots=TRUE, compresidplots=TRUE,
    sprtarg="default", btarg="default", minbthresh="default", pntscalar=2.6, minnbubble=8, aalyear=-1, aalbin=-1, 
    aalresids=FALSE, maxneff=5000, cohortlines=c(), smooth=TRUE, showsampsize=TRUE, showeffN=TRUE, showlegend=TRUE,
    pwidth=7, pheight=7, punits="in", ptsize=12, res=300, cex.main=1,selexlines=1:5,
    rows=1, cols=1, maxrows=6, maxcols=6, maxrows2=2, maxcols2=4, tagrows=3, tagcols=3, fixdims=TRUE, new=TRUE,
    SSplotDatMargin=8, catchasnumbers=NULL, catchbars=TRUE,
    legendloc="topleft", minyr=NULL, maxyr=NULL, scalebins=FALSE, ...)
{
  ################################################################################
  #
  # SS_plots
  # This function comes with no warranty or guarantee of accuracy
  #
  # Purpose: A wrapper to call many plot functions which collectively
  #          sumarize the results of a Stock Synthesis model run.
  # Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
  #          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
  #          and other contributors to http://code.google.com/p/r4ss/
  # Returns: Plots with plot history in R GUI and/or .png files.
  # General: Updated for Stock Synthesis version 3.10; R version 2.8.1
  # Notes:   See users guide for documentation.
  # Required SS3v_output function and plotrix package
  # Credit:  Based loosely on an early version of "Scape" (A. Magnusson) and "Output viewer" (R. Methot)
  #
  ################################################################################


  codedate <- "April 13, 2011"

  if(verbose) cat("R function updated:",codedate,
    "\nCheck for new code and report problems at http://code.google.com/p/r4ss/\n")

  flush.console()

  # label table is a step toward internationalization of the code
  # in the future, this could be read from a file, or we could have multiple columns
  # in the table to choose from

  if(is.null(replist)) stop("The input 'replist' should refer to an R object created by the function 'SS_output'.")

  # get quantities from the big list
  nfleets     <- replist$nfleets
  nfishfleets <- replist$nfishfleets
  nareas      <- replist$nareas
  nseasons    <- replist$nseasons
  timeseries  <- replist$timeseries
  lbins       <- replist$lbins
  inputs      <- replist$inputs
  endyr       <- replist$endyr
  SS_version  <- replist$SS_version
  Run_time    <- replist$Run_time
  Files_used  <- replist$Files_used
  FleetNames  <- replist$FleetNames
  rmse_table <- replist$rmse_table
  comp_data_exists <- replist$comp_data_exists
  
  # check for internal consistency
  if(uncertainty==TRUE & inputs$covar==FALSE){
    stop("To use uncertainty=T, you need to have covar=T in the input to the SS_output function")
  }
  if(forecastplot==TRUE & inputs$forecast==FALSE){
    stop("To use forecastplot=T, you need to have forecast=T in the input to the SSoutput function")
  }
  if(forecastplot==TRUE & max(timeseries$Yr > endyr+1)==0){
    cat("Changeing 'forecastplot' input to FALSE because all years up to endyr+1 are included by default\n")
    forecastplot <- FALSE
  }

  # derived quantities
  if(fleets[1]=="all"){
    fleets <- 1:nfleets
  }else{
    if(length(intersect(fleets,1:nfleets))!=length(fleets)){
      return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
  }}
  if(areas[1]=="all"){
    areas <- 1:nareas
  }else{ if(length(intersect(areas,1:nareas))!=length(areas)){
      return("Input 'areas' should be 'all' or a vector of values between 1 and nareas.")
  }}

  if(verbose) cat("Finished defining objects\n")
  
  ## if(nareas>1){
  ##   cat("! Warning: some plots are not configured for mult-area models (nareas=",nareas,")\n",sep="")
  ##   if(areanames[1]=="default") areanames <- paste("area",1:nareas)
  ## }
  
  if(fleetnames[1]=="default") fleetnames <- FleetNames
  if(fleetcols[1]=="default"){
    fleetcols <- rich.colors.short(nfishfleets)
    if(nfishfleets > 2) fleetcols <- rich.colors.short(nfishfleets+1)[-1]
  }
  if(length(fleetlty)<nfishfleets) fleetlty <- rep(fleetlty,nfishfleets)
  if(length(fleetpch)<nfishfleets) fleetpch <- rep(fleetpch,nfishfleets)
  if(areacols[1]=="default"){
    areacols  <- rich.colors.short(nareas)
    if(nareas > 2) areacols <- rich.colors.short(nareas+1)[-1]
  }


  #### prepare for plotting
  # make plot window (operating system specific)
  nplots <- length(intersect(1:50,plot))
  nprints <- length(intersect(1:50,print))

  OS <- "Mac" # don't know the version$os info for Mac
  if(length(grep("linux",version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw",version$os)) > 0) OS <- "Windows"

  if(nprints>0 & pdf){
    stop("can't have pdf=T and print!=0: use print only or pdf & plot inputs")
  }
  if(nplots>0 & !pdf & new){
    if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
    if(OS=="Windows") windows(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
    if(OS=="Linux") X11(width=pwidth,height=pheight,pointsize=ptsize)
    if(OS=="Mac") quartz(width=pwidth,height=pheight,pointsize=ptsize)
  }
  if(nplots>0 & !new){
    if(verbose) cat("Adding plots to existing plot window. Plot history not erased.\n")
  }
  if(dir=="default") dir <- inputs$dir
  plotdir <- paste(dir,printfolder,"/",sep="")
  if(nprints>0){
    dir.create(dir,showWarnings=FALSE)
    dir.create(plotdir,showWarnings=FALSE)
    if(nprints>0 & verbose) cat("Plots specified by 'print' will be written to",plotdir,"\n")
  }
  
  if(pdf){
    if(dir=="default") dir <- inputs$dir
    dir.create(dir,showWarnings=FALSE)
    pdffile <- paste(dir,"/SS_plots_",format(Sys.time(),'%d-%b-%Y_%H.%M' ),".pdf",sep="")
    pdf(file=pdffile,width=pwidth,height=pheight)
    if(verbose) cat("PDF file with plots will be:",pdffile,'\n')
  }
  if(new) par(mfcol=c(rows,cols)) # make multi-panel plot if requested

  if(pdf){
    mar0 <- par()$mar # current margins
    par(mar=rep(0,4))
    plot(0,type="n",xlab="",ylab="",axes=FALSE,xlim=c(0,1),ylim=c(0,1))
    y <- 0.9
    ystep <- -.05
    text(0,y,"Plots created using the 'r4ss' package in R",pos=4)
    y <- y+ystep
    text(0,y,paste("Stock Synthesis version:",substr(SS_version,1,9)),pos=4)
    y <- y+ystep
    text(0,y,Run_time,pos=4)
    y <- y+ystep
    Files2 <- strsplit(Files_used," ")[[1]]
    text(0,y,paste(Files2[[1]],Files2[2]),pos=4)
    y <- y+ystep
    text(0,y,paste(Files2[[3]],Files2[4]),pos=4)
    par(mar=mar0) # replace margins
  }

  #### plots 1 (now contains 2 as well)
  # Static growth (mean weight, maturity, fecundity, spawning output)
  # and Time-varying growth
  if(1 %in% c(plot, print) | length(cohortlines)>0)
  {
    SSplotBiology(replist=replist,
                  plot=(1 %in% plot),print=(1 %in% print),
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res, cex.main=cex.main,
                  plotdir=plotdir)
    if(verbose) cat("Finished plot 1: Biology (weight, maturity, spawning output, growth)\n")
  }

  ### plots 3 and 4 selectivity and retention
  # Length selex and retention
  if(length(intersect(c(3,4), c(plot,print)))>0)
  {
    SSplotSelex(replist=replist, selexlines=selexlines,
                fleets=fleets, fleetnames=fleetnames,
                plot=(3 %in% plot), print=(3 %in% print),
                pwidth=pwidth, pheight=pheight, punits=punits,
                ptsize=ptsize, res=res, cex.main=cex.main,
                plotdir=plotdir)
    if(verbose) cat("Finished plots 3 and 4: selectivity plots\n")
  }


  ### plot 5: Basic time series (contains what used to be plot 7)
  # stats and dimensions
  if(5 %in% c(plot, print))
  {
    cat("Starting time series plots (group 5)\n")
    for(isubplot in 1:15){ # which of 12 subplots to make
      for(doforecast in unique(c(FALSE,forecastplot))){ # add forecast or not
        if(isubplot %in% c(7,9,11)){
          for(douncertainty in unique(c(FALSE,uncertainty))){ # add uncertainty or not
            SSplotTimeseries(replist=replist,
                             subplot=isubplot,
                             areas=areas,
                             areacols=areacols,
                             areanames=areanames,
                             forecastplot=doforecast,
                             uncertainty=douncertainty,
                             plot=(5 %in% plot),
                             print=(5 %in% print),
                             verbose=verbose,
                             btarg=btarg, 
                             minbthresh=minbthresh,
                             minyr=minyr,maxyr=maxyr,
                             pwidth=pwidth, pheight=pheight, punits=punits,
                             ptsize=ptsize, res=res, cex.main=cex.main,
                             plotdir=plotdir)
          } # end loop over uncertainty or not
        }else{ # these plots don't have the option for uncertainty
            SSplotTimeseries(replist=replist,
                             subplot=isubplot,
                             areas=areas,
                             areacols=areacols,
                             areanames=areanames,
                             forecastplot=doforecast,
                             uncertainty=FALSE,
                             plot=(5 %in% plot),
                             print=(5 %in% print),
                             verbose=verbose,
                             btarg=btarg, 
                             minbthresh=minbthresh,
                             minyr=minyr,maxyr=maxyr,
                             pwidth=pwidth, pheight=pheight, punits=punits,
                             ptsize=ptsize, res=res, cex.main=cex.main,
                             plotdir=plotdir)
        }
      }
    }
  } # end if 5 in plot or print

  if(6 %in% c(plot, print))
  {
    # time series of catch
    if(verbose) cat("Starting catch plots (group 6):\n")
    SSplotCatch(replist=replist,
                plot=(6 %in% plot),print=(6 %in% print),
                fleetnames=fleetnames,
                fleetlty=fleetlty,
                fleetpch=fleetpch,
                fleetcols=fleetcols, 
                minyr=minyr,maxyr=maxyr,
                pwidth=pwidth, pheight=pheight, punits=punits,
                ptsize=ptsize, res=res,cex.main=cex.main,
                catchasnumbers=catchasnumbers,
                catchbars=catchbars,
                plotdir=plotdir)
  } # end if 6 in plot or print

  ### Plot 8: discard fractions (if present) ###
  if(8 %in% c(plot, print)){
    SSplotDiscard(replist=replist,
                  plot=(8 %in% plot),
                  print=(8 %in% print),
                  fleets=fleets,
                  fleetnames=fleetnames,
                  datplot=datplot,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,cex.main=cex.main,
                  plotdir=plotdir)
  } # end if 8 in plot or print

  ### Plot 9: mean body weight (if present) ###
  if(9 %in% c(plot, print) & !is.na(replist$mnwgt) && nrow(replist$mnwgt)>0){
    SSplotMnwt(replist=replist,
               plot=(9 %in% plot),
               print=(9 %in% print),
               fleets=fleets,
               fleetnames=fleetnames,
               datplot=datplot,
               pwidth=pwidth, pheight=pheight, punits=punits,
               ptsize=ptsize, res=res,cex.main=cex.main,
               plotdir=plotdir)
  } # end if 9 in plot or print

  ### Plot 11: SPR and fishing intensity plots ###
  if(11 %in% c(plot, print)){
    SSplotSPR(replist=replist,
              plot=(11 %in% plot),
              print=(11 %in% print),
              uncertainty=uncertainty,
              sprtarg=sprtarg, btarg=btarg,
              pwidth=pwidth, pheight=pheight, punits=punits,
              ptsize=ptsize, res=res,cex.main=cex.main,
              plotdir=plotdir)
  } # end if 11 in plot or print

  ### Plot 7: recruitment (moved to near S-R curve, but needs renumbering) ###
  if(7 %in% c(plot, print)){
    SSplotRecdevs(replist=replist,
                  plot=(7 %in% plot),
                  print=(7 %in% print),
                  forecastplot=forecastplot,
                  uncertainty=uncertainty,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,cex.main=cex.main,
                  plotdir=plotdir)

    if(nareas>1 & nseasons>1){
      SSplotRecdist(replist=replist,
                    plot=(7 %in% plot),
                    print=(7 %in% print),
                    verbose=verbose,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,cex.main=cex.main,
                    plotdir=plotdir)
    }
  } # end if 7 in plot or print

  ### Plot 26: estimating recruitment bias adjustment (probably needs renumbering) ###
  if(26 %in% c(plot, print) & uncertainty){
    if(max(rmse_table$RMSE)>0){
      SS_fitbiasramp(replist=replist,
                     plot=(26 %in% plot),
                     print=(26 %in% print),
                     twoplots=FALSE,
                     pwidth=pwidth, pheight=pheight, punits=punits,
                     ptsize=ptsize, res=res,cex.main=cex.main)
    }else{
      cat("Skipping bias adjustment fit because root mean squared error of recruit devs is 0.\n")
    }
  } # end if 26 in plot or print                   

  ### Plot 12: spawner-recruit curve ###
  if(12 %in% c(plot, print)){
    SSplotSpawnrecruit(replist=replist,
                       plot=(12 %in% plot),
                       print=(12 %in% print),
                       virg=TRUE,  # add point on curve at equilibrium values (B0,R0)
                       init=FALSE, # add point on curve at initial values (B1,R1)
                       pwidth=pwidth, pheight=pheight, punits=punits,
                       ptsize=ptsize, res=res,cex.main=cex.main,
                       plotdir=plotdir)

  } # end if 12 in plot or print

  ### Plot 13: CPUE plots ###
  if(13 %in% c(plot, print))
  {
    SSplotIndices(replist=replist,
                  fleets=fleets,
                  fleetnames=fleetnames,
                  plot=(13 %in% plot),
                  print=(13 %in% print),
                  datplot=datplot,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,cex.main=cex.main,
                  plotdir=plotdir,
                  minyr=minyr,
                  maxyr=maxyr)
  } # end if 13 in plot or print

  ### Plot 14: numbers at age ###
  if(14 %in% c(plot, print)){
    SSplotNumbers(replist=replist,
                  areas=areas,
                  areanames=areanames,
                  areacols=areacols,
                  pntscalar=pntscalar,
                  plot=(14 %in% plot),
                  print=(14 %in% print),
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,cex.main=cex.main,
                  plotdir=plotdir)
  } # close if 14 in plot or print

  ### Plot 15: Composition data plots ###
  # use of SSplotcomps function to make composition plots
  if(is.null(comp_data_exists) || !comp_data_exists){
    cat("No composition data, skipping all composition plots\n")
  }else{
    if(!datplot)
    {
      if(length(intersect(15:17,c(plot,print)))>0)
        cat("skipped data-only plots 15-17 (comp data without fit) because input 'datplot=F'\n")
    }else{
      if(15 %in% c(plot,print))  # data only aspects
      {
        # length comp bar plot
        SSplotComps(replist=replist,datonly=TRUE,kind="LEN",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(15%in%print),plot=(15%in%plot),plotdir=plotdir,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
        # size comp bubble plot
        for(sizemethod in sort(unique(replist$sizedbase$method))){
          SSplotComps(replist=replist,datonly=TRUE,kind="SIZE",sizemethod=sizemethod,
                      bub=TRUE,verbose=verbose,fleets=fleets,fleetnames=fleetnames,
                      samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                      minnbubble=minnbubble, pntscalar=pntscalar,
                      maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                      print=(15%in%print),plot=(15%in%plot),plotdir=plotdir,cex.main=cex.main,
                      scalebins=scalebins,
                      pwidth=pwidth, pheight=pheight, punits=punits,
                      ptsize=ptsize, res=res,
                      ...)
    
        }
        if(verbose) cat("Finished plot 15: length and size comp data\n")
        flush.console()
      }
      if(16 %in% c(plot,print)){
        # age comp bar plot
        SSplotComps(replist=replist,datonly=TRUE,kind="AGE",bub=FALSE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(16%in%print),plot=(16%in%plot),plotdir=plotdir,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
        # age comp bubble plot
        SSplotComps(replist=replist,datonly=TRUE,kind="AGE",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(16%in%print),plot=(16%in%plot),plotdir=plotdir,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
        # ghost age comp bar plot
        SSplotComps(replist=replist,datonly=TRUE,kind="GSTAGE",bub=FALSE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=samplesizeplots,showsampsize=FALSE,showeffN=FALSE,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(16%in%print),plot=(16%in%plot),plotdir=plotdir,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
        # ghost age comp bubble plot
        SSplotComps(replist=replist,datonly=TRUE,kind="GSTAGE",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=samplesizeplots,showsampsize=FALSE,showeffN=FALSE,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(16%in%print),plot=(16%in%plot),plotdir=plotdir,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
        if(verbose) cat("Finished plot 16: age comp data\n")
        flush.console()
      }
      if(17 %in% c(plot,print)){
        # conditional age plot
        SSplotComps(replist=replist,datonly=TRUE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=FALSE,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,
                    fixdims=fixdims,rows=rows,cols=cols,
                    print=(17%in%print),plot=(17%in%plot),plotdir=plotdir,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
        if(verbose) cat("Finished plot 17: conditional age at length data\n")
        flush.console()
      }
    } # end if data plot
    
    # plot 18: length comp data with fits, sample size, etc.
    if(18 %in% c(plot,print)){
      SSplotComps(replist=replist,datonly=FALSE,kind="LEN",bub=TRUE,verbose=verbose,fleets=fleets,
                  fleetnames=fleetnames,
                  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                  minnbubble=minnbubble, pntscalar=pntscalar,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(18%in%print),plot=(18%in%plot),smooth=smooth,plotdir=plotdir,
                  maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,
                  scalebins=scalebins,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,
                  ...)
      SSplotComps(replist=replist,datonly=FALSE,kind="GSTLEN",bub=TRUE,verbose=verbose,fleets=fleets,
                  fleetnames=fleetnames,
                  samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                  minnbubble=minnbubble, pntscalar=pntscalar,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(18%in%print),plot=(18%in%plot),smooth=smooth,plotdir=plotdir,
                  maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,
                  scalebins=scalebins,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,
                  ...)
      for(sizemethod in sort(unique(replist$sizedbase$method))){
        SSplotComps(replist=replist,datonly=FALSE,kind="SIZE",sizemethod=sizemethod,
                    bub=TRUE,verbose=verbose,fleets=fleets, fleetnames=fleetnames,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                    print=(18%in%print),plot=(18%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,cohortlines=cohortlines,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
      }
      if(verbose) cat("Finished plot 18: length and size comps with fits\n")
      flush.console()
    }
    
    # plot 19: age comp data with fits, sample size, etc.
    if(19 %in% c(plot,print)){
      SSplotComps(replist=replist,datonly=FALSE,kind="AGE",bub=TRUE,verbose=verbose,fleets=fleets,
                  fleetnames=fleetnames,
                  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                  minnbubble=minnbubble, pntscalar=pntscalar,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(19%in%print),plot=(19%in%plot),smooth=smooth,plotdir=plotdir,
                  maxneff=maxneff,cex.main=cex.main,
                  scalebins=scalebins,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,
                  ...)
      SSplotComps(replist=replist,datonly=FALSE,kind="GSTAGE",bub=TRUE,verbose=verbose,fleets=fleets,
                  fleetnames=fleetnames,
                  samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                  minnbubble=minnbubble, pntscalar=pntscalar,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(19%in%print),plot=(19%in%plot),smooth=smooth,plotdir=plotdir,
                  maxneff=maxneff,cex.main=cex.main,
                  scalebins=scalebins,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,
                  ...)
      if(verbose) cat("Finished plot 19: age comps with fits\n")
      flush.console()
    } # end if 19 in plot or print
    
    # plot 20: conditional age at length plot with fits, sample size, etc.
    if(20 %in% c(plot,print)){
      if(aalresids==TRUE){
        SSplotComps(replist=replist,subplots=3,datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,rows=rows,cols=cols,
                    print=(20%in%print),plot=(20%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
      }
      # conditional age at length for a given year
      if(length(intersect(aalyear, unique(timeseries$Yr)))>0){
        SSplotComps(replist=replist,subplots=4:5,datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    aalbin=aalbin,aalyear=aalyear,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,rows=rows,cols=cols,
                    print=(20%in%print),plot=(20%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
      }
      # conditional age at length for a given length bin
      if(length(intersect(aalbin, unique(lbins)))>0){
        SSplotComps(replist=replist,subplots=6,datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                    fleetnames=fleetnames,
                    aalbin=aalbin,
                    samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                    minnbubble=minnbubble, pntscalar=pntscalar,
                    maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,rows=rows,cols=cols,
                    print=(20%in%print),plot=(20%in%plot),smooth=smooth,plotdir=plotdir,
                    maxneff=maxneff,cex.main=cex.main,
                    scalebins=scalebins,
                    pwidth=pwidth, pheight=pheight, punits=punits,
                    ptsize=ptsize, res=res,
                    ...)
      }
      if(verbose) cat("Finished plot 20: conditional age at length with fits\n")
    } #end if 20 in plot or print
    
    if(21 %in% c(plot,print)){
      # plot 21: Andre's new conditional age-at-length plots
      SSplotComps(replist=replist,subplots=8,datonly=FALSE,kind="cond",bub=TRUE,verbose=verbose,fleets=fleets,
                  fleetnames=fleetnames,
                  aalbin=aalbin,aalyear=aalyear,
                  samplesizeplots=samplesizeplots,showsampsize=showsampsize,showeffN=showeffN,
                  minnbubble=minnbubble, pntscalar=pntscalar,
                  maxrows=maxrows,maxcols=maxcols,maxrows2=maxrows2,maxcols2=maxcols2,fixdims=fixdims,rows=rows,cols=cols,
                  print=(21%in%print),plot=(21%in%plot),smooth=smooth,plotdir=plotdir,
                  maxneff=maxneff,cex.main=cex.main,
                  scalebins=FALSE,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,
                  ...)
      if(nrow(replist$condbase)>0 & verbose){
        cat("Finished plot 21: mean age and std. dev. in conditional AAL\n",
            "  This is a new plot, currently in beta mode.\n",
            "  Left plots are mean AAL by size-class (obs. and pred.)\n",
            "  with 90% CIs based on adding 1.64 SE of mean to the data\n",
            "  Right plots in each pair are SE of mean AAL (obs. and pred.)\n",
            "  with 90% CIs based on the chi-square distribution.\n")
      }
    } # end if 21 in plot or print
    # plot 22: mean length at age and mean weight at age
    if(22 %in% c(plot,print)){
      SSplotComps(replist=replist,datonly=FALSE,kind="L@A",bub=TRUE,verbose=verbose,fleets=fleets,
                  fleetnames=fleetnames,
                  samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                  minnbubble=minnbubble, pntscalar=pntscalar,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(22%in%print),plot=(22%in%plot),smooth=smooth,plotdir=plotdir,
                  maxneff=maxneff,cex.main=cex.main,
                  scalebins=scalebins,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,
                  ...)
      SSplotComps(replist=replist,datonly=FALSE,kind="W@A",bub=TRUE,verbose=verbose,fleets=fleets,
                  fleetnames=fleetnames,
                  samplesizeplots=FALSE,showsampsize=FALSE,showeffN=FALSE,
                  minnbubble=minnbubble, pntscalar=pntscalar,
                  maxrows=maxrows,maxcols=maxcols,fixdims=fixdims,rows=rows,cols=cols,
                  print=(22%in%print),plot=(22%in%plot),smooth=smooth,plotdir=plotdir,
                  maxneff=maxneff,cex.main=cex.main,
                  scalebins=scalebins,
                  pwidth=pwidth, pheight=pheight, punits=punits,
                  ptsize=ptsize, res=res,
                  ...)

      if(verbose) cat("Finished plot 22: mean length at age and mean weight at age\n")
      flush.console()
    } # end if 22 in plot or print
  }

  # restore default single panel settings if needed
  # conditional because if adding to existing plot may mess up layout
  if(FALSE %in% (par()$mfcol == c(rows,cols))) par(mfcol=c(rows,cols))
  if(FALSE %in% (par()$mar == c(5,4,4,2)+.1)) par(mar=c(5,4,4,2)+.1, oma=rep(0,4))

  # Yield curve
  if(23 %in% c(plot, print)){
    SSplotYield(replist=replist,
                plot=(23 %in% plot),
                print=(23 %in% print),
                pwidth=pwidth, pheight=pheight, punits=punits,
                ptsize=ptsize, res=res, cex.main=cex.main,
                plotdir=plotdir)
  } # close plot section 23

  ### Plot 24: Tag plots ###
  if(24 %in% c(plot, print)){
    SSplotTags(replist=replist,
               rows=rows,cols=cols,
               tagrows=tagrows,tagcols=tagcols,
               pntscalar=pntscalar,minnbubble=minnbubble,
               plot=(24 %in% plot),
               print=(24 %in% print),
               pwidth=pwidth, pheight=pheight, punits=punits,
               ptsize=ptsize, res=res, cex.main=cex.main,
               plotdir=plotdir)
  } # end if 24 in plot or print

  ### Plot 25: Movement rates ###
  if(25 %in% plot & nrow(replist$movement)>0)
    SSplotMovementRates(replist=replist,cex.main=cex.main)
  # note! need to add png output for this funciton
  # end if 25 in plot or print

  ### Plot 27: Data availability ###
  if(27 %in% c(plot, print))
    SSplotData(replist=replist,
               plot=(27 %in% plot),
               print=(27 %in% print),
               pwidth=pwidth, pheight=pheight, punits=punits,
               ptsize=ptsize, res=res, cex.main=cex.main,
               plotdir=plotdir, margins=c(5.1,2.1,4.1,SSplotDatMargin),
               fleetnames=fleetnames)
  # end if 27 in plot or print
  
  if(pdf) dev.off() # close PDF file if it was open
  if(verbose) cat("Finished all requested plots in SS_plots_old function\n")

  cat("\nNote: this function is no longer in development. Try 'SS_plots' instead.\n")
  ### end of SS_plots function
  return(invisible(999))
}
