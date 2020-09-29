#' plot many quantities related to output from Stock Synthesis
#'
#' Creates a user-chosen set of plots, including biological quantities, time
#' series, and fits to data.  Plots are sent to R GUI, single PDF file, or
#' multiple PNG files. This is now just a wrapper which calls on separate
#' functions to make all the plots.
#'
#'
#' @param replist List created by \code{SS_output}
#' @param plot Plot sets to be created, see list of plots below.  Use to
#' specify only those plot sets of interest, e.g., c(1,2,5,10). Plots for data
#' not available in the model run will automatically be skipped, whether called
#' or not.
#' Current grouping of plots is as follows:
#' \enumerate{
#'   \item Biology
#'   \item Selectivity and retention
#'   \item Timeseries
#'   \item Recruitment deviations
#'   \item Recruitment bias adjustment
#'   \item Spawner-recruit
#'   \item Catch
#'   \item SPR
#'   \item Discards
#'   \item Mean weight
#'   \item Indices
#'   \item Numbers at age
#'   \item Length comp data
#'   \item Age comp data
#'   \item Conditional age-at-length data
#'   \item Length comp fits
#'   \item Age comp fits
#'   \item Conditional age-at-length fits
#'   \item Francis and Punt conditional age-at-length comp fits
#'   \item Mean length-at-age and mean weight-at-age
#'   \item Tags
#'   \item Yield
#'   \item Movement
#'   \item Data range
#'   \item Parameter distributions
#'   \item Diagnostic tables
#' }
#'
#' @param print Deprecated input for backward compatibility, now replaced by
#' \code{png = TRUE/FALSE}.
#' @param pdf Send plots to PDF file instead of R GUI?
#' @param png Send plots to PNG files instead of R GUI?
#' @param html Run \code{\link{SS_html}} on completion? By default has same
#' value as \code{png}.
#' @param printfolder The sub-directory under 'dir' (see below) in which the
#' PNG files will be located.  The default sub-directory is "plots".
#' The directory will be created if it doesn\'t exist.
#' If 'printfolder' is set to "", it is ignored and the PNG files will be located
#' in the directory specified by 'dir'.
#' @param dir The directory in which a PDF file (if requested) will be created
#' and within which the printfolder sub-directory (see above) will be created
#' if png=TRUE. By default it will be the same directory that the report file
#' was read from by the \code{SS_output} function. Alternatives to the default
#' can be either relative (to the working directory) or absolute paths.
#' The function will attempt to create the directory it doesn't exist, but it
#' does not do so recursively.
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
#' (requires positive definite hessian in model?  Default=TRUE.
#' @param forecastplot Include forecast years in the timeseries plots and
#' plots of time-varying quantities? Default=TRUE.
#' @param datplot Plot the data by itself? This is useful in document
#' preparation, but doesn't change across alternative model runs with the same
#' data, so can be committed to save time once the plots have been created once.
#' Setting datplot=FALSE is equivalent to leaving off plots 15 and 16.
#' Default=TRUE.
#' @param Natageplot Plot the expected numbers at age bubble plots and mean-age
#' time series?  Default=T.
#' @param samplesizeplots Show sample size plots?  Default=T.
#' @param compresidplots Show residuals for composition plots?
#' @param comp.yupper Upper limit on ymax for polygon/histogram composition
#' plots. This avoids scaling all plots to have max=1 if there is a vector
#' with only a single observed fish in it. Default=0.4.
#' @param sprtarg Specify the F/SPR proxy target. Default=0.4.
#' @param btarg Target %unfished to be used in plots showing %unfished. May be
#' omitted by setting to NA.
#' @param minbthresh Threshold depletion to be used in plots showing depletion.
#' May be omitted by setting to NA.
#' @param pntscalar This scalar defines the maximum bubble size for bubble
#' plots. This option is still available but a better choice is to use
#' bub.scale.pearson and bub.scale.dat, which are allow the same scaling
#' throughout all plots.
#' @param pntscalar.nums This scalar defines the maximum bubble size for
#' numbers-at-age and numbers-at-length plots.
#' @param pntscalar.tags This scalar defines the maximum bubble size for
#' tagging plots.
#' @param bub.scale.pearson Character expansion (cex) value for a proportion of
#' 1.0 in bubble plot of Pearson residuals. Default=1.5.
#' @param bub.scale.dat Character expansion (cex) value for a proportion of 1.0
#' in bubble plot of composition data. Default=3.
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
#' residuals? Turn to FALSE if plots are taking too long and you don't want
#' them.
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
#' @param sampsizeline show line for input sample sizes on top of conditional
#' age-at-length plots (TRUE/FALSE, still in development)
#' @param effNline show line for effective sample sizes on top of conditional
#' age-at-length plots (TRUE/FALSE, still in development)
#' @param showlegend Display legends in various plots? Default=T.
#' @param pwidth Width of plots printed to files in units of
#' \code{punits}. Default recently changed from 7 to 6.5.
#' @param pheight Height width of plots printed to files in units of
#' \code{punits}. Default recently changed from 7 to 5.0
#' @param punits Units for \code{pwidth} and \code{pheight}. Can be "px"
#' (pixels), "in" (inches), "cm" or "mm". Default="in".
#' @param ptsize Point size for plotted text in plots printed to files (see
#' help("png") in R for details). Default recently changed from 12 to 10.
#' @param res Resolution of plots printed to files. Default=300.
#' @param mainTitle Logical indicating if a title should be included at the top
#' @param cex.main Character expansion parameter for plot titles (not yet
#' implemented for all plots). Default=1.
#' @param selexlines Vector controlling which lines should be shown on
#' selectivity plots if the model includes retention. Default=1:5.
#' @param rows Number of rows to use for single panel plots. Default=1.
#' @param cols Number of columns to use for single panel plots. Default=1.
#' @param maxrows Maximum number of rows to for multi-panel plots.  Default=4.
#' @param maxcols Maximum number of columns for multi-panel plots.  Default=4.
#' @param maxrows2 Maximum number of rows for conditional age-at-length
#' multi-panel plots. Default=2.
#' @param maxcols2 Maximum number of rows for conditional age-at-length
#' multi-panel plots. Default=4.
#' @param andrerows Number of rows of Andre's conditional age-at-length plots
#' within each page. Default=3.
#' @param tagrows Number of rows for tagging-related plots. Default=3.
#' @param tagcols Number of columns for tagging-related plots.  Default=3.
#' @param parrows Number of rows for parameter distribution plots.
#' @param parcols Number of columns for parameter distribution plots.
#' @param fixdims Control whether multi-panel plots all have dimensions equal
#' to maxrows by maxcols, or resized within those limits to fit number of
#' plots. Default=T.
#' @param new Open a new window or add to existing plot windows.  Default=T.
#' @param SSplotDatMargin Size of right-hand margin in data plot (may be too
#' small if fleet names are long)
#' @param filenotes Optional vector of character strings to be added to intro
#' HTML page (if created) with notes about the model.
#' @param catchasnumbers Is catch input in numbers instead of biomass?
#' Default=F.
#' @param catchbars show catch by fleet as barplot instead of stacked polygons
#' (default=TRUE)
#' @param legendloc Location for all legends. Default="topleft".
#' @param minyr First year to show in time-series and time-varying plots
#' @param maxyr Last year to show in time-series and time-varying plots. This
#' can either be an alternative to, or redundant with, the forecastplot input.
#' @param sexes Which sexes to show in composition plots. Default="all".
#' @param scalebins Rescale expected and observed proportions in composition
#' plots by dividing by bin width for models where bins have different widths?
#' Caution!: May not work correctly in all cases.
#' @param scalebubbles scale data-only bubbles by sample size, not just
#' proportion within sample? Default=FALSE.
#' @param tslabels Either NULL to have default labels for timeseries plots or
#' a vector of appropriate length (currently 11) with labels for each figure
#' @param catlabels Either NULL to have default labels for catch plots or
#' a vector of appropriate length (currently 10) with labels for each figure
#' @param maxsize The size of the largest bubble in the datasize
#' plot. Default is 1.0.
#' @param showmle Show MLE estimate and asymptotic variance estimate with blue
#' lines in the parameter distribution plots?
#' @param showprior Show prior distribution as black line in the parameter
#' distribution plots?
#' @param showpost Show posterior distribution as bar graph in parameter
#' distribution plots (requires MCMC results to be available in \code{replist})?
#' @param showinit Show initial value as red triangle in the parameter
#' distribution plots?
#' @param showdev Include devs in the parameter distribution plots?
#' @param fitrange Fit range in parameter distribution plots tightly around MLE
#' and posterior distributions instead of full parameter range?
#' @param \dots Additional arguments that will be passed to some subfunctions.
#' @author Ian Stewart, Ian Taylor
#' @export
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
SS_plots <-
  function(
           replist = NULL, plot = 1:26, print = NULL, pdf = FALSE, png = TRUE, html = png,
           printfolder = "plots", dir = "default", fleets = "all", areas = "all",
           fleetnames = "default", fleetcols = "default", fleetlty = 1, fleetpch = 1,
           lwd = 1, areacols = "default", areanames = "default",
           verbose = TRUE, uncertainty = TRUE, forecastplot = TRUE,
           datplot = TRUE, Natageplot = TRUE, samplesizeplots = TRUE, compresidplots = TRUE,
           comp.yupper = 0.4,
           sprtarg = "default", btarg = "default", minbthresh = "default", pntscalar = NULL,
           bub.scale.pearson = 1.5, bub.scale.dat = 3, pntscalar.nums = 2.6,
           pntscalar.tags = 2.6, minnbubble = 8, aalyear = -1, aalbin = -1, aalresids = TRUE,
           maxneff = 5000, cohortlines = c(), smooth = TRUE, showsampsize = TRUE,
           showeffN = TRUE, sampsizeline = FALSE, effNline = FALSE,
           showlegend = TRUE, pwidth = 6.5, pheight = 5.0, punits = "in", ptsize = 10, res = 300,
           mainTitle = FALSE, cex.main = 1, selexlines = 1:6, rows = 1, cols = 1,
           maxrows = 4, maxcols = 4, maxrows2 = 2, maxcols2 = 4, andrerows = 3,
           tagrows = 3, tagcols = 3, parrows = 2, parcols = 2, fixdims = TRUE, new = TRUE,
           SSplotDatMargin = 8, filenotes = NULL, catchasnumbers = NULL, catchbars = TRUE,
           legendloc = "topleft", minyr = -Inf, maxyr = Inf, sexes = "all", scalebins = FALSE,
           scalebubbles = FALSE, tslabels = NULL, catlabels = NULL, maxsize = 1.0,
           showmle = TRUE, showpost = TRUE, showprior = TRUE, showinit = TRUE, showdev = FALSE,
           fitrange = FALSE, ...) {
    if (!is.null(print)) {
      stop(
        "The 'print' input has been replaced by 'png = TRUE/FALSE'\n",
        "  which is combined with the vector of numbers input to 'plot'"
      )
    }
    flush.console()

    # label table is a step toward internationalization of the code
    # in the future, this could be read from a file, or we could have multiple columns
    # in the table to choose from

    if (is.null(replist) || !is.list(replist) || !"nfleets" %in% names(replist)) {
      stop(
        "The input 'replist' should refer to an R object created by",
        " the function 'SS_output'."
      )
    }


    # get quantities from the big list
    nfleets <- replist$nfleets
    nfishfleets <- replist$nfishfleets
    nareas <- replist$nareas
    nseasons <- replist$nseasons
    timeseries <- replist$timeseries
    lbins <- replist$lbins
    inputs <- replist$inputs
    endyr <- replist$endyr
    SS_version <- replist$SS_version
    SS_versionNumeric <- replist$SS_versionNumeric
    StartTime <- replist$StartTime
    Files_used <- replist$Files_used
    FleetNames <- replist$FleetNames
    rmse_table <- replist$rmse_table
    comp_data_exists <- replist$comp_data_exists

    # check for internal consistency
    if (pdf & png) {
      stop("Inputs 'pdf' and 'png' are mututally exclusive. You need to set one of them to FALSE")
    }
    if (html & !png) {
      stop("You can't set 'html=TRUE' without also setting 'png=TRUE'")
    }
    if (uncertainty & !inputs$covar) {
      warning("covar information unavailable, changing 'uncertainty' to FALSE")
      uncertainty <- FALSE
    }
    if (forecastplot & max(timeseries$Yr > endyr + 1) == 0) {
      message("Changing 'forecastplot' input to FALSE because all years up to endyr+1 are included by default")
      forecastplot <- FALSE
    }

    # derived quantities
    if (fleets[1] == "all") {
      fleets <- 1:nfleets
    } else {
      if (length(intersect(fleets, 1:nfleets)) != length(fleets)) {
        return("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
      }
    }
    if (areas[1] == "all") {
      areas <- 1:nareas
    } else {
      if (length(intersect(areas, 1:nareas)) != length(areas)) {
        return("Input 'areas' should be 'all' or a vector of values between 1 and nareas.")
      }
    }

    if (verbose) {
      message("Finished defining objects")
    }

    # set fleet-specific names, and plotting parameters
    if (fleetnames[1] == "default") {
      fleetnames <- FleetNames
    }
    if (fleetcols[1] == "default") {
      fleetcols <- rich.colors.short(nfishfleets)
      if (nfishfleets > 2) fleetcols <- rich.colors.short(nfishfleets + 1)[-1]
    }
    if (length(fleetlty) < nfishfleets) {
      fleetlty <- rep(fleetlty, nfishfleets)
    }
    if (length(fleetpch) < nfishfleets) {
      fleetpch <- rep(fleetpch, nfishfleets)
    }
    # set default area-specific colors if not specified
    if (areacols[1] == "default") {
      areacols <- rich.colors.short(nareas)
      if (nareas == 3) {
        areacols <- c("blue", "red", "green3")
      }
      if (nareas > 3) {
        areacols <- rich.colors.short(nareas + 1)[-1]
      }
    }

    #### prepare for plotting

    # number of plot groups
    nplots <- length(intersect(1:50, plot))

    # make plot window (hopefully no-longer operating system specific)
    if (nplots > 0 & !png & !pdf & new) {
      ### Note: the following line has been commented out because it was identified
      ###       by Brian Ripley as "against CRAN policies".
      # if(exists(".SavedPlots",where=1)) rm(.SavedPlots,pos=1)
      dev.new(width = pwidth, height = pheight, pointsize = ptsize, record = TRUE)
    }
    if (nplots > 0 & !new) {
      if (verbose) {
        message("Adding plots to existing plot window. Plot history not erased.")
      }
    }

    ### deal with directories in which to create PNG or PDF files
    if (dir == "default") {
      # directory within which printfolder will be created
      # by default it is assumed to be the location of the model files
      dir <- inputs$dir
    }
    if (png | pdf) {
      # get info on directory where subfolder will go
      # (typically folder with model output files)
      dir.isdir <- file.info(dir)$isdir
      # create directory
      if (is.na(dir.isdir) | !dir.isdir) {
        message("Directory doesn't exist, attempting to create:\n", dir)
        dir.create(dir)
      }
      # test again (even though failure to create dir should have already caused error)
      # get info on directory where subfolder will go
      # (typically folder with model output files)
      dir.isdir <- file.info(dir)$isdir
      # create
      if (is.na(dir.isdir) | !dir.isdir) {
        stop("Not able to create directory:\n", dir, "\n")
      }
    }

    plotdir <- "default" # dummy value passed to functions that ignore it if png=FALSE
    if (png) {
      # add subdirectory for PNG and HTML files if that option is chosen

      # close any in-process plots still open
      graphics.off()

      # figure out path to where PNG files will go
      plotdir <- file.path(dir, printfolder)
      plotdir.isdir <- file.info(plotdir)$isdir
      if (is.na(plotdir.isdir) | !plotdir.isdir) {
        dir.create(plotdir)
      }
      if (verbose) {
        message(
          "Plots will be written to PNG files in the directory:\n  ",
          plotdir
        )
      }
      # get info on any older plots inside the plotdir directory
      csv.files <- grep("plotInfoTable.+csv", dir(plotdir), value = TRUE)
      if (length(csv.files) > 0) {
        StartTimes.old <- NULL
        for (ifile in 1:length(csv.files)) {
          plotInfo.old <- read.csv(file.path(plotdir, csv.files[ifile]),
            stringsAsFactors = FALSE
          )
          StartTimes.old <- c(StartTimes.old, unique(plotInfo.old$StartTime))
        }
        if (any(StartTimes.old != StartTime)) {

          # if there are plots that are older than those from the current model,
          # rename the directory to something containing the older model start time
          StartTimeName <- gsub(":", ".", StartTimes.old[1], fixed = TRUE)
          StartTimeName <- gsub(" ", "_", StartTimeName, fixed = TRUE)
          StartTimeName <- gsub("._", "_", StartTimeName, fixed = TRUE)
          plotdir.old <- file.path(dir, paste0("plots_", StartTimeName))
          message(
            "NOTE: the directory\n   ",
            plotdir,
            "\n  contains plots from a previous model run, renaming to\n   ",
            plotdir.old
          )
          file.rename(plotdir, plotdir.old)
          # create a new, empty directory for the new plots
          dir.create(plotdir)
        }
      }
    }

    # create PDF file if requested
    if (pdf) {
      pdffile <- file.path(dir, paste0(
        "SS_plots_",
        format(Sys.time(), "%d-%m-%Y_%H.%M"),
        ".pdf"
      ))
      pdf(file = pdffile, width = pwidth, height = pheight)
      if (verbose) {
        message("PDF file with plots will be:", pdffile)
      }
    }

    # blank table to store plot info
    plotInfoTable <- NULL

    if (new & !png) {
      # make multi-panel plot if requested (not available for PNG files)
      par(mfcol = c(rows, cols))
    }
    if (pdf) {
      mar0 <- par()$mar # current margins
      par(mar = rep(0, 4))
      plot(0, type = "n", xlab = "", ylab = "", axes = FALSE, xlim = c(0, 1), ylim = c(0, 1))
      y <- 0.9
      ystep <- -.05
      text(0, y, "Plots created using the 'r4ss' package in R", pos = 4)
      y <- y + ystep
      text(0, y, paste("Stock Synthesis version:", substr(SS_version, 1, 9)), pos = 4)
      y <- y + ystep
      text(0, y, StartTime, pos = 4)
      y <- y + ystep
      Files2 <- strsplit(Files_used, " ")[[1]]
      text(0, y, paste(Files2[[1]], Files2[2]), pos = 4)
      y <- y + ystep
      text(0, y, paste(Files2[[3]], Files2[4]), pos = 4)
      if (!is.null(filenotes)) {
        y <- y + ystep
        text(0, y, "Notes:", pos = 4)
        for (i in 1:length(filenotes)) {
          y <- y + ystep
          text(0, y, filenotes[i], pos = 4)
        }
      }
      par(mar = mar0) # replace margins
    }
    mar0 <- par()$mar # current inner margins
    oma0 <- par()$oma # current outer margins

    if (length(tslabels) == 0) {
      tslabels <- c(
        "Total biomass (mt)", # 1
        "Total biomass (mt) at beginning of season", # 2
        "Summary biomass (mt)", # 3
        "Summary biomass (mt) at beginning of season", # 4
        "Spawning biomass (mt)", # 5
        "Fraction of unfished", # 6
        "Spawning output", # 7
        "Age-0 recruits (1,000s)", # 8
        "Fraction of total Age-0 recruits", # 9
        "Management target", # 10
        "Minimum stock size threshold"
      ) # 11
    }

    if (length(catlabels) == 0) {
      catlabels <- c(
        "Harvest rate/Year", # 1
        "Continuous F", # 2
        "Landings", # 3
        "Total catch", # 4
        "Predicted Discards", # 5  # should add units
        "Discard fraction", # 6  # need to add by weight or by length
        "(mt)", # 7
        "(numbers x1000)", # 8
        "Observed and expected", # 9
        "aggregated across seasons"
      ) # 10
    }

    ##########################################
    # Biology plots (mean weight, maturity, fecundity, spawning output)
    # and Time-varying growth
    #
    igroup <- 1
    if (igroup %in% plot | length(cohortlines) > 0) {
      if (verbose) {
        message("Starting biology plots (group ", igroup, ")")
      }
      plotinfo <- SSplotBiology(
        replist = replist,
        forecast = forecastplot, minyr = minyr, maxyr = maxyr,
        plot = !png, print = png,
        pwidth = pwidth, pheight = pheight, punits = punits,
        ptsize = ptsize, res = res, mainTitle = mainTitle,
        cex.main = cex.main, plotdir = plotdir
      )
      if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
    }

    ##########################################
    # Selectivity and retention plots
    #
    igroup <- 2
    if (igroup %in% plot) {
      if (verbose) {
        message("Starting selectivity and retention plots (group ", igroup, ")")
      }
      selexinfo <-
        SSplotSelex(
          replist = replist, selexlines = selexlines,
          fleets = fleets, fleetnames = fleetnames,
          minyr = minyr, maxyr = maxyr,
          plot = !png, print = png,
          pwidth = pwidth, pheight = pheight, punits = punits,
          ptsize = ptsize, res = res, cex.main = cex.main,
          plotdir = plotdir
        )
      plotinfo <- selexinfo$plotinfo
      if (!is.null(plotinfo)) {
        plotInfoTable <- rbind(plotInfoTable, plotinfo)
      }

      # add plots of unavailable (cryptic) spawning output
      if (FALSE) {
        # needs revision to work in SS 3.30 and models with multiple birth seasons
        plotinfo <-
          SSunavailableSpawningOutput(
            replist = replist,
            plot = !png, print = png,
            plotdir = plotdir,
            pwidth = pwidth, pheight = pheight,
            punits = punits, res = res,
            ptsize = ptsize, cex.main = cex.main
          )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
      }
    } # end if igroup in plot or print

    ##########################################
    # Basic time series
    #
    igroup <- 3
    if (igroup %in% plot) {
      if (verbose) {
        message("Starting timeseries plots (group ", igroup, ")")
      }
      for (isubplot in 1:15) { # which of 12 subplots to make
        for (doforecast in unique(c(FALSE, forecastplot))) { # add forecast or not
          if (isubplot %in% c(7, 9, 11)) {
            for (douncertainty in unique(c(FALSE, uncertainty))) { # add uncertainty or not
              plotinfo <-
                SSplotTimeseries(
                  replist = replist,
                  subplot = isubplot,
                  areas = areas,
                  areacols = areacols,
                  areanames = areanames,
                  forecastplot = doforecast,
                  uncertainty = douncertainty,
                  plot = !png, print = png,
                  verbose = verbose,
                  btarg = btarg,
                  minbthresh = minbthresh,
                  minyr = minyr, maxyr = maxyr,
                  pwidth = pwidth, pheight = pheight, punits = punits,
                  ptsize = ptsize, res = res, cex.main = cex.main,
                  labels = tslabels,
                  plotdir = plotdir
                )
              if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
            } # end loop over uncertainty or not
          } else { # these plots don't have the option for uncertainty
            plotinfo <-
              SSplotTimeseries(
                replist = replist,
                subplot = isubplot,
                areas = areas,
                areacols = areacols,
                areanames = areanames,
                forecastplot = doforecast,
                uncertainty = FALSE,
                plot = !png, print = png,
                verbose = verbose,
                btarg = btarg,
                minbthresh = minbthresh,
                minyr = minyr, maxyr = maxyr,
                pwidth = pwidth, pheight = pheight, punits = punits,
                ptsize = ptsize, res = res, cex.main = cex.main,
                labels = tslabels,
                plotdir = plotdir
              )
            if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
          }
        }
      } # end loop over timeseries subplots

      ### add plot of Summary F
      # first get vector of years
      yrs <- replist$startyr:replist$endyr
      yrs <- yrs[yrs >= minyr & yrs <= maxyr]
      # now run plot function
      plotinfo <- SSplotSummaryF(
        replist = replist,
        yrs = yrs,
        uncertainty = uncertainty,
        plot = !png, print = png,
        verbose = verbose,
        pwidth = pwidth, pheight = pheight, punits = punits,
        ptsize = ptsize, res = res,
        plotdir = plotdir
      )
      if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
    } # end if igroup in plot or print

    ##########################################
    # Recruitment deviation plots
    #
    igroup <- 4
    if (igroup %in% plot) {
      if (verbose) {
        message("Starting recruitment deviation plots (group ", igroup, ")")
      }
      plotinfo <-
        SSplotRecdevs(
          replist = replist,
          plot = !png, print = png,
          forecastplot = forecastplot,
          uncertainty = uncertainty,
          pwidth = pwidth, pheight = pheight, punits = punits,
          ptsize = ptsize, res = res, cex.main = cex.main,
          plotdir = plotdir
        )
      if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)

      if (nareas > 1 & nseasons > 1) {
        plotinfo <-
          SSplotRecdist(
            replist = replist,
            plot = !png, print = png,
            verbose = verbose,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res, cex.main = cex.main,
            plotdir = plotdir
          )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
      }
    } # end if igroup in plot or print

    ##########################################
    # Estimating recruitment bias adjustment plots
    #
    igroup <- 5
    if (igroup %in% plot) {
      if (uncertainty) {
        if (verbose) {
          message("Starting estimation of recruitment bias adjustment and associated plots (group ", igroup, ")")
        }
        if (is.numeric(rmse_table$RMSE)) {
          if (max(rmse_table$RMSE) > 0) {
            temp <-
              SS_fitbiasramp(
                replist = replist,
                plot = !png, print = png,
                twoplots = FALSE,
                pwidth = pwidth, pheight = pheight, punits = punits,
                ptsize = ptsize, res = res, cex.main = cex.main,
                plotdir = plotdir
              )
            plotinfo <- temp$plotinfo
            if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
          } else {
            message("Skipping bias adjustment fit because root mean squared error of recruit devs is 0.")
          }
        } else {
          message(
            "skipping bias adjustment fit because\n",
            "input list element 'rmse_table' has non-numeric 'RMSE' column"
          )
        }
      } else {
        if (verbose) {
          message("Skipping estimation of recruitment bias adjustment (group ", igroup, ") because uncertainty=FALSE")
        }
      }
    } # end if igroup in plot or print

    ##########################################
    # spawner-recruit curve
    #
    igroup <- 6
    if (igroup %in% plot) {
      if (verbose) {
        message("Starting spawner-recruit curve plot (group ", igroup, ")")
      }
      plotinfo <-
        SSplotSpawnrecruit(
          replist = replist,
          plot = !png, print = png,
          virg = TRUE, # add point on curve at equilibrium values (B0,R0)
          init = FALSE, # add point on curve at initial values (B1,R1)
          pwidth = pwidth, pheight = pheight, punits = punits,
          ptsize = ptsize, res = res,
          plotdir = plotdir
        )
      if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
    } # end if igroup in plot or print

    ##########################################
    # time series of catch
    #
    igroup <- 7
    if (igroup %in% plot) {
      if (verbose) {
        message("Starting catch plots (group ", igroup, ")")
      }
      temp <-
        SSplotCatch(
          replist = replist,
          plot = !png, print = png,
          fleetnames = fleetnames,
          fleetlty = fleetlty,
          fleetpch = fleetpch,
          fleetcols = fleetcols,
          minyr = minyr, maxyr = maxyr,
          pwidth = pwidth, pheight = pheight, punits = punits,
          ptsize = ptsize, res = res,
          cex.main = cex.main,
          catchasnumbers = catchasnumbers,
          order = "default",
          catchbars = catchbars,
          labels = catlabels,
          legendloc = legendloc,
          plotdir = plotdir,
          verbose = verbose
        )
      plotinfo <- temp$plotinfo
      if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
    } # end if igroup in plot or print

    ##########################################
    # SPR and fishing intensity plots
    #
    igroup <- 8
    if (igroup %in% plot) {
      if (verbose) {
        message("Starting SPR plots (group ", igroup, ")")
      }
      plotinfo <-
        SSplotSPR(
          replist = replist,
          plot = !png, print = png,
          uncertainty = uncertainty,
          sprtarg = sprtarg, btarg = btarg,
          pwidth = pwidth, pheight = pheight, punits = punits,
          ptsize = ptsize, res = res, cex.main = cex.main,
          plotdir = plotdir
        )
      if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
    } # end if igroup in plot or print

    ##########################################
    # discard fit plots (if present)
    #
    igroup <- 9
    if (igroup %in% plot) {
      if (!is.na(replist$discard) && nrow(replist$discard) > 0) {
        if (verbose) {
          message("Starting discard plot (group ", igroup, ")")
        }
        plotinfo <-
          SSplotDiscard(
            replist = replist,
            plot = !png, print = png,
            fleets = fleets,
            fleetnames = fleetnames,
            datplot = datplot,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res, cex.main = cex.main,
            plotdir = plotdir
          )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
      } else {
        if (verbose) {
          message("Skipping discard plot (group ", igroup, ") because no discard data")
        }
      }
    } # end if igroup in plot or print

    ##########################################
    # mean body weight (if present)
    #
    igroup <- 10
    if (igroup %in% plot) {
      if (!is.na(replist$mnwgt) && nrow(replist$mnwgt) > 0) {
        if (verbose) {
          message("Starting mean body weight plot (group ", igroup, ")")
        }
        plotinfo <-
          SSplotMnwt(
            replist = replist,
            plot = !png, print = png,
            fleets = fleets,
            fleetnames = fleetnames,
            datplot = datplot,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res, cex.main = cex.main,
            plotdir = plotdir
          )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
      } else {
        if (verbose) {
          message("Skipping mean weight plot (group ", igroup, ") because no mean weight data")
        }
      }
    } # end if igroup in plot or print


    ##########################################
    # Index plots
    #
    igroup <- 11
    if (igroup %in% plot) {
      if (!is.null(dim(replist$cpue))) {
        if (verbose) {
          message("Starting index plots (group ", igroup, ")")
        }
        plotinfo <- SSplotIndices(
          replist = replist,
          fleets = fleets,
          fleetnames = fleetnames,
          plot = !png, print = png,
          datplot = datplot,
          pwidth = pwidth, pheight = pheight, punits = punits,
          ptsize = ptsize, res = res,
          mainTitle = mainTitle, cex.main = cex.main,
          plotdir = plotdir,
          minyr = minyr,
          maxyr = maxyr
        )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
      } else {
        if (verbose) {
          message(
            "Skipping index plots (group ", igroup,
            ") because no indices in model (or are not reported)"
          )
        }
      }
    } # end if igroup in plot or print

    ##########################################
    # Numbers at age plots
    #
    igroup <- 12
    if (igroup %in% plot) {
      if (!is.null(replist$natage)) {
        if (verbose) {
          message("Starting numbers at age plots (group ", igroup, ")")
        }
        plotinfo <-
          SSplotNumbers(
            replist = replist,
            areas = areas,
            areanames = areanames,
            areacols = areacols,
            pntscalar = pntscalar.nums,
            bublegend = showlegend,
            plot = !png, print = png,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res,
            mainTitle = mainTitle, cex.main = cex.main,
            plotdir = plotdir
          )
        if (!is.null(plotinfo)) {
          plotInfoTable <- rbind(plotInfoTable, plotinfo)
        }
      } else {
        message(
          "Skipping numbers plots (group ", igroup,
          ") because numbers-at-age table not included in output"
        )
        # end check for numbers-at-age table available
      }
    } # end if igroup in plot or print

    ##########################################
    # Composition data plots
    #
    # use of SSplotcomps function to make composition plots
    if (is.null(comp_data_exists) || !comp_data_exists) {
      message("No composition data, skipping all composition plots")
    } else {
      lenCompDatGroup <- 13
      ageCompDatGroup <- 14
      condCompDatGroup <- 15
      if (!datplot) {
        if (length(intersect(
          c(lenCompDatGroup, ageCompDatGroup, condCompDatGroup),
          plot
        )) > 0) {
          message(
            "Skipping plot groups ",
            lenCompDatGroup,
            "-",
            condCompDatGroup,
            " (comp data without fit) because input 'datplot=FALSE'"
          )
        }
      } else {
        if (lenCompDatGroup %in% plot) # data only aspects
          {
            if (verbose) {
              message("Starting length comp data plots (group ", lenCompDatGroup, ")")
            }
            # length comp polygon and bubble plots
            plotinfo <-
              SSplotComps(
                replist = replist, datonly = TRUE, kind = "LEN", bub = TRUE, verbose = verbose, fleets = fleets,
                fleetnames = fleetnames,
                samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = FALSE,
                minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.dat,
                bublegend = showlegend,
                maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
                plot = !png, print = png,
                plotdir = plotdir, mainTitle = mainTitle, cex.main = cex.main,
                sexes = sexes, yupper = comp.yupper,
                scalebins = scalebins, scalebubbles = scalebubbles,
                pwidth = pwidth, pheight = pheight, punits = punits,
                ptsize = ptsize, res = res,
                ...
              )
            if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)

            # length comp sex ratios (data only, for 2-sex models only)
            if (replist$nsexes == 2) {
              plotinfo <-
                SSplotSexRatio(
                  replist = replist,
                  datonly = TRUE,
                  kind = "LEN", bub = TRUE, verbose = verbose, fleets = fleets,
                  fleetnames = fleetnames,
                  linescol = 0, # turn off line showing expected value
                  samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = showeffN,
                  minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
                  bublegend = showlegend,
                  maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
                  plot = !png, print = png, smooth = smooth, plotdir = plotdir,
                  maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
                  cohortlines = cohortlines,
                  # sexes=sexes,
                  # yupper=comp.yupper,
                  scalebins = scalebins,
                  pwidth = pwidth, pheight = pheight, punits = punits,
                  ptsize = ptsize, res = res,
                  ...
                )
              if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
            }

            # size comp polygon and bubble plots (data only)
            for (sizemethod in sort(unique(replist$sizedbase$method))) {
              plotinfo <-
                SSplotComps(
                  replist = replist, datonly = TRUE, kind = "SIZE", sizemethod = sizemethod,
                  bub = TRUE, verbose = verbose, fleets = fleets, fleetnames = fleetnames,
                  samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = FALSE,
                  minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.dat,
                  bublegend = showlegend,
                  maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
                  plot = !png, print = png,
                  plotdir = plotdir, mainTitle = mainTitle, cex.main = cex.main,
                  sexes = sexes, yupper = comp.yupper,
                  scalebins = scalebins, scalebubbles = scalebubbles,
                  pwidth = pwidth, pheight = pheight, punits = punits,
                  ptsize = ptsize, res = res,
                  ...
                )
              if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
            }
          }
        if (ageCompDatGroup %in% plot) {
          if (verbose) {
            message("Starting age comp data plots (group ", ageCompDatGroup, ")")
          }
          # age comp polygon and bubble plots (data only)
          plotinfo <-
            SSplotComps(
              replist = replist, datonly = TRUE, kind = "AGE", bub = TRUE, verbose = verbose, fleets = fleets,
              fleetnames = fleetnames,
              samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = FALSE,
              minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.dat,
              bublegend = showlegend,
              maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
              plot = !png, print = png,
              plotdir = plotdir, mainTitle = mainTitle, cex.main = cex.main,
              sexes = sexes, yupper = comp.yupper,
              scalebins = scalebins, scalebubbles = scalebubbles,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res,
              ...
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
          # ghost age comp polygon and bubble plots (data only)
          plotinfo <-
            SSplotComps(
              replist = replist, datonly = TRUE, kind = "GSTAGE", bub = TRUE, verbose = verbose, fleets = fleets,
              fleetnames = fleetnames,
              samplesizeplots = samplesizeplots, showsampsize = FALSE, showeffN = FALSE,
              minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.dat,
              bublegend = showlegend,
              maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
              plot = !png, print = png,
              plotdir = plotdir, mainTitle = mainTitle, cex.main = cex.main,
              sexes = sexes, yupper = comp.yupper,
              scalebins = scalebins, scalebubbles = scalebubbles,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res,
              ...
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
          flush.console()

          # age comp sex ratios (data only)
          if (replist$nsexes == 2) {
            plotinfo <-
              SSplotSexRatio(
                replist = replist,
                datonly = TRUE,
                kind = "AGE", bub = TRUE, verbose = verbose, fleets = fleets,
                fleetnames = fleetnames,
                samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = showeffN,
                minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
                bublegend = showlegend,
                maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
                plot = !png, print = png, smooth = smooth, plotdir = plotdir,
                maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
                # sexes=sexes, yupper=comp.yupper,
                scalebins = scalebins,
                pwidth = pwidth, pheight = pheight, punits = punits,
                ptsize = ptsize, res = res,
                ...
              )
            if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
          }
        }
        if (condCompDatGroup %in% plot) {
          if (verbose) {
            message("Starting conditional comp data plots (group ", condCompDatGroup, ")")
          }
          # conditional age plot (data only)
          plotinfo <-
            SSplotComps(
              replist = replist, datonly = TRUE, kind = "cond", bub = TRUE, verbose = verbose, fleets = fleets,
              fleetnames = fleetnames,
              samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = FALSE,
              sampsizeline = sampsizeline, effNline = effNline,
              minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.dat,
              bublegend = showlegend,
              maxrows = maxrows, maxcols = maxcols, maxrows2 = maxrows2, maxcols2 = maxcols2,
              fixdims = fixdims, rows = rows, cols = cols,
              andrerows = andrerows,
              plot = !png, print = png,
              plotdir = plotdir, mainTitle = mainTitle, cex.main = cex.main,
              sexes = sexes, yupper = comp.yupper,
              scalebins = scalebins, scalebubbles = scalebubbles,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res,
              ...
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        } # end conditional data plots

        if (!is.null(plotInfoTable)) {
          plotInfoTable$category[plotInfoTable$category == "Comp"] <- "CompDat"
        }

        flush.console()
      } # end if data plot

      ##########################################
      # Length comp fits
      #
      igroup <- 16
      if (igroup %in% plot) {
        if (verbose) {
          message("Starting fit to length comp plots (group ", igroup, ")")
        }
        # regular length comps
        plotinfo <-
          SSplotComps(
            replist = replist, datonly = FALSE, kind = "LEN", bub = TRUE, verbose = verbose, fleets = fleets,
            fleetnames = fleetnames,
            samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = showeffN,
            minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
            bublegend = showlegend,
            maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
            plot = !png, print = png, smooth = smooth, plotdir = plotdir,
            maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
            cohortlines = cohortlines,
            sexes = sexes, yupper = comp.yupper,
            scalebins = scalebins,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res,
            ...
          )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)

        # ghost length comps
        plotinfo <-
          SSplotComps(
            replist = replist, datonly = FALSE, kind = "GSTLEN", bub = TRUE, verbose = verbose, fleets = fleets,
            fleetnames = fleetnames,
            samplesizeplots = FALSE, showsampsize = FALSE, showeffN = FALSE,
            minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
            bublegend = showlegend,
            maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
            plot = !png, print = png, smooth = smooth, plotdir = plotdir,
            maxneff = maxneff, cex.main = cex.main, cohortlines = cohortlines,
            sexes = sexes, yupper = comp.yupper,
            scalebins = scalebins,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res,
            ...
          )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)

        # loop over size methods for generalized size comp data
        if (nrow(replist$sizedbase) > 0) {
          for (sizemethod in sort(unique(replist$sizedbase$method))) {
            plotinfo <-
              SSplotComps(
                replist = replist, datonly = FALSE, kind = "SIZE", sizemethod = sizemethod,
                bub = TRUE, verbose = verbose, fleets = fleets, fleetnames = fleetnames,
                samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = showeffN,
                minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
                bublegend = showlegend,
                maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
                plot = !png, print = png, smooth = smooth, plotdir = plotdir,
                maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
                cohortlines = cohortlines,
                sexes = sexes, yupper = comp.yupper,
                scalebins = scalebins,
                pwidth = pwidth, pheight = pheight, punits = punits,
                ptsize = ptsize, res = res,
                ...
              )
            if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
          }
        }

        # length comp sex ratios (for 2-sex models only)
        if (replist$nsexes == 2) {
          plotinfo <-
            SSplotSexRatio(
              replist = replist,
              kind = "LEN", bub = TRUE, verbose = verbose, fleets = fleets,
              fleetnames = fleetnames,
              samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = showeffN,
              minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
              bublegend = showlegend,
              maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
              plot = !png, print = png, smooth = smooth, plotdir = plotdir,
              maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
              cohortlines = cohortlines,
              # sexes=sexes,
              # yupper=comp.yupper,
              scalebins = scalebins,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res,
              ...
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        }

        if (!is.null(plotInfoTable)) {
          plotInfoTable$category[plotInfoTable$category == "Comp"] <- "LenComp"
        }
      }

      ##########################################
      # Age comp fits
      #
      igroup <- 17
      if (igroup %in% plot) {
        if (verbose) {
          message("Starting fit to age comp plots (group ", igroup, ")")
        }
        # normal marginal ages
        plotinfo <-
          SSplotComps(
            replist = replist, datonly = FALSE, kind = "AGE", bub = TRUE, verbose = verbose, fleets = fleets,
            fleetnames = fleetnames,
            samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = showeffN,
            minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
            bublegend = showlegend,
            maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
            plot = !png, print = png, smooth = smooth, plotdir = plotdir,
            maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
            sexes = sexes, yupper = comp.yupper,
            scalebins = scalebins,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res,
            ...
          )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        # ghost ages
        plotinfo <-
          SSplotComps(
            replist = replist, datonly = FALSE, kind = "GSTAGE", bub = TRUE, verbose = verbose, fleets = fleets,
            fleetnames = fleetnames,
            samplesizeplots = FALSE, showsampsize = FALSE, showeffN = FALSE,
            minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
            bublegend = showlegend,
            maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
            plot = !png, print = png, smooth = smooth, plotdir = plotdir,
            maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
            sexes = sexes, yupper = comp.yupper,
            scalebins = scalebins,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res,
            ...
          )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        # age comp sex ratios
        if (replist$nsexes == 2) {
          plotinfo <-
            SSplotSexRatio(
              replist = replist,
              kind = "AGE", bub = TRUE, verbose = verbose, fleets = fleets,
              fleetnames = fleetnames,
              samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = showeffN,
              minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
              bublegend = showlegend,
              maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
              plot = !png, print = png, smooth = smooth, plotdir = plotdir,
              maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
              # sexes=sexes, yupper=comp.yupper,
              scalebins = scalebins,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res,
              ...
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        }
        if (!is.null(plotInfoTable)) {
          plotInfoTable$category[plotInfoTable$category == "Comp"] <- "AgeComp"
        }
      } # end if igroup in plot or print

      ##########################################
      # Conditional age-at-length comp fits
      #
      igroup <- 18
      if (igroup %in% plot) {
        if (verbose) {
          message("Starting fit to conditional age-at-length comp plots (group ", igroup, ")")
        }
        if (aalresids) {
          plotinfo <-
            SSplotComps(
              replist = replist, subplots = 3, datonly = FALSE, kind = "cond", bub = TRUE, verbose = verbose, fleets = fleets,
              fleetnames = fleetnames,
              samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = showeffN,
              sampsizeline = sampsizeline, effNline = effNline,
              minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
              bublegend = showlegend,
              maxrows = maxrows, maxcols = maxcols, maxrows2 = maxrows2, maxcols2 = maxcols2, fixdims = fixdims, rows = rows, cols = cols,
              plot = !png, print = png, smooth = smooth, plotdir = plotdir,
              maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
              sexes = sexes, yupper = comp.yupper,
              scalebins = scalebins,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res,
              ...
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        }
        # conditional age at length for a given year
        if (length(intersect(aalyear, unique(timeseries$Yr))) > 0) {
          plotinfo <-
            SSplotComps(
              replist = replist, subplots = 4:5, datonly = FALSE, kind = "cond", bub = TRUE, verbose = verbose, fleets = fleets,
              fleetnames = fleetnames,
              aalbin = aalbin, aalyear = aalyear,
              samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = showeffN,
              sampsizeline = sampsizeline, effNline = effNline,
              minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
              bublegend = showlegend,
              maxrows = maxrows, maxcols = maxcols, maxrows2 = maxrows2, maxcols2 = maxcols2, fixdims = fixdims, rows = rows, cols = cols,
              plot = !png, print = png, smooth = smooth, plotdir = plotdir,
              maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
              sexes = sexes, yupper = comp.yupper,
              scalebins = scalebins,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res,
              ...
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        }
        # conditional age at length for a given length bin
        if (length(intersect(aalbin, unique(lbins))) > 0) {
          plotinfo <-
            SSplotComps(
              replist = replist, subplots = 6, datonly = FALSE, kind = "cond", bub = TRUE, verbose = verbose, fleets = fleets,
              fleetnames = fleetnames,
              aalbin = aalbin,
              samplesizeplots = samplesizeplots, showsampsize = showsampsize, showeffN = showeffN,
              minnbubble = minnbubble, pntscalar = pntscalar, cexZ1 = bub.scale.pearson,
              bublegend = showlegend,
              maxrows = maxrows, maxcols = maxcols, maxrows2 = maxrows2, maxcols2 = maxcols2, fixdims = fixdims, rows = rows, cols = cols,
              plot = !png, print = png, smooth = smooth, plotdir = plotdir,
              maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
              sexes = sexes, yupper = comp.yupper,
              scalebins = scalebins,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res,
              ...
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        }
        if (!is.null(plotInfoTable)) {
          plotInfoTable$category[plotInfoTable$category == "Comp"] <- "A@LComp"
        }
      } # end if igroup in plot or print

      ##########################################
      # Francis and Punt conditional age-at-length comp fits
      #
      igroup <- 19
      if (igroup %in% plot) {
        if (nrow(replist$condbase) > 0) {
          if (replist$nagebins == 1) {
            if (verbose) {
              message(
                "Skipping conditional age-at-length diagnostic plots (group ",
                igroup,
                ") due to only 1 age bin"
              )
            }
          } else {
            if (verbose) {
              message("Starting conditional age-at-length diagnostic plots (group ", igroup, ")")
            }
            plotinfo <-
              SSplotComps(
                replist = replist, subplots = 9:10, datonly = FALSE, kind = "cond",
                bub = TRUE, verbose = verbose, fleets = fleets,
                fleetnames = fleetnames,
                aalbin = aalbin, aalyear = aalyear,
                samplesizeplots = samplesizeplots,
                showsampsize = showsampsize, showeffN = showeffN,
                minnbubble = minnbubble, pntscalar = pntscalar,
                maxrows = maxrows, maxcols = maxcols,
                maxrows2 = maxrows2, maxcols2 = maxcols2,
                fixdims = fixdims, rows = rows, cols = cols,
                andrerows = andrerows,
                plot = !png, print = png, smooth = smooth, plotdir = plotdir,
                maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
                sexes = sexes, scalebins = FALSE,
                pwidth = pwidth, pheight = pheight, punits = punits,
                ptsize = ptsize, res = res,
                ...
              )
            if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
            if (!is.null(plotInfoTable)) {
              plotInfoTable$category[plotInfoTable$category == "Comp"] <- "A@LComp"
            }
          }
        } else {
          if (verbose) {
            message("Skipping conditional A@L plots (group ", igroup, ") because no such data in model")
          }
        }
      } # end if igroup in plot or print

      ##########################################
      # Mean length-at-age and mean weight-at-age plots
      #
      igroup <- 20
      if (igroup %in% plot) {
        if (verbose) {
          message("Starting mean length-at-age and mean weight-at-age plots (group ", igroup, ")")
        }
        if (datplot) {
          # data-only plot of mean length at age
          plotinfo <-
            SSplotComps(
              replist = replist, datonly = TRUE, kind = "L@A", bub = TRUE, verbose = verbose, fleets = fleets,
              fleetnames = fleetnames,
              samplesizeplots = FALSE, showsampsize = FALSE, showeffN = FALSE,
              minnbubble = minnbubble, pntscalar = pntscalar,
              maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
              plot = !png, print = png, smooth = smooth, plotdir = plotdir,
              maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
              sexes = sexes, scalebins = scalebins,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res,
              ...
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
          # data-only plot of mean weight at age
          plotinfo <-
            SSplotComps(
              replist = replist, datonly = TRUE, kind = "W@A", bub = TRUE, verbose = verbose, fleets = fleets,
              fleetnames = fleetnames,
              samplesizeplots = FALSE, showsampsize = FALSE, showeffN = FALSE,
              minnbubble = minnbubble, pntscalar = pntscalar,
              maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
              plot = !png, print = png, smooth = smooth, plotdir = plotdir,
              maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
              sexes = sexes, scalebins = scalebins,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res,
              ...
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        }
        # mean length at age with model fit
        plotinfo <-
          SSplotComps(
            replist = replist, datonly = FALSE, kind = "L@A", bub = TRUE, verbose = verbose, fleets = fleets,
            fleetnames = fleetnames,
            samplesizeplots = FALSE, showsampsize = FALSE, showeffN = FALSE,
            minnbubble = minnbubble, pntscalar = pntscalar,
            maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
            plot = !png, print = png, smooth = smooth, plotdir = plotdir,
            maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
            sexes = sexes, scalebins = scalebins,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res,
            ...
          )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        # mean weight at age with model fit
        plotinfo <-
          SSplotComps(
            replist = replist, datonly = FALSE, kind = "W@A", bub = TRUE, verbose = verbose, fleets = fleets,
            fleetnames = fleetnames,
            samplesizeplots = FALSE, showsampsize = FALSE, showeffN = FALSE,
            minnbubble = minnbubble, pntscalar = pntscalar,
            maxrows = maxrows, maxcols = maxcols, fixdims = fixdims, rows = rows, cols = cols,
            plot = !png, print = png, smooth = smooth, plotdir = plotdir,
            maxneff = maxneff, mainTitle = mainTitle, cex.main = cex.main,
            sexes = sexes, scalebins = scalebins,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res,
            ...
          )
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
      } # end if length-at-age and weight-at-age comps in plot or print
      if (!is.null(plotInfoTable)) {
        plotInfoTable$category[plotInfoTable$category == "Comp"] <- "Mean@A"
      }

      # restore default single panel settings if needed
      # conditional because if adding to existing plot may mess up layout
      if (any(par()$mfcol != c(rows, cols))) {
        par(mfcol = c(rows, cols))
      }
      if (any(par()$mar != mar0)) {
        par(mar = mar0)
      }
      if (any(par()$oma != oma0)) {
        par(oma = oma0)
      }

      ##########################################
      # Tag plots
      #
      igroup <- 21
      if (igroup %in% plot) {
        if (is.null(replist$tagdbase2) || nrow(replist$tagdbase2) == 0) {
          if (verbose) {
            message("Skipping tag plots (group ", igroup, ") because no tag data in model")
          }
        } else {
          if (verbose) {
            message("Starting tag plots (group ", igroup, ")")
          }
          plotinfo <-
            SSplotTags(
              replist = replist,
              rows = rows, cols = cols,
              tagrows = tagrows, tagcols = tagcols,
              latency = replist$tagfirstperiod,
              pntscalar = pntscalar.tags, minnbubble = minnbubble,
              plot = !png, print = png,
              pwidth = pwidth, pheight = pheight, punits = punits,
              ptsize = ptsize, res = res, cex.main = cex.main,
              plotdir = plotdir,
              verbose = verbose
            )
          if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
        } # end if data present
      } # end if igroup in plot or print
    } # end if comp data

    ##########################################
    # Yield plots
    #
    igroup <- 22
    if (igroup %in% plot) {
      if (verbose) {
        message("Starting yield plots (group ", igroup, ")")
      }
      plotinfo <-
        SSplotYield(
          replist = replist,
          plot = !png, print = png,
          pwidth = pwidth, pheight = pheight, punits = punits,
          ptsize = ptsize, res = res, cex.main = cex.main,
          plotdir = plotdir
        )
      if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
    } # end if igroup in plot or print


    ##########################################
    # Movement rate plots
    #
    igroup <- 23
    if (igroup %in% plot) {
      if (!is.null(replist$movement) && nrow(replist$movement) > 0) {
        if (verbose) {
          message("Starting movement rate plots (group ", igroup, ")")
        }
        plotinfo <- NULL
        temp <-
          SSplotMovementRates(
            replist = replist,
            plot = !png, print = png,
            pwidth = pwidth, pheight = pheight, punits = punits,
            ptsize = ptsize, res = res, cex.main = cex.main,
            plotdir = plotdir
          )
        plotinfo <- temp$plotinfo
        if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
      } else {
        if (verbose) {
          message(
            "Skipping movement plots (group ", igroup,
            ") because no movement in model\n"
          )
        }
      } # end if movement included in model
    } # end if igroup in plot or print

    ##########################################
    # Data range plots
    #
    igroup <- 24
    if (igroup %in% plot) {
      if (verbose) {
        message("Starting data range plots (group ", igroup, ")")
      }
      plotinfo <- NULL
      temp <-
        SSplotData(
          replist = replist,
          plot = !png, print = png,
          pwidth = pwidth, pheight = pheight, punits = punits,
          ptsize = ptsize, res = res, mainTitle = mainTitle, cex.main = cex.main,
          plotdir = plotdir, margins = c(5.1, 2.1, 4.1, SSplotDatMargin),
          fleetnames = fleetnames, maxsize = maxsize
        )
      if (!is.null(temp) & length(temp) > 0) plotinfo <- temp$plotinfo
      if (!is.null(plotinfo)) plotInfoTable <- rbind(plotInfoTable, plotinfo)
    } # end if igroup in plot or print

    ##########################################
    # Parameter distribution plots
    #
    igroup <- 25
    if (igroup %in% plot) {
      if (verbose) {
        message("Starting parameter distribution plots (group ", igroup, ")")
      }
      if (showpost && is.null(replist$mcmc)) {
        showpost <- FALSE
      }
      plotinfo <- SSplotPars(
        replist = replist,
        plot = !png, print = png,
        pwidth = pwidth, pheight = pheight, punits = punits,
        ptsize = ptsize, res = res,
        nrows = parrows,
        ncols = parcols,
        showmle = showmle,
        showpost = showpost,
        showprior = showprior,
        showinit = showinit,
        showdev = showdev,
        fitrange = fitrange,
        verbose = verbose,
        plotdir = plotdir
      )
      if (!is.null(plotinfo)) {
        plotInfoTable <- rbind(plotInfoTable, plotinfo)
      }
    } # end if igroup in plot or print

    if (pdf) dev.off() # close PDF file if it was open
    if (verbose) {
      message("Finished all requested plots in SS_plots function")
    }

    ##########################################
    # diagnostic tables
    #
    igroup <- 26
    if (igroup %in% plot) {
      if (nrow(replist$estimated_non_dev_parameters) == 0) {
        if (verbose) {
          message(
            "Skipping diagnostic tables (group ", igroup,
            ") because there are no estimated non-dev parameters"
          )
        }
      } else {
        if (!png) {
          message(
            "Skipping diagnostic tables (group ", igroup,
            ") because png=FALSE"
          )
        } else {
          if (verbose) {
            message("Starting diagnostic tables (group ", igroup, ")")
          }

          plotinfo <- NULL
          plotinfo <- SS_makeHTMLdiagnostictable(
            replist = replist,
            plotdir = plotdir,
            gradmax = 1E-3
          )
          if (!is.null(plotinfo)) {
            plotInfoTable <- rbind(plotInfoTable, plotinfo)
          }
        } # end making the tables
      } # end check for estimated non-dev parameters
    } # end if igroup %in% plot

    ##########################################
    # Write and return table of plot info for any PNG files that got created
    #
    if (!is.null(plotInfoTable)) {
      # make sure there are no factors
      plotInfoTable$file <- as.character(plotInfoTable$file)
      plotInfoTable$caption <- as.character(plotInfoTable$caption)
      # record the current time and the model run time
      png_time <- Sys.time()
      # png_time2 <- format(writetime,'%d-%m-%Y_%H.%M')
      plotInfoTable$png_time <- png_time
      plotInfoTable$StartTime <- StartTime
      # create a name for the file and write it to the plot directory
      csvname <- file.path(
        plotdir,
        paste0(
          "plotInfoTable_",
          format(png_time, "%d-%m-%Y_%H.%M.%S"), ".csv"
        )
      )
      write.csv(plotInfoTable, csvname, row.names = FALSE)
      if (verbose) message("Wrote table of info on PNG files to:\n   ", csvname)
      # write HTML files to display the images
      if (html) {
        SS_html(replist, filenotes = filenotes, plotdir = plotdir, verbose = verbose, ...)
      }
      # make paths absolute
      # return notes on the plots
      return(invisible(plotInfoTable))
    } else {
      # if there's some problem (perhaps if no plots were created), return a 999 code
      return(invisible(999))
    }
    ### end of SS_plots function
  }
