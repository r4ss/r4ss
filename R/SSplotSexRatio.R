#' Plot sex-ratio data and fits for two sex models
#'
#' Plot sex-ratio data and fits from Stock Synthesis output.  Multi-figure
#' plots depend on `make_multifig`. The confidence intervals around the
#' observed points are based on a Jeffreys interval calculated from
#' the adjusted input sample size (with a floor of 1).
#'
#'
#' @template replist
#' @param kind indicator of type of plot can be "LEN", "SIZE", "AGE", "cond",
#' "GSTAGE", "L@A", or "W@A".
#' @param sexratio.option code to choose among (1) female:male ratio or
#' (2) fraction females out of the total
#' @param CI confidence interval for uncertainty
#' @template plot
#' @template print
#' @template fleets
#' @template fleetnames
#' @param yupper upper limit on ymax (only applies for sexratio.option == 1)
#' @param datonly make plots of data without fits?
#' @param linescol Color for line showing expected value (default is purple).
#' @template lwd
#' @param showsampsize add sample sizes to plot
#' @param showeffN add effective sample sizes to plot
#' @param axis1 position of bottom axis values
#' @param axis2 position of left size axis values
#' @template pwidth
#' @template pheight
#' @template punits
#' @template ptsize
#' @template res
#' @template plotdir
#' @template cex.main
#' @template labels
#' @param maxrows maximum (or fixed) number or rows of panels in the plot
#' @param maxcols maximum (or fixed) number or columns of panels in the plot
#' plots
#' @param rows number or rows to return to as default for next plots to come or
#' for single plots
#' @param cols number or cols to return to as default for next plots to come or
#' for single plots
#' @param fixdims fix the dimensions at maxrows by maxcols or resize based on
#' number of years of data
#' @template verbose
#' @template mainTitle
#' @param \dots additional arguments that will be passed to the plotting.
#' @author Cole Monnahan, Ian Taylor
#' @export
#' @seealso [SS_plots()], [make_multifig_sexratio()]
#' @references Brown, L.; Cai, T. Tony; DasGupta, A. (2001).
#' Interval Estimation for a Binomial Proportion. Statistical Science.
#' 16(2): 101-133. http://www.jstor.org/stable/2676784.
SSplotSexRatio <-
  function(
    replist,
    kind = "AGE",
    sexratio.option = 2,
    CI = 0.75,
    plot = TRUE,
    print = FALSE,
    fleets = "all",
    fleetnames = "default",
    yupper = 4,
    datonly = FALSE,
    linescol = rgb(0.6, 0, 0.9, .7), # a purple color
    lwd = 2,
    showsampsize = TRUE,
    showeffN = TRUE,
    axis1 = NULL,
    axis2 = NULL,
    pwidth = 6.5,
    pheight = 5.0,
    punits = "in",
    ptsize = 10,
    res = 300,
    plotdir = "default",
    cex.main = 1,
    labels = c(
      "Length (cm)",
      "Age (yr)",
      "Sex ratio (females:males)",
      "Fraction female"
    ),
    maxrows = 6,
    maxcols = 6,
    rows = 1,
    cols = 1,
    fixdims = TRUE,
    verbose = TRUE,
    mainTitle = FALSE,
    ...
  ) {
    # table to store information on each plot
    plotinfo <- NULL

    SS_versionNumeric <- replist[["SS_versionNumeric"]]

    lendbase <- replist[["lendbase"]]
    agedbase <- replist[["agedbase"]]
    ghostagedbase <- replist[["ghostagedbase"]]
    ghostlendbase <- replist[["ghostlendbase"]]
    nfleets <- replist[["nfleets"]]
    nseasons <- replist[["nseasons"]]
    seasfracs <- replist[["seasfracs"]]
    FleetNames <- replist[["FleetNames"]]
    nsexes <- replist[["nsexes"]]

    ## define a variety of titles and labels
    titles <- NULL
    titlemkt <- ""
    if (plotdir == "default") {
      plotdir <- replist[["inputs"]][["dir"]]
    }

    ## sort out which fleets will be included
    if (fleets[1] == "all") {
      fleets <- 1:nfleets
    } else if (length(intersect(fleets, 1:nfleets)) != length(fleets)) {
      stop(
        "Input 'fleets' should be 'all' or a vector of values between 1 and nfleets."
      )
    }

    if (fleetnames[1] == "default") {
      fleetnames <- FleetNames
    }

    ## a few quantities related to data type and plot number
    if (kind == "LEN") {
      dbase_kind <- lendbase
      kindlab <- labels[1]
      filenamestart <- "sexratio_len_"
      titledata <- "Sex ratios for length comps, "
    } else if (kind == "AGE") {
      dbase_kind <- agedbase
      kindlab <- labels[2]
      filenamestart <- "sexratio_age_"
      titledata <- "Sex ratios for age comps, "
    } else {
      stop("Only kind of LEN and AGE are currently supported")
    }

    # modify filename for data-only plots
    if (datonly) {
      filenamestart <- gsub(
        pattern = "sexratio",
        replacement = "sexratio_data",
        x = filenamestart
      )
    }

    ## else if(kind=="GSTLEN"){
    ##     dbase_kind <- ghostlendbase
    ##     kindlab=labels[1]
    ##     filenamestart <- "sexratio_gstlen_"
    ##     titledata <- "sex ratios for ghost length comps, "
    ##   } else if(kind=="GSTAGE"){
    ##     dbase_kind <- ghostagedbase
    ##     kindlab=labels[2]
    ##     filenamestart <- "sexratio_gstage_"
    ##     titledata <- "sex ratios for ghost age comps, "
    ##   }

    ## Add asterix to indicate super periods and then remove rows labeled "skip".
    ## It would be better to somehow show the range of years, but that seems difficult.
    if (
      any(dbase_kind[["SuprPer"]] == "Sup" & dbase_kind[["Used"]] == "skip")
    ) {
      message(
        "Removing super-period composition values labeled 'skip'\n",
        "     and designating super-period values with a '*'"
      )
      dbase_kind <- dbase_kind[
        dbase_kind[["SuprPer"]] == "No" | dbase_kind[["Used"]] != "skip",
      ]
      dbase_kind[["YrSeasName"]] <- paste(
        dbase_kind[["YrSeasName"]],
        ifelse(dbase_kind[["SuprPer"]] == "Sup", "*", ""),
        sep = ""
      )
    }
    ageerr_warning <- TRUE

    ## # subset data based on requested range of sexes
    ## dbase_kind <- dbase_kind[dbase_kind[["sex"]] %in% sexes,]
    ## loop over fleets
    for (f in fleets) {
      dbasef <- dbase_kind[dbase_kind[["Fleet"]] == f, ]
      ## For a 2-sex model some fleets may have only females so skip those
      if (length(unique(dbasef[["Sex"]])) > 1 & nrow(dbasef) > 0) {
        ## loop over partitions (discard, retain, total)
        for (j in unique(dbasef[["Part"]])) {
          dbase <- dbasef[dbasef[["Part"]] == j, ]
          ## assemble pieces of plot title
          ## market category
          if (j == 0) {
            titlemkt <- "whole catch, "
          }
          if (j == 1) {
            titlemkt <- "discard, "
          }
          if (j == 2) {
            titlemkt <- "retained, "
          }

          ## aggregating identifiers for plot titles and filenames
          title_mkt <- paste(titlemkt, sep = "")
          filename_fltmkt <- paste("flt", f, "mkt", j, sep = "")
          caption <- paste(titledata, title_mkt, fleetnames[f], sep = "") ## total title
          if (mainTitle) {
            ptitle <- caption
          } else {
            ptitle <- ""
          }
          # titles <- c(ptitle,titles) ## compiling list of all plot titles
          tempfun <- function(ipage, ...) {
            ## a function to combine a bunch of repeated commands
            make_multifig_sexratio(
              dbase = dbase,
              sexratio.option = sexratio.option,
              CI = CI,
              nlegends = 3,
              legtext = list("Yr", "N", "effN"),
              lwd = lwd,
              showsampsize = showsampsize,
              showeffN = showeffN,
              main = ptitle,
              cex.main = cex.main,
              xlab = kindlab,
              ylab = labels[3:4][sexratio.option],
              maxrows = maxrows,
              maxcols = maxcols,
              rows = rows,
              cols = cols,
              fixdims = fixdims,
              ipage = ipage,
              scalebins = FALSE,
              linescol = linescol,
              axis1 = axis1,
              axis2 = axis2,
              yupper = yupper,
              datonly = datonly
            )
          } # end tempfun

          ## Do the plotting and saving
          if (plot) {
            tempfun(ipage = 0, ...)
          }
          if (print) {
            # set up plotting to png file if required
            npages <- ceiling(length(unique(dbase[["Yr"]])) / maxrows / maxcols)
            for (ipage in 1:npages) {
              pagetext <- ""
              caption_count <- ""
              caption_extra <- ""
              if (npages > 1) {
                pagetext <- paste0("_page", ipage)
                caption_count <- paste0(" (plot ", ipage, " of ", npages, ")")
              }
              if (ipage == 1) {
                caption_extra <-
                  paste0(
                    ".<br>Observed sex ratios (points) with ",
                    100 * CI,
                    "% intervals (vertical lines) calculated as a ",
                    "<a href='https://www.jstor.org/stable/2676784'>",
                    "Jeffreys interval</a>",
                    " based on the adjusted input sample size."
                  )
                if (!datonly) {
                  caption_extra <- paste(
                    caption_extra,
                    "The model expectation is shown",
                    " in the purple line."
                  )
                }
              }
              file <- paste0(filenamestart, filename_fltmkt, pagetext, ".png")
              caption <- paste0(caption, caption_count, caption_extra)
              plotinfo <- save_png(
                plotinfo = plotinfo,
                file = file,
                plotdir = plotdir,
                pwidth = pwidth,
                pheight = pheight,
                punits = punits,
                res = res,
                ptsize = ptsize,
                caption = caption
              )
              tempfun(ipage = ipage, ...)
              dev.off()
            }
          }
        } # end loop over partitions (index j)
        ##        } # end test for whether sex in vector of requested sexes
        ##      } # end loop over combined/not-combined sexes
      } # end if data
    } # end loop over fleets

    if (!is.null(plotinfo)) {
      plotinfo[["category"]] <- "Comp"
    }
    return(invisible(plotinfo))
  } # end embedded SSplotComps function
###########################
