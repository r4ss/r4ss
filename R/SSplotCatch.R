#' Plot catch related quantities.
#'
#' Plot catch related quantities from Stock Synthesis output. Plots include
#' harvest rate, continuous F, landings, and discard fraction.
#'
#'
#' @template replist
#' @param subplots Vector controlling which subplots to create
#' Numbering of subplots is as follows,
#'
#' *Basic plots for all models*
#' \itemize{
#'   \item 1 total catch as line plot (if no discards) or landings (if discards present)
#'   \item 2 total catch as stacked bars (if no discards) or landings (if discards present)
#'   \item 3 observed and expected landings (if different)
#'   \item 9 harvest rate
#' }
#'
#' *Plots for models with discards*
#' \itemize{
#'   \item 4 total catch (including discards)
#'   \item 5 total catch (including discards) stacked
#'   \item 6 discards
#'   \item 7 discards stacked plot (depends on multiple fleets)
#'   \item 8 discard fraction
#'   \item 16 landings + dead discards
#' }
#'
#' *Plots for seasonal models*
#' \itemize{
#'   \item 10 landings aggregated across seasons
#'   \item 11 landings aggregated across seasons stacked
#'   \item 12 total catch (if discards present) aggregated across seasons
#'   \item 13 total catch (if discards present) aggregated across seasons stacked
#'   \item 14 discards aggregated across seasons
#'   \item 15 discards aggregated across seasons stacked
#' }
#' @param add Add to existing plot? (not yet implemented)
#' @param areas Optional subset of areas to plot for spatial models
#' @template plot
#' @template print
#' @param type Type parameter passed to plot function. Default "l" is lines
#' only.  Other options include "o" for overplotting points on lines.
#' @param fleetlty Vector of line type by fleet
#' @param fleetpch Vector of plot character by fleet
#' @param fleetcols Vector of colors by fleet
#' @template fleetnames
#' @template lwd
#' @template areacols
#' @param areanames Names for areas. Default is to use Area1, Area2,...
#' @param minyr Optional input for minimum year to show in plots
#' @param maxyr Optional input for maximum year to show in plots
#' @param annualcatch Include plot of catch aggregated across seasons within
#' each year
#' @param forecastplot Add points from forecast years
#' @template plotdir
#' @param showlegend Put legend on plot
#' @template legendloc
#' @param order Optional input to change the order of fleets in stacked plots.
#' @param xlab x-label for all plots
#' @template labels
#' @param catchasnumbers Is catch in numbers instead of biomass? Should be set
#' automatically if set to NULL. If fleets include a mix of biomass and
#' numbers, then catch plots should be interpreted carefully.
#' @param catchbars Show catch by fleet as barplot instead of stacked polygons?
#' (default=TRUE)
#' @param addmax Add a point on the y-axis for the maximum catch (default=TRUE)
#' @param ymax Optional input for ymax value (can be used to add or subtract
#' white space at the top of the figure)
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @template cex.main
#' @template verbose
#' @author Ian Taylor, Ian Stewart
#' @export
#' @seealso [SS_plots()], [SS_output()]
SSplotCatch <-
  function(
    replist,
    subplots = 1:16,
    add = FALSE,
    areas = 1,
    plot = TRUE,
    print = FALSE,
    type = "l",
    fleetlty = 1,
    fleetpch = 1,
    fleetcols = "default",
    fleetnames = "default",
    lwd = 3,
    areacols = NULL,
    areanames = "default",
    minyr = -Inf,
    maxyr = Inf,
    annualcatch = TRUE,
    forecastplot = FALSE,
    plotdir = "default",
    showlegend = TRUE,
    legendloc = "topleft",
    order = "default",
    xlab = "Year",
    labels = c(
      "Harvest rate/Year", # 1
      "Continuous F", # 2
      "Landings", # 3
      "Total catch", # 4
      "Predicted discards", # 5 # should add units
      "Discard fraction", # 6  # need to add by weight or by length
      "(t)", # 7
      "(numbers x1000)", # 8
      "Observed and expected", # 9
      "aggregated across seasons" # 10
    ),
    catchasnumbers = NULL,
    catchbars = TRUE,
    addmax = TRUE,
    ymax = NULL,
    pwidth = 6.5,
    pheight = 5.0,
    punits = "in",
    res = 300,
    ptsize = 10,
    cex.main = 1, # note: no plot titles yet implemented
    verbose = TRUE
  ) {
    # IGT 16Apr2021: labels were not getting passed into the function.
    # I don't why by this work-around should bring them back until we can
    # figure out what's going on
    if (is.null(labels)) {
      labels <- c(
        "Harvest rate/Year", # 1
        "Continuous F", # 2
        "Landings", # 3
        "Total catch", # 4
        "Predicted discards", # 5 # should add units
        "Discard fraction", # 6  # need to add by weight or by length
        "(t)", # 7
        "(numbers x1000)", # 8
        "Observed and expected", # 9
        "aggregated across seasons" # 10
      )
    }

    # note: stacked plots depend on multiple fleets
    subplot_names <- c(
      "1: landings",
      "2: landings stacked",
      "3: observed and expected landings (if different)",
      # note: subplots 4-8 depend on discards
      "4: total catch (including discards)",
      "5: total catch (including discards) stacked",
      "6: discards",
      "7: discards stacked plot (depends on multiple fleets)",
      "8: discard fraction",
      "9: harvest rate",
      # note: subplots 10-15 are only for seasonal models
      "10: landings aggregated across seasons",
      "11: landings aggregated across seasons stacked",
      "12: total catch (if discards present) aggregated across seasons",
      "13: total catch (if discards present) aggregated across seasons stacked",
      "14: discards aggregated across seasons",
      "15: discards aggregated across seasons stacked",
      # note: subplot 16
      "16: landings + dead discards"
    )

    # table to store information on each plot
    plotinfo <- NULL

    F_method <- replist[["F_method"]]
    timeseries <- replist[["timeseries"]]
    nseasons <- replist[["nseasons"]]
    nareas <- replist[["nareas"]]
    nfleets <- replist[["nfleets"]]
    nfishfleets <- replist[["nfishfleets"]]
    catch_units <- replist[["catch_units"]]
    fleet_types <- replist[["definitions"]][["fleet_type"]]
    endyr <- replist[["endyr"]]
    FleetNames <- replist[["FleetNames"]]
    IsFishFleet <- replist[["IsFishFleet"]]
    SS_versionshort <- toupper(substr(replist[["SS_version"]], 1, 8))
    SS_versionNumeric <- replist[["SS_versionNumeric"]]

    # if the user has no specified whether catch should be in numbers or not
    # 1 = biomass, 2 = numbers
    if (is.null(catchasnumbers)) {
      if (min(catch_units, na.rm = TRUE) == 2) {
        # if there is no fleet with catch_units = 1, then assume numbers
        catchasnumbers <- TRUE
      } else {
        # if there is any fleet with catch_units = 1, plot in biomass
        catchasnumbers <- FALSE
        # check for other fleets with catch_units = 2 = numbers
        # and warn that to be careful interpreting plots
        if (2 %in% catch_units[fleet_types != 3]) {
          warning(
            "Catch is a mix of numbers and biomass,",
            " so be careful interpreting catch plots.\n",
            "  Use 'SS_plots(..., catchasnumbers = TRUE)'",
            " to get plots in numbers"
          )
        }
      }
    }

    if (fleetnames[1] == "default") {
      fleetnames <- FleetNames
    }
    if (plotdir == "default") {
      plotdir <- replist[["inputs"]][["dir"]]
    }

    if (catchasnumbers) {
      labels[3] <- paste(labels[3], labels[8])
      labels[4] <- paste(labels[4], labels[8])
      labels[5] <- paste(labels[5], labels[8])
    } else {
      labels[3] <- paste(labels[3], labels[7])
      labels[4] <- paste(labels[4], labels[7])
      labels[5] <- paste(labels[5], labels[7])
    }

    # time series quantities used for multiple plots
    if (nseasons > 1) {
      timeseries[["Yr"]] <- timeseries[["Yr"]] + replist[["seasfracs"]]
    }
    ts <- timeseries[
      timeseries[["Yr"]] >= minyr & timeseries[["Yr"]] <= maxyr,
    ]
    # filter out forecast years if requested
    if (!forecastplot) {
      ts <- ts[ts[["Yr"]] <= endyr + 1, ]
    }

    # spread equilibrium catch over all seasons for 3.24 and earlier models
    # starting with 3.30, equilibrium catch can be season-specific
    if (nseasons > 1 & SS_versionNumeric <= 3.24) {
      catch.cols <- NULL
      for (string in c("sel", "dead", "retain", "obs", "Hrate:_", "F:_")) {
        catch.cols <- c(catch.cols, grep(string, names(ts)))
      }
      # which columns contain some form of catch
      catch.cols <- sort(unique(catch.cols))
      equil.catch.vec <- ts[which(ts[["Era"]] == "INIT")[1], catch.cols]
      for (irow in which(ts[["Era"]] == "INIT")) {
        ts[irow, catch.cols] <- equil.catch.vec / nseasons
      }
    }

    # harvest rates
    if (F_method == 1) {
      stringF <- "Hrate:_"
      ylabF <- labels[1]
    } else {
      # for either continuous F or hybrid F (methods 2 and 3)
      stringF <- "F:_"
      ylabF <- labels[2]
    }

    ### total landings (retained) & catch (encountered)
    goodrows <- ts[["Area"]] == 1 & ts[["Era"]] %in% c("INIT", "TIME")
    if (forecastplot) {
      goodrows <- ts[["Area"]] == 1 & ts[["Era"]] %in% c("INIT", "TIME", "FORE")
    }
    catchyrs <- ts[["Yr"]][goodrows] # T/F indicator of the lines for which we want to plot catch

    if (SS_versionNumeric == 3.11) {
      stringN <- "enc(N)"
      stringB <- "enc(B)"
    } else {
      stringN <- "sel(N)"
      stringB <- "sel(B)"
    }
    if (catchasnumbers) {
      retmat <- as.matrix(ts[
        goodrows,
        substr(names(ts), 1, nchar("retain(N)")) == "retain(N)"
      ])
      deadmat <- as.matrix(ts[
        goodrows,
        substr(names(ts), 1, nchar("dead(N)")) == "dead(N)"
      ])
      totcatchmat <- as.matrix(ts[
        goodrows,
        substr(names(ts), 1, nchar(stringN)) == stringN
      ])
      if (ncol(totcatchmat) == 1) {
        colnames(totcatchmat) <- grep(
          stringN,
          names(ts),
          fixed = TRUE,
          value = TRUE
        )
      }
    } else {
      retmat <- as.matrix(ts[
        goodrows,
        substr(names(ts), 1, nchar("retain(B)")) == "retain(B)"
      ])
      deadmat <- as.matrix(ts[
        goodrows,
        substr(names(ts), 1, nchar("dead(B)")) == "dead(B)"
      ])
      totcatchmat <- as.matrix(ts[
        goodrows,
        substr(names(ts), 1, nchar(stringB)) == stringB
      ])
      if (ncol(totcatchmat) == 1) {
        colnames(totcatchmat) <- grep(
          stringB,
          names(ts),
          fixed = TRUE,
          value = TRUE
        )
      }
    }
    totobscatchmat <- as.matrix(ts[
      goodrows,
      substr(names(ts), 1, nchar("obs_cat")) == "obs_cat"
    ])
    Hratemat <- as.matrix(ts[
      goodrows,
      substr(names(ts), 1, nchar(stringF)) == stringF
    ])

    # add total across areas
    if (nareas > 1) {
      for (iarea in 2:nareas) {
        arearows <- ts[["Area"]] == iarea & ts[["Era"]] %in% c("INIT", "TIME")
        if (forecastplot) {
          arearows <- ts[["Area"]] == iarea &
            ts[["Era"]] %in% c("INIT", "TIME", "FORE")
        }
        if (catchasnumbers) {
          retmat <- retmat +
            as.matrix(ts[
              arearows,
              substr(names(ts), 1, nchar("retain(N)")) == "retain(N)"
            ])
          totcatchmat <- totcatchmat +
            as.matrix(ts[
              arearows,
              substr(names(ts), 1, nchar(stringN)) == stringN
            ])
        } else {
          retmat <- retmat +
            as.matrix(ts[
              arearows,
              substr(names(ts), 1, nchar("retain(B)")) == "retain(B)"
            ])
          deadmat <- deadmat +
            as.matrix(ts[
              arearows,
              substr(names(ts), 1, nchar("dead(B)")) == "dead(B)"
            ])
          totcatchmat <- totcatchmat +
            as.matrix(ts[
              arearows,
              substr(names(ts), 1, nchar(stringB)) == stringB
            ])
        }
        totobscatchmat <- totobscatchmat +
          as.matrix(ts[
            arearows,
            substr(names(ts), 1, nchar("obs_cat")) == "obs_cat"
          ])
        Hratemat <- Hratemat +
          as.matrix(ts[
            arearows,
            substr(names(ts), 1, nchar(stringF)) == stringF
          ])
      }
    }

    # how many fleets have catch
    nfleets_with_catch <- ncol(totcatchmat)

    # ghost is a fleet with no catch yet still represented in timeseries
    ghost <- rep(TRUE, nfleets_with_catch)
    ghost[(1:nfleets_with_catch)[colSums(totcatchmat) > 0]] <- FALSE

    if (nfleets_with_catch == 1) {
      showlegend <- FALSE
    }
    # get fleet numbers from names used in timeseries
    fleetnums <- unlist(strsplit(
      dimnames(totcatchmat)[[2]],
      split = ":_",
      fixed = TRUE
    ))
    fleetnums <- as.numeric(fleetnums[seq(2, length(fleetnums), 2)])
    fleetnames <- fleetnames[fleetnums]

    # sort out order of fleets in plot
    if (order[1] == "default") {
      order <- nfleets_with_catch:1
    }

    # discards are difference between total catch and retained
    discmat <- totcatchmat - retmat

    discfracmat <- discmat / totcatchmat
    discfracmat[totcatchmat == 0] <- NA

    # sort out line types and colors
    if (length(fleetlty) < nfleets_with_catch) {
      fleetlty <- rep(fleetlty, nfleets_with_catch)
    }
    if (length(fleetpch) < nfleets_with_catch) {
      fleetpch <- rep(fleetpch, nfleets_with_catch)
    }

    if (fleetcols[1] == "default") {
      fleetcols <- rich.colors.short(nfleets_with_catch)
      if (nfleets_with_catch > 2) {
        fleetcols <- rich.colors.short(nfleets_with_catch + 1)[-1]
      }
    }

    # set default area-specific colors if not specified
    areacols <- get_areacols(areacols, nareas)

    # add total across seasons "mat2" indicates aggregation across seasons
    if (nseasons > 1) {
      catchyrs2 <- floor(ts[["Yr"]][goodrows & ts[["Seas"]] == 1]) # T/F indicator of the lines for which we want to plot catch
      subset <- ts[["Seas"]][goodrows] == 1
      retmat2 <- retmat[subset, ]
      totcatchmat2 <- totcatchmat[subset, ]
      # totcatchmat2Yr  <- ts[["Yr"]][subset]
      totobscatchmat2 <- totobscatchmat[subset, ]
      discmat2 <- discmat[subset, ]
      for (iseason in 2:nseasons) {
        subset <- ts[["Seas"]][goodrows] == iseason
        retmat2 <- retmat2 + retmat[subset, ]
        totcatchmat2 <- totcatchmat2 + totcatchmat[subset, ]
        totobscatchmat2 <- totobscatchmat2 + totobscatchmat[subset, ]
        discmat2 <- discmat2 + discmat[subset, ]
      }
    }
    # generic function to plot catch, landings, discards or harvest rates
    linefunc <- function(
      ymat,
      ylab,
      ymax = NULL,
      addtotal = TRUE,
      x = catchyrs
    ) {
      ymat <- as.matrix(ymat)
      if (addtotal & nfleets_with_catch > 1) {
        ytotal <- rowSums(ymat)
        if (is.null(ymax) || is.na(ymax)) ymax <- max(ytotal, na.rm = TRUE)
      } else {
        ytotal <- rep(NA, nrow(ymat))
        if (is.null(ymax) || is.na(ymax)) ymax <- max(ymat, na.rm = TRUE)
      }
      plot(
        x,
        ytotal,
        ylim = c(0, ymax),
        xlab = xlab,
        ylab = ylab,
        type = type,
        lwd = lwd,
        col = "black"
      )
      abline(h = 0, col = "grey")
      # abline(h=1,col="grey")
      for (f in 1:nfleets_with_catch) {
        # if there are any non-NA values and the max is > 0
        if (any(!is.na(ymat[, f])) && max(ymat[, f], na.rm = TRUE) > 0) {
          lines(
            x,
            ymat[, f],
            type = type,
            col = fleetcols[f],
            lty = fleetlty[f],
            lwd = lwd,
            pch = fleetpch[f]
          )
        }
      }
      if (showlegend & nfleets_with_catch != 1) {
        if (type == "l") {
          pchvec <- NA
        } else {
          pchvec <- c(1, fleetpch[!ghost])
        }
        if (sum(!ghost) > 1 & addtotal) {
          legend(
            legendloc,
            lty = fleetlty[!ghost],
            lwd = lwd,
            pch = pchvec,
            col = c("black", fleetcols[!ghost]),
            legend = c("Total", fleetnames[!ghost]),
            bty = "n"
          )
        } else {
          legend(
            legendloc,
            lty = fleetlty[!ghost],
            lwd = lwd,
            pch = pchvec,
            col = fleetcols[!ghost],
            legend = fleetnames[!ghost],
            bty = "n"
          )
        }
      }
      return(TRUE)
    } # end linefunc

    # function for stacked polygons
    stackfunc <- function(ymat, ylab, x = catchyrs, hashyrs = NULL) {
      ## call to function in plotrix (formerly copied into r4ss)
      if (length(order) == ncol(ymat)) {
        ymat <- ymat[, order]
      }
      stackpoly(
        x = x,
        y = ymat,
        border = "black",
        xlab = xlab,
        ylab = ylab,
        col = fleetcols[order],
        x.hash = hashyrs
      )
      if (showlegend) {
        legend(
          legendloc,
          fill = fleetcols[!ghost],
          legend = fleetnames[!ghost],
          bty = "n"
        )
      }
      return(TRUE)
    } # end stackfunc

    barfunc <- function(ymat, ylab, ymax = NULL, x = catchyrs, add = FALSE) {
      # adding labels to barplot as suggested by Mike Prager on R email list:
      #    http://tolstoy.newcastle.edu.au/R/e2/help/07/03/13013.html
      if (is.null(ymax)) {
        ylim <- NULL
      } else {
        ylim <- c(0, ymax)
      }
      if (length(order) == ncol(ymat)) {
        ymat <- ymat[, order]
      }
      mp <- barplot(
        t(ymat),
        xlab = xlab,
        ylab = ylab,
        axisnames = FALSE,
        ylim = ylim,
        col = fleetcols[order],
        space = 0,
        yaxs = "i",
        axes = FALSE,
        add = add
      )
      # Get major and minor multiples for choosing labels:
      ntick <- length(mp)
      if (nseasons > 1) {
        ntick <- floor(ntick / nseasons)
      }
      # formula for major/minor ticks
      {
        if (ntick < 16) {
          mult <- c(2, 2)
        } else if (ntick < 41) {
          mult <- c(5, 5)
        } else if (ntick < 101) {
          mult <- c(10, 5)
        } else {
          mult <- c(20, 5)
        }
      }
      # vertical axis
      if (nfleets_with_catch > 1) {
        ymax2 <- round(max(apply(ymat, 1, sum)))
      } else {
        ymax2 <- round(max(ymat))
      }
      yticks <- pretty(c(0, ymax2))
      if (addmax) {
        yticks <- sort(c(yticks, ymax2))
      }
      axis(2, at = yticks)
      # in seasonal models, x values don't contain integers:
      # 1987.125 1987.375 1987.625 1987.875
      # so reducing by smallest decimal value to have some integers
      x.shift <- x - min(x - floor(x), na.rm = TRUE)
      integer.index <- which(x.shift %% 1 == 0)
      label.index <- which(x.shift %% mult[1] == 0)
      minor.index <- which(x.shift %% mult[2] == 0)
      # Draw horizontal dotted lines
      abline(h = yticks, lty = 3, col = rgb(0, 0, 0, .3), lwd = 1)
      # Draw ticks at all integer years:
      axis(side = 1, at = mp[integer.index], labels = FALSE, tcl = -0.2)
      # Draw minor ticks:
      axis(side = 1, at = mp[minor.index], labels = FALSE, tcl = -0.5)
      # Draw major ticks & labels:
      axis(
        side = 1,
        at = mp[label.index],
        labels = x.shift[label.index],
        tcl = -0.7
      )

      # add legend
      if (showlegend) {
        legend(
          legendloc,
          fill = fleetcols[!ghost],
          legend = fleetnames[!ghost],
          bty = "n"
        )
      }
      return(TRUE)
    }

    # choose one of the above functions
    if (catchbars) {
      stackfunc <- barfunc # unsophisticated way to implement choice of plot type
    }

    makeplots <- function(subplot) {
      a <- FALSE
      if (subplot == 1) {
        if (max(discmat, na.rm = TRUE) > 0) {
          # if there are discards, label is Landings
          label <- labels[3] # "Landings", # 3
        } else {
          # if not, this is total catch
          label <- labels[4] # "Total catch", # 4
        }
        a <- linefunc(
          ymat = retmat,
          ymax = ymax,
          ylab = label,
          addtotal = TRUE
        )
      }
      if (subplot == 2) {
        if (max(discmat, na.rm = TRUE) > 0) {
          # if there are discards, label is Landings
          label <- labels[3] # "Landings", # 3
        } else {
          # if not, this is total catch
          label <- labels[4] # "Total catch", # 4
        }
        a <- stackfunc(ymat = retmat, ymax = ymax, ylab = label, add = add)
      }
      # if observed catch differs from estimated by more than 0.1%, then make plot to compare
      if (
        subplot == 3 &
          diff(range(retmat - totobscatchmat, na.rm = TRUE)) /
            max(totobscatchmat, na.rm = TRUE) >
            0.001
      ) {
        a <- linefunc(
          ymat = retmat,
          ylab = paste(labels[9], labels[3]),
          addtotal = FALSE,
          ymax = max(totobscatchmat, retmat)
        )
        for (f in 1:nfleets_with_catch) {
          if (max(totobscatchmat[, f], na.rm = TRUE) > 0) {
            lines(
              catchyrs,
              totobscatchmat[, f],
              type = type,
              col = fleetcols[f],
              lty = 3,
              lwd = lwd,
              pch = 4
            )
          }
        }
        legend(
          legendloc,
          lty = c(fleetlty[!ghost], rep(3, sum(!ghost))),
          lwd = lwd,
          pch = c(fleetpch[!ghost], rep(4, sum(!ghost))),
          col = fleetcols[!ghost],
          legend = c(fleetnames[!ghost], paste(fleetnames[!ghost], "obs.")),
          bty = "n"
        )
      }
      # if there are discards, add additional plots
      if (max(discmat, na.rm = TRUE) > 0) {
        if (subplot == 4) {
          a <- linefunc(
            ymat = totcatchmat,
            ymax = ymax,
            ylab = labels[4],
            addtotal = TRUE
          )
        }
        if (subplot == 5 & nfleets_with_catch > 1) {
          a <- stackfunc(
            ymat = totcatchmat,
            ymax = ymax,
            ylab = labels[4],
            add = add
          )
        }
        if (subplot == 6) {
          a <- linefunc(
            ymat = discmat,
            ymax = ymax,
            ylab = labels[5],
            addtotal = TRUE
          )
        }
        if (subplot == 7 & nfleets_with_catch > 1) {
          a <- stackfunc(ymat = discmat, ymax = ymax, ylab = labels[5])
        }
        if (subplot == 8) {
          a <- linefunc(
            ymat = discfracmat,
            ymax = ymax,
            ylab = labels[6],
            addtotal = FALSE
          )
        }
      }
      if (subplot == 9) {
        a <- linefunc(
          ymat = Hratemat,
          ymax = ymax,
          ylab = ylabF,
          addtotal = FALSE
        )
      }
      if (nseasons > 1) {
        if (subplot == 10) {
          a <- linefunc(
            ymat = retmat2,
            ymax = ymax,
            ylab = paste(labels[3], labels[10]),
            addtotal = TRUE,
            x = catchyrs2
          )
        }
        if (subplot == 11 & nfleets_with_catch > 1) {
          a <- stackfunc(
            ymat = retmat2,
            ymax = ymax,
            ylab = paste(labels[3], labels[10]),
            x = catchyrs2
          )
        }
        if (max(discmat, na.rm = TRUE) > 0) {
          if (subplot == 12) {
            a <- linefunc(
              ymat = totcatchmat2,
              ymax = ymax,
              ylab = paste(labels[4], labels[10]),
              addtotal = TRUE,
              x = catchyrs2
            )
          }
          if (subplot == 13 & nfleets_with_catch > 1) {
            a <- stackfunc(
              ymat = totcatchmat2,
              ymax = ymax,
              ylab = paste(labels[4], labels[10]),
              x = catchyrs2
            )
          }
          if (subplot == 14) {
            a <- linefunc(
              ymat = discmat2,
              ymax = ymax,
              ylab = paste(labels[5], labels[10]),
              addtotal = TRUE,
              x = catchyrs2
            )
          }
          if (subplot == 15 & nfleets_with_catch > 1) {
            a <- stackfunc(
              ymat = discmat2,
              ymax = ymax,
              ylab = paste(labels[5], labels[10]),
              x = catchyrs2
            )
          }
        }
      }
      if (max(discmat, na.rm = TRUE) > 0 & subplot == 16) {
        a <- stackfunc(ymat = deadmat, ymax = ymax, ylab = "", add = add)
      }
      #### turning off message for now
      ## if (verbose & a) {
      ##   message("  finished catch subplot", subplot_names[subplot])
      ## }
      return(a)
    } # end makeplots

    if (plot) {
      for (isubplot in subplots) {
        makeplots(isubplot)
      }
    }

    if (print) {
      for (isubplot in subplots) {
        a <- FALSE
        myname <- subplot_names[isubplot]
        badstrings <- c(":", "  ", "__")
        for (i in seq_along(badstrings)) {
          myname <- gsub(
            pattern = badstrings[i],
            replacement = " ",
            x = myname,
            fixed = T
          )
        }
        myname <- gsub(" ", "_", myname)
        file <- paste0("catch", myname, ".png")
        # default caption is based on the subplot_names vector defined at the top
        caption <- substring(myname, 3)
        # add to caption for a few plots
        if (
          exists("equil.catch.vec") &&
            max(equil.catch.vec, na.rm = TRUE) > 0 &&
            isubplot %in% 1:9
        ) {
          caption <- paste(
            caption,
            "<br>Note: the first ",
            nseasons,
            " values represent the unfinished equilibrium catch",
            " divided equally among all seasons."
          )
        }
        if (isubplot == 3 & any(catch_units == 2, na.rm = TRUE)) {
          caption <- paste0(
            caption,
            ". Catch input in numbers is compared here to catch in biomass, ",
            "making this figure less useful for models like this with catch in numbers. ",
            "Future versions of this plot will be improve to ",
            "compare catch in the same units."
          )
        }
        plotinfo2 <- save_png(
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
        # "a" is TRUE/FALSE indicator that plot got produced
        a <- makeplots(isubplot)
        dev.off()
        # delete empty files if they somehow got created
        if (!a & file.exists(file)) {
          file.remove(file)
        }
        if (a) {
          plotinfo <- plotinfo2
        }
      }
    }

    totcatchmat <- as.data.frame(totcatchmat)
    totobscatchmat <- as.data.frame(totobscatchmat)
    names(totcatchmat) <- fleetnames[1:nfleets_with_catch]
    names(totobscatchmat) <- fleetnames[1:nfleets_with_catch]
    totcatchmat[["Yr"]] <- catchyrs
    totobscatchmat[["Yr"]] <- catchyrs
    returnlist <- list()
    returnlist[["totcatchmat"]] <- totcatchmat
    returnlist[["totobscatchmat"]] <- totobscatchmat
    if (nseasons > 1) {
      totcatchmat2 <- as.data.frame(totcatchmat2)
      names(totcatchmat2) <- fleetnames[1:nfleets_with_catch]
      # totcatchmat2[["Yr"]] <- totcatchmat2Yr
      returnlist[["totcatchmat2"]] <- totcatchmat2
    }
    if (!is.null(plotinfo)) {
      plotinfo[["category"]] <- "Catch"
    }
    returnlist[["plotinfo"]] <- plotinfo
    return(invisible(returnlist))
  }
