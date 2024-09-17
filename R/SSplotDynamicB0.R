#' Plot Dynamic B0
#'
#' Plots the spawning output with and without fishing mortality
#'
#' @template replist
#' @param ylab Y-axis label. Default is "Spawning biomass (t)" which is replaced
#' by `replist[["SpawnOutputLabel"]]` for models with 
#' `replist[["SpawnOutputUnits"]] == "numbers"`
#' @param equilibrium Show equilibrium in plot? Applies whether "yrs" is specified
#' or not.
#' @param forecast Show forecast years in plot? Only applies if yrs = "all".
#' @param yrs Which years to include. Default "all" will show startyr to endyr + 1
#' modified by the arguments `forecast`.
#' @template plot
#' @template print
#' @template plotdir
#' @template verbose
#' @param uncertainty Show 95% uncertainty intervals around point estimates?
#' These intervals will only appear when uncertainty in the dynamic B0
#' estimates is available via the control file settings for
#' "read specs for more stddev reporting".
#' @template legend
#' @param legendlabels Character vector with labels for the unfished
#' equilibrium point (if `equilibrium = TRUE`) and the two lines showing
#' spawning biomass or output without and with fishing.
#' @template legendloc
#' @param col Optional vector of colors to be used for the two lines
#' (single value will apply to both lines).
#' @param lty Optional vector of line types to be used for the two lines
#' (single value will apply to both lines).
#' @param lwd Optional vector of line widths to be used for the two lines.
#'  Single value will apply to both lines.
#' @param add add to existing plot
#' @template pwidth
#' @template pheight
#' @template punits
#' @template ptsize
#' @template res
#' @template mainTitle
#' @template mar
#' @author Ian G. Taylor
#' @export
#' @seealso [SSplotTimeseries()]
SSplotDynamicB0 <- function(replist,
                            ylab = "Spawning biomass (t)",
                            equilibrium = TRUE,
                            forecast = FALSE,
                            yrs = "all",
                            plot = TRUE, print = FALSE, plotdir = "default", verbose = TRUE,
                            uncertainty = TRUE,
                            legend = TRUE,
                            legendlabels = c("equilibrium", "without fishing", "with fishing"),
                            legendloc = "bottom",
                            col = c("blue", "red"),
                            lty = 1,
                            lwd = 2,
                            add = FALSE,
                            pwidth = 6.5, pheight = 5.0, punits = "in", res = 300, ptsize = 10,
                            mainTitle = FALSE,
                            mar = NULL) {
  Dynamic_Bzero <- replist[["Dynamic_Bzero"]]
  if (is.null(Dynamic_Bzero)) {
    warning('No element "Dynamic_Bzero" in replist input')
    return()
  }
  if (!"SSB_nofishing" %in% names(replist[["Dynamic_Bzero"]])) {
    warning("Skipping Dynamic B0 plot (not yet working for this model configuration)")
    return()
  }

  # check if spawning output rather than spawning biomass is plotted
  if (is.null(replist[["SpawnOutputUnits"]]) ||
    is.na(replist[["SpawnOutputUnits"]]) ||
    replist[["SpawnOutputUnits"]] == "numbers") { # quantity from test in SS_output
    if (ylab == "Spawning biomass (t)") {
      ylab <- replist[["SpawnOutputLabel"]]
    }
  }

  # set default plot margins
  if (is.null(mar)) {
    if (mainTitle) {
      main <- "Dynamic B0"
      mar <- c(5, 4, 4, 2) + 0.1
    } else {
      main <- ""
      mar <- c(5, 4, 2, 2) + 0.1
    }
  }

  # repeat inputs for line style if needed
  if (length(col) == 1) {
    col <- rep(col, 2)
  }
  if (length(lty) == 1) {
    lty <- rep(lty, 2)
  }
  if (length(lwd) == 1) {
    lwd <- rep(lwd, 2)
  }

  plotinfo <- NULL
  # set plot directory
  if (plotdir == "default") {
    plotdir <- replist[["inputs"]][["dir"]]
  }

  # set default range of years
  if (yrs[1] == "all") {
    yrs <- replist[["startyr"]]:(replist[["endyr"]] + 1)
    if (forecast) {
      yrs <- replist[["startyr"]]:max(Dynamic_Bzero[["Yr"]])
    }
  }

  # equilibrium year shifted by 1 because INIT point isn't shown
  sub_equil <- Dynamic_Bzero[["Era"]] == "VIRG"
  equil_yr <- Dynamic_Bzero[["Yr"]][sub_equil] + 1

  # set ymax
  ymax <- max(
    ifelse(equilibrium,
      Dynamic_Bzero[["SSB"]][sub_equil],
      NA
    ),
    Dynamic_Bzero[["SSB"]][Dynamic_Bzero[["Yr"]] %in% yrs],
    Dynamic_Bzero[["SSB_nofishing"]][Dynamic_Bzero[["Yr"]] %in% yrs],
    na.rm = TRUE
  )

  # get subsets of derived quantities and extract year value
  quants <- replist[["derived_quants"]]
  quants_SSB <- quants[grep("SSB_", quants[["Label"]]), ]
  quants_SSB_nofishing <- quants[grep("Dyn_Bzero_", quants[["Label"]]), ]

  if (uncertainty & nrow(quants_SSB_nofishing) == 0) {
    uncertainty <- FALSE
    warning(
      "Dynamic B0 not found in derived quantities, ",
      "changing uncertainty to FALSE. ",
      "To get uncertainty, modify control file under ",
      "'read specs for more stddev reporting'."
    )
  }

  if (uncertainty) {
    # calculate intervals

    # add columns to store info
    Dynamic_Bzero[["SSB_lo"]] <- NA
    Dynamic_Bzero[["SSB_hi"]] <- NA
    Dynamic_Bzero[["SSB_nofishing_lo"]] <- NA
    Dynamic_Bzero[["SSB_nofishing_hi"]] <- NA

    # get year for each row
    quants_SSB[["Yr"]] <- substring(quants_SSB[["Label"]],
      first = nchar("SSB_") + 1
    )
    quants_SSB_nofishing[["Yr"]] <- substring(quants_SSB_nofishing[["Label"]],
      first = nchar("Dyn_Bzero_") + 1
    )

    # get unfished equilibrium uncertainty intervals (same with/without fishing)
    Dynamic_Bzero[sub_equil, c("SSB_lo", "SSB_hi")] <-
      qnorm(
        p = c(0.025, 0.975),
        mean = quants["SSB_Virgin", "Value"],
        sd = quants["SSB_Virgin", "StdDev"]
      )
    # loop over years to get uncertainty intervals
    for (y in Dynamic_Bzero[["Yr"]]) {
      # fill in SSB uncertainty
      if (y %in% quants_SSB[["Yr"]]) {
        Dynamic_Bzero[Dynamic_Bzero[["Yr"]] == y, c("SSB_lo", "SSB_hi")] <-
          qnorm(
            p = c(0.025, 0.975),
            mean = quants_SSB[paste0("SSB_", y), "Value"],
            sd = quants_SSB[paste0("SSB_", y), "StdDev"]
          )
      }
      # fill in dynamic B0 uncertainty
      if (y %in% quants_SSB_nofishing[["Yr"]]) {
        Dynamic_Bzero[Dynamic_Bzero[["Yr"]] == y, c("SSB_nofishing_lo", "SSB_nofishing_hi")] <-
          qnorm(
            p = c(0.025, 0.975),
            mean = quants_SSB_nofishing[paste0("Dyn_Bzero_", y), "Value"],
            sd = quants_SSB_nofishing[paste0("Dyn_Bzero_", y), "StdDev"]
          )
      }
    }
    # update ymax
    ymax <- max(ymax,
      ifelse(equilibrium,
        Dynamic_Bzero[["SSB_hi"]][sub_equil],
        NA
      ),
      Dynamic_Bzero[["SSB_hi"]][Dynamic_Bzero[["Yr"]] %in% yrs],
      Dynamic_Bzero[["SSB_nofishing_hi"]][Dynamic_Bzero[["Yr"]] %in% yrs],
      na.rm = TRUE
    )
  }

  plotfun <- function() {
    if (!add) {
      plot(0,
        type = "n", , xlab = "Year", ylab = ylab, xlim = range(yrs),
        ylim = c(0, 1.1 * ymax),
        yaxs = "i",
        cex.lab = 1.0, cex.axis = 1.0, cex = 0.7,
        mar = mar,
        main = main
      )
    }
    sub <- Dynamic_Bzero[["Yr"]] %in% yrs & Dynamic_Bzero[["Era"]] != "VIRG"


    if (uncertainty) {
      # add intervals to plot

      if (equilibrium) {
        # add equilibrium intervals
        arrows(
          x0 = equil_yr,
          y0 = Dynamic_Bzero[sub_equil, "SSB_lo"],
          x1 = equil_yr,
          y1 = Dynamic_Bzero[sub_equil, "SSB_hi"],
          length = 0.01, angle = 90, code = 3, col = col[1],
          lwd = 1
        )
      }

      # function to add shaded uncertainty intervals behind line
      addpoly <- function(yrvec, lower, upper, col) {
        lower[lower < 0] <- 0 # max of value or 0
        good <- !is.na(lower) & !is.na(upper)
        polygon(
          x = c(yrvec[good], rev(yrvec[good])),
          y = c(lower[good], rev(upper[good])),
          border = NA, col = col
        )
      }

      # add polygons around time series
      addpoly(
        yrvec = Dynamic_Bzero[["Yr"]][sub],
        lower = Dynamic_Bzero[["SSB_lo"]][sub],
        upper = Dynamic_Bzero[["SSB_hi"]][sub],
        col = adjustcolor(col[2], alpha.f = 0.1)
      )
      addpoly(
        yrvec = Dynamic_Bzero[["Yr"]][sub],
        lower = Dynamic_Bzero[["SSB_nofishing_lo"]][sub],
        upper = Dynamic_Bzero[["SSB_nofishing_hi"]][sub],
        col = adjustcolor(col[1], alpha.f = 0.1)
      )
    } # end check for uncertainty

    lines(
      x = Dynamic_Bzero[["Yr"]][sub],
      y = Dynamic_Bzero[["SSB"]][sub],
      lwd = lwd[2], lty = lty[2], col = col[2]
    )
    lines(
      x = Dynamic_Bzero[["Yr"]][sub],
      y = Dynamic_Bzero[["SSB_nofishing"]][sub],
      lwd = lwd[1], lty = lty[1], col = col[1]
    )
    if (equilibrium) {
      points(
        x = equil_yr,
        y = Dynamic_Bzero[["SSB"]][sub_equil],
        cex = 1.5,
        pch = 16,
        col = col[1]
      )
      # add to vectors used in the legend
      lty <- c(NA, lty)
      lwd <- c(NA, lwd)
      col <- c(col[1], col)
      pch <- c(16, NA, NA)
    } else {
      pch <- NA
      legendlabels <- legendlabels[2:3]
    }
    if (legend) {
      legend(
        x = legendloc,
        legend = legendlabels,
        col = col,
        lwd = lwd,
        lty = lty,
        pch = pch,
        pt.cex = 1.5,
        bty = "n",
        ncol = ifelse(equilibrium, 3, 2)
      )
    }
  }
  if (plot) {
    plotfun()
  }
  if (print) {
    caption <- paste0(
      "Dynamic B0 plot. The lower line shows the time series ",
      "of estimated ", ylab, " in the presence of fishing ",
      "mortality. The upper line shows the time series that ",
      "could occur under the same dynamics (including ",
      "deviations in recruitment), but without fishing."
    )
    if (equilibrium) {
      caption <- paste0(
        caption,
        " The point at the left represents the unfished equilibrium."
      )
    }

    file <- "ts_DynamicB0.png"
    plotinfo <- save_png(
      plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
      pheight = pheight, punits = punits, res = res, ptsize = ptsize,
      caption = caption
    )
    plotfun()
    dev.off()
    if (!is.null(plotinfo)) {
      plotinfo[["category"]] <- "Timeseries"
    }
  }
  if (verbose) message("Plotting Dynamic B0")
  return(invisible(plotinfo))
}
