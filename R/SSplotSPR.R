#' Plot Spawning Potential Ratio (SPR) quantities.
#'
#' Plot time series of SPR, 1-SPR, the chosen SPR ratio and the phase plot.
#'
#' @template replist
#' @param add add to existing plot (not yet implemented)
#' @template plot
#' @template print
#' @param uncertainty include plots showing uncertainty?
#' @param subplots vector controlling which subplots to create
#' Numbering of subplots is as follows:
#'   1. timeseries of SPR,
#'   2. timeseries of 1 - SPR,
#'   3. timeseries of SPR ratio (as specified in the starter file), and
#'   4. phase plot of Biomass ratio vs SPR ratio (as specified in the
#' starter file).
#' @param forecastplot Include forecast years in plot?
#' @param col1 first color used
#' @param col2 second color used
#' @param col3 third color used
#' @param col4 fourth color used
#' @param sprtarg F/SPR proxy target. "default" chooses based on model output,
#' where models which have SPR_std_basis = 0 or 1 specified in the starter
#' file will use the SPR target specified in the forecast file. Models which
#' have SPR_std_basis = 2 will use SPR at MSY for the SPR target
#' and models which have the SPR_std_basis = 3 will use
#' SPR at Btarget for the SPR target in these plots. Zero or negative values of
#' sprtarg input here will cause no horizontal line to be plotted.
#' @param btarg target depletion to be used in plots showing depletion. May be
#' omitted by setting to NA. "default" chooses based on model output.
#' @param minbthresh minimum biomass threshold to be used in plots
#' showing depletion. May be omitted by setting to NA. "default" chooses
#' based on model output.
#' @template labels
#' @template pwidth
#' @template pheight
#' @template pheight_tall
#' @template punits
#' @template res
#' @template ptsize
#' @template cex.main
#' @template plotdir
#' @template verbose
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso [SS_plots()], [SS_output()]
SSplotSPR <-
  function(
    replist,
    add = FALSE,
    plot = TRUE,
    print = FALSE,
    uncertainty = TRUE,
    subplots = 1:4,
    forecastplot = FALSE,
    col1 = "black",
    col2 = "blue",
    col3 = "darkolivegreen4",
    col4 = "brown4",
    sprtarg = "default",
    btarg = "default",
    minbthresh = "default",
    labels = c(
      "Year", # 1
      "SPR", # 2
      "1-SPR", # 3
      "Relative fishing intensity", # 4
      "Fraction of unfished spawning output" # 5
    ),
    pwidth = 6.5,
    pheight = 5.0,
    pheight_tall = 5.0,
    punits = "in",
    res = 300,
    ptsize = 10,
    cex.main = 1,
    plotdir = "default",
    verbose = TRUE
  ) {
    # table to store information on each plot
    plotinfo <- NULL

    if (plotdir == "default") {
      plotdir <- replist[["inputs"]][["dir"]]
    }

    sprseries <- replist[["sprseries"]]
    derived_quants <- replist[["derived_quants"]]
    nseasons <- replist[["nseasons"]]
    endyr <- replist[["endyr"]]
    SPRratioLabel <- replist[["SPRratioLabel"]]

    # message about skipping plots
    if (is.null(sprseries)) {
      message("Skipping SPR plots: no output available")
      return()
    }
    # at least one non-converged model had NaN values for all years
    if (all(is.nan(sprseries[["SPR"]]))) {
      warning("NaN values in SPR series, skipping plots")
      return()
    }

    # get SPR target and associated label based on forecast specified SPR target or
    # the denominator of the SPR ratio as specified in the starter file
    sprtarg_label <- "SPR target"
    if (sprtarg == "default") {
      sprtarg <- replist[["sprtarg"]]
      if (grepl("SPR_MSY", SPRratioLabel)) {
        sprtarg <- replist[["derived_quants"]]["SPR_MSY", "Value"]
        sprtarg_label <- "SPR at MSY"
      }
      if (grepl("SPR_at_B", SPRratioLabel)) {
        sprtarg <- replist[["derived_quants"]]["SPR_Btgt", "Value"]
        sprtarg_label <- substring(
          SPRratioLabel,
          first = nchar("(1-SPR)/(1-") + 1,
          last = nchar("(1-SPR)/(1-SPR_at_B48%")
        )
      }
    }
    if (btarg == "default") {
      btarg <- replist[["btarg"]]
    }
    if (minbthresh == "default") {
      minbthresh <- replist[["minbthresh"]]
    }

    # choose which points to plot
    good <- sprseries[["Yr"]] <= endyr
    if (forecastplot) {
      good <- rep(TRUE, nrow(sprseries))
    }

    spr_timeseries <- function() {
      if (!add) {
        plot(
          0,
          xlab = labels[1],
          ylab = labels[2],
          xlim = range(sprseries[["Yr"]][good]),
          ylim = c(
            0,
            max(1, max(sprseries[["SPR"]][!is.na(sprseries[["SPR"]])]))
          ),
          type = "n"
        )
      }
      lines(
        sprseries[["Yr"]][good],
        sprseries[["SPR"]][good],
        type = "o",
        col = col2
      )
      if (sprtarg > 0) {
        abline(h = sprtarg, col = col4, lty = 2)
      }
      abline(h = 0, col = "grey")
      abline(h = 1, col = "grey")
    }

    if (1 %in% subplots) {
      if (plot) {
        spr_timeseries()
      }
      if (print) {
        file <- "SPR1_series.png"
        caption <- "Timeseries of SPR"
        if (sprtarg > 0) {
          caption <- paste0(
            caption,
            ". Horizontal line is at ",
            sprtarg_label,
            ": ",
            round(sprtarg, 3)
          )
        }
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
        spr_timeseries()
        dev.off()
      }
    }

    # temporary disable multi-season models until code cleanup
    if (2 %in% subplots) {
      if (nseasons > 1) {
        message(
          "Skipped 1-SPR plot because it's not yet configured for multi-season models\n"
        )
      }
      if (nseasons == 1) {
        minus_spr_timeseries <- function() {
          if (!add) {
            plot(
              0,
              xlim = range(sprseries[["Yr"]][good]),
              xlab = labels[1],
              ylab = labels[3],
              ylim = c(0, 1),
              type = "n"
            )
          }
          lines(
            sprseries[["Yr"]][good],
            (1 - sprseries[["SPR"]][good]),
            type = "o",
            col = col2
          )
          if (sprtarg > 0) {
            abline(h = (1 - sprtarg), col = col4, lty = 2)
          }
          abline(h = 0, col = "grey")
          abline(h = 1, col = "grey")
        }

        if (plot) {
          minus_spr_timeseries()
        }
        if (print) {
          file <- "SPR2_minusSPRseries.png"
          caption <- "Timeseries of 1-SPR"
          if (sprtarg > 0) {
            caption <- paste0(
              caption,
              ". Horizontal line is at 1 - ",
              sprtarg_label,
              ": ",
              "1 - ",
              round(sprtarg, 3),
              " = ",
              round(1 - sprtarg, 3)
            )
          }
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
          minus_spr_timeseries()
          dev.off()
        }
      } # end check for nseasons == 1
    } # end check for subplot 2

    # get SPR ratio and uncertainty (used in suplots 3 and 4)
    SPRratio <- derived_quants[
      grep(
        "^SPRratio_",
        derived_quants[["Label"]]
      ),
    ]
    # add "Yr" column based on string in label
    SPRratio[["Yr"]] <- as.numeric(substring(SPRratio[["Label"]], 10))
    # add "period" column to populate with different eras
    SPRratio[["period"]] <- "fore"
    SPRratio[["period"]][SPRratio[["Yr"]] <= (endyr)] <- "time"
    # calculate 95% intervals
    SPRratio[["upper"]] <- qnorm(
      p = 0.975,
      mean = SPRratio[["Value"]],
      sd = SPRratio[["StdDev"]]
    )
    SPRratio[["lower"]] <- qnorm(
      p = 0.025,
      mean = SPRratio[["Value"]],
      sd = SPRratio[["StdDev"]]
    )
    # assume that SPR ratio is non-negative
    SPRratio[["lower"]] <- pmax(
      SPRratio[["lower"]],
      0
    )

    # get B ratio and uncertainty (used in suplots 3 and 4)
    Bratio <- derived_quants[grep("^Bratio_", derived_quants[["Label"]]), ]
    # add "Yr" column based on string in label
    Bratio[["Yr"]] <- as.numeric(substring(Bratio[["Label"]], 8))
    # add "period" column to populate with different eras
    Bratio[["period"]] <- "fore"
    Bratio[["period"]][Bratio[["Yr"]] <= (endyr)] <- "time"
    # calculate 95% intervals
    Bratio[["upper"]] <- qnorm(
      p = 0.975,
      mean = Bratio[["Value"]],
      sd = Bratio[["StdDev"]]
    )
    Bratio[["lower"]] <- qnorm(
      p = 0.025,
      mean = Bratio[["Value"]],
      sd = Bratio[["StdDev"]]
    )
    Bratio_endyr_SD <- Bratio[["StdDev"]][Bratio[["Yr"]] == endyr]
    SPRratio_endyr_SD <- SPRratio[["StdDev"]][SPRratio[["Yr"]] == endyr]
    B_SPR_endyr_corr <- replist[["CoVar"]][
      replist[["CoVar"]][["label.i"]] %in%
        paste0("Bratio_", endyr) &
        replist[["CoVar"]][["label.j"]] %in%
          paste0("SPRratio_", endyr),
      "corr"
    ]

    # x-axis label for phase plot
    xlab <- paste0(labels[5], ": ", replist[["Bratio_label"]])
    # y-axis label and limits for time series and phase plot
    ylab <- paste0(labels[4], ": ", SPRratioLabel)
    # if not a ratio, take out the word "Relative"
    if (!grepl(pattern = "/", x = ylab, fixed = TRUE)) {
      ylab <- gsub(
        pattern = "Relative fishing",
        replacement = "Fishing",
        x = ylab
      )
    }
    ylim <- c(0, max(1, SPRratio[["upper"]][SPRratio[["period"]] == "time"]))

    spr_ratio_timeseries <- function() {
      if (!add) {
        plot(
          SPRratio[["Yr"]][SPRratio[["period"]] == "time"],
          SPRratio[["Value"]][SPRratio[["period"]] == "time"],
          xlab = labels[1],
          ylim = ylim,
          ylab = ylab,
          type = "n"
        )
      }
      lines(
        SPRratio[["Yr"]][SPRratio[["period"]] == "time"],
        SPRratio[["Value"]][SPRratio[["period"]] == "time"],
        type = "o",
        col = col2
      )
      abline(h = 0, col = "grey")
      abline(h = 1, col = col4)
      if (sprtarg > 0 & SPRratioLabel == "1-SPR") {
        abline(h = (1 - sprtarg), col = col4, lty = 2)
      }
      #### "management target" doesn't make sense for all settings and
      #### is probably a limit, not a target, even where it does
      ## text((min(SPRratio[["Yr"]]) + 4), (1 + 0.02),
      ##   "Management target",
      ##   adj = 0
      ## )
      lines(
        SPRratio[["Yr"]][SPRratio[["period"]] == "time"],
        SPRratio[["upper"]][SPRratio[["period"]] == "time"],
        col = col2,
        lty = "dashed"
      )
      lines(
        SPRratio[["Yr"]][SPRratio[["period"]] == "time"],
        SPRratio[["lower"]][SPRratio[["period"]] == "time"],
        col = col2,
        lty = "dashed"
      )
    }
    if (3 %in% subplots) {
      if (plot) {
        spr_ratio_timeseries()
      }
      if (print) {
        file <- "SPR3_ratiointerval.png"
        caption <- paste(
          "Timeseries of SPR ratio:",
          SPRratioLabel
        )
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
        spr_ratio_timeseries()
        dev.off()
      }
    }

    make.phase.plot.MLE <- function(x.max = 1.3, y.max = 1.3, period = "time") {
      # this function modified from make.phase.plot for Pacific Hake at
      # https://github.com/pacific-hake/hake-assessment/blob/master/R/figures-timeseries.R

      # Plots the relative fishing intensity in year t against
      # fraction of unfished spawning output in year t

      # find years that are shared by both sets of outputs
      shared_yrs <- intersect(
        Bratio[["Yr"]][Bratio[["period"]] %in% period],
        SPRratio[["Yr"]][SPRratio[["period"]] %in% period]
      )
      Bratio_vals <- Bratio[["Value"]][Bratio[["Yr"]] %in% shared_yrs]
      SPRratio_vals <- SPRratio[["Value"]][SPRratio[["Yr"]] %in% shared_yrs]
      if (length(Bratio_vals) != length(SPRratio_vals)) {
        message(
          "Bratio and SPRratio vectors are different in length,",
          "skipping phase plot."
        )
        return()
      }
      # put things into a data.frame to keep track of relationships
      # between year, Bratio, SPRratio and color
      phase_df <- data.frame(
        yr = shared_yrs,
        Bratio = Bratio_vals,
        SPRratio = SPRratio_vals,
        col = viridis(length(shared_yrs))
        # col = rev(rich.colors.short(n = length(shared_yrs)))
      )
      # make empty plot
      plot(
        phase_df[["Bratio_vals"]],
        phase_df[["SPRratio_vals"]],
        type = "n",
        pch = 20,
        xlim = c(0, 1.1 * max(1, Bratio_vals)),
        ylim = c(0, 1.1 * max(1, SPRratio_vals[!is.na(SPRratio_vals)])),
        xlab = xlab,
        ylab = ylab,
        xaxs = "i",
        yaxs = "i"
      )
      # deal with warnings about zero-length arrow
      old_warn <- options()[["warn"]] # previous setting
      options(warn = -1) # turn off "zero-length arrow" warning
      # add arrows
      arrows(
        x0 = phase_df[["Bratio"]][-nrow(phase_df)],
        y0 = phase_df[["SPRratio"]][-nrow(phase_df)],
        x1 = phase_df[["Bratio"]][-1],
        y1 = phase_df[["SPRratio"]][-1],
        length = 0.09,
        col = phase_df[["col"]][-1]
      )
      options(warn = old_warn) # returning to old value
      # add points for each year:
      points(
        x = phase_df[["Bratio"]],
        y = phase_df[["SPRratio"]],
        pch = 21,
        col = 1,
        bg = phase_df[["col"]]
      )

      # add uncertainty intervals for final year:
      # vertical interval
      segments(
        x0 = Bratio[["Value"]][Bratio[["Yr"]] == endyr],
        y0 = SPRratio[["lower"]][SPRratio[["Yr"]] == endyr],
        x1 = Bratio[["Value"]][Bratio[["Yr"]] == endyr],
        y1 = SPRratio[["upper"]][SPRratio[["Yr"]] == endyr],
        col = rgb(0, 0, 0, 0.5)
      )
      # horizontal interval
      segments(
        x0 = Bratio[["lower"]][Bratio[["Yr"]] == endyr],
        y0 = SPRratio[["Value"]][SPRratio[["Yr"]] == endyr],
        x1 = Bratio[["upper"]][Bratio[["Yr"]] == endyr],
        y1 = SPRratio[["Value"]][SPRratio[["Yr"]] == endyr],
        col = rgb(0, 0, 0, 0.5)
      )

      # get mean and variance-covariance matrix of bivariate normal
      # joint distribution based on normal approxmation from ADMB
      if (
        isTRUE(
          Bratio_endyr_SD > 0 &
            SPRratio_endyr_SD > 0 &
            !is.null(B_SPR_endyr_corr)
        )
      ) {
        mu <- c(
          Bratio[["Value"]][Bratio[["Yr"]] == endyr],
          SPRratio[["Value"]][SPRratio[["Yr"]] == endyr]
        )
        sigma <- matrix(
          data = c(
            Bratio_endyr_SD^2,
            Bratio_endyr_SD * SPRratio_endyr_SD * B_SPR_endyr_corr,
            Bratio_endyr_SD * SPRratio_endyr_SD * B_SPR_endyr_corr,
            SPRratio_endyr_SD^2
          ),
          nrow = 2,
          ncol = 2
        )

        # calculate 95% ellipse showing correlation among the points
        # calculate points using code from mixtools::ellipse()
        # which has GPL-2 | GPL-3 license
        # https://CRAN.R-project.org/package=mixtools
        es <- eigen(sigma)
        e1 <- es[["vectors"]] %*% diag(sqrt(es[["values"]]))
        r1 <- sqrt(qchisq(0.95, 2))
        theta <- seq(0, 2 * pi, len = 250)
        v1 <- cbind(r1 * cos(theta), r1 * sin(theta))
        pts <- t(mu - (e1 %*% t(v1)))

        # add polygon
        polygon(pts, col = gray(0, alpha = 0.2), border = NA)
      }
      # label a sequence of years
      # choice of years is first, last, and multiple of 10
      label_yrs <- unique(c(
        min(shared_yrs),
        shared_yrs[which(shared_yrs %% 10 == 0)],
        max(shared_yrs)
      ))
      df_rows <- which(phase_df[["yr"]] %in% label_yrs)
      text(
        x = phase_df[df_rows, "Bratio"],
        y = phase_df[df_rows, "SPRratio"],
        labels = phase_df[df_rows, "yr"],
        cex = 0.6,
        adj = c(-0.5, -0.3), # above and to the right
        col = adjustcolor(
          phase_df[df_rows, "col"],
          offset = c(-0.5, -0.5, -0.5, 0)
        )
      )

      # add lines at 1.0 in each dimension
      abline(
        h = 1,
        v = 1,
        lty = 2,
        col = rgb(0, 0, 0, 0.4)
      )

      # if Bratio denominator is B0, then add line at btarg and minbthresh
      if (replist[["Bratio_label"]] == "B/B_0" & btarg > 0) {
        abline(
          v = c(btarg, minbthresh),
          lty = 2,
          col = rgb(0, 0, 0, 0.4)
        )
      }

      # if SPRratio denominator is not SPR target,
      # then add a line at sprtarg
      if (sprtarg > 0 & SPRratioLabel == "1-SPR") {
        abline(
          h = 1 - sprtarg,
          lty = 2,
          col = rgb(0, 0, 0, 0.4)
        )
      }

      # deal with warnings about zero-length arrow
      old_warn <- options()[["warn"]] # previous setting
      options(warn = -1) # turn off "zero-length arrow" warning
      arrows(
        x0 = phase_df[-nrow(phase_df), "Bratio"],
        y0 = phase_df[-nrow(phase_df), "SPRratio"],
        x1 = phase_df[-1, "Bratio"],
        y1 = phase_df[-1, "SPRratio"],
        length = 0.09,
        col = phase_df[["col"]][-1]
      )
      options(warn = old_warn) # returning to old value

      # add bigger points for first and final years
      points(
        x = phase_df[1, "Bratio"],
        y = phase_df[1, "SPRratio"],
        pch = 21,
        col = 1,
        bg = phase_df[1, "col"],
        cex = 1.2
      )
      points(
        x = phase_df[nrow(phase_df), "Bratio"],
        y = phase_df[nrow(phase_df), "SPRratio"],
        pch = 21,
        col = 1,
        bg = phase_df[nrow(phase_df), "col"],
        cex = 1.2
      )
    } # end make.phase.plot.MLE function

    if (4 %in% subplots) {
      if (plot) {
        make.phase.plot.MLE()
      }
      if (print) {
        file <- "SPR4_phase.png"
        caption <- paste0(
          "Phase plot of biomass ratio vs. SPR ratio.<br> ",
          "Each point represents the biomass ratio at the ",
          "start of the year and the relative fishing ",
          "intensity in that same year. ",
          "Warmer colors (red) represent early years and ",
          "colder colors (blue) represent recent years. "
        )
        if (
          isTRUE(
            Bratio_endyr_SD > 0 &
              SPRratio_endyr_SD > 0 &
              !is.null(B_SPR_endyr_corr)
          )
        ) {
          caption <- paste0(
            caption,
            "Lines through the final point show 95% intervals ",
            "based on the asymptotic uncertainty for each ",
            "dimension. The shaded ellipse is a 95% region ",
            "which accounts for the estimated correlation ",
            "between the two quantities: ",
            round(B_SPR_endyr_corr, 3),
            ". "
          )
        }
        # expand caption if ref point lines present
        if (replist[["Bratio_label"]] == "B/B_0" & btarg > 0) {
          caption <- paste0(
            caption,
            "The vertical line at ",
            btarg,
            " indicates the reference point as defined in the forecast.ss ",
            "which can be removed from the plot via ",
            "<code>SS_plots(..., btarg = -1)</code>. "
          )
        }
        # expand caption if SPR target line present
        if (sprtarg > 0 & SPRratioLabel == "1-SPR") {
          caption <- paste0(
            caption,
            "The horizontal line is at 1 - ",
            sprtarg_label,
            ": ",
            "1 - ",
            round(sprtarg, 3),
            " = ",
            round(1 - sprtarg, 3),
            ". "
          )
        }

        # save to png
        plotinfo <- save_png(
          plotinfo = plotinfo,
          file = file,
          plotdir = plotdir,
          pwidth = pwidth,
          pheight = pheight_tall,
          punits = punits,
          res = res,
          ptsize = ptsize,
          caption = caption
        )
        make.phase.plot.MLE()
        dev.off()
      } # end if print
    } # end if 4 %in% subplots
    if (!is.null(plotinfo)) {
      plotinfo[["category"]] <- "SPR"
    }
    return(invisible(plotinfo))
  }
