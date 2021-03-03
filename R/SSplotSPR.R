#' Plot SPR quantities.
#'
#' Plot SPR quantities, including 1-SPR and phase plot.
#'
#'
#' @template replist
#' @param add add to existing plot (not yet implemented)
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param uncertainty include plots showing uncertainty?
#' @param subplots vector controlling which subplots to create
#' @param forecastplot Include forecast years in plot?
#' @param col1 first color used
#' @param col2 second color used
#' @param col3 third color used
#' @param col4 fourth color used
#' @param sprtarg F/SPR proxy target. "default" chooses based on model output.
#' @param btarg target depletion to be used in plots showing depletion. May be
#' omitted by setting to NA. "default" chooses based on model output.
#' @param labels vector of labels for plots (titles and axis labels)
#' @param pwidth width of plot
#' @param pheight height of plot
#' @param punits units for PNG file
#' @template res
#' @param ptsize point size for PNG file
#' @param cex.main character expansion for plot titles
#' @param plotdir directory where PNG files will be written. by default it will
#' be the directory where the model was run.
#' @param verbose report progress to R GUI?
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso [SS_plots()], [SS_output()]
SSplotSPR <-
  function(replist, add = FALSE, plot = TRUE, print = FALSE,
           uncertainty = TRUE,
           subplots = 1:4, forecastplot = FALSE,
           col1 = "black", col2 = "blue", col3 = "green3", col4 = "red",
           sprtarg = "default", btarg = "default",
           labels = c(
             "Year", # 1
             "SPR", # 2
             "1-SPR", # 3
             "Relative fishing intensity", # 4
             "Relative spawning output" # 5
           ),
           pwidth = 6.5, pheight = 5.0, punits = "in", res = 300, ptsize = 10, cex.main = 1,
           plotdir = "default",
           verbose = TRUE) {
    # plot SPR-related quantities

    # subfunction to write png files
    pngfun <- function(file, caption = NA) {
      png(
        filename = file.path(plotdir, file),
        width = pwidth, height = pheight, units = punits, res = res, pointsize = ptsize
      )
      plotinfo <- rbind(plotinfo, data.frame(file = file, caption = caption))
      return(plotinfo)
    }
    plotinfo <- NULL

    if (plotdir == "default") {
      plotdir <- replist[["inputs"]][["dir"]]
    }

    sprseries <- replist[["sprseries"]]
    timeseries <- replist[["timeseries"]]
    derived_quants <- replist[["derived_quants"]]
    nsexes <- replist[["nsexes"]]
    nseasons <- replist[["nseasons"]]
    nareas <- replist[["nareas"]]
    endyr <- replist[["endyr"]]
    managementratiolabels <- replist[["managementratiolabels"]]

    # message about skipping plots
    if (is.null(sprseries)) {
      message("Skipping SPR plots: no output available")
      return()
    }

    if (sprtarg == "default") sprtarg <- replist[["sprtarg"]]
    if (btarg == "default") btarg <- replist[["btarg"]]

    # choose which points to plot
    good <- sprseries[["Yr"]] <= endyr
    if (forecastplot) good <- rep(TRUE, nrow(sprseries))

    spr_timeseries <- function() {
      if (!add) {
        plot(0,
          xlab = labels[1], ylab = labels[2], xlim = range(sprseries[["Yr"]][good]),
          ylim = c(0, max(1, max(sprseries[["spr"]][!is.na(sprseries[["spr"]])]))), type = "n"
        )
      }
      lines(sprseries[["Yr"]][good], sprseries[["spr"]][good], type = "o", col = col2)
      if (sprtarg > 0) abline(h = sprtarg, col = col4, lty = 2)
      abline(h = 0, col = "grey")
      abline(h = 1, col = "grey")
    }

    if (1 %in% subplots) {
      if (plot) spr_timeseries()
      if (print) {
        file <- "SPR1_series.png"
        caption <- "Timeseries of SPR"
        plotinfo <- pngfun(file = file, caption = caption)
        spr_timeseries()
        dev.off()
      }
    }

    # temporary disable multi-season models until code cleanup
    if (nseasons > 1) {
      message("Skipped 1-SPR plot because it's not yet configured for multi-season models\n")
    }
    if (nseasons == 1) {
      minus_spr_timeseries <- function() {
        if (!add) {
          plot(0,
            xlim = range(sprseries[["Yr"]][good]),
            xlab = labels[1], ylab = labels[3], ylim = c(0, 1), type = "n"
          )
        }
        lines(sprseries[["Yr"]][good], (1 - sprseries[["spr"]][good]), type = "o", col = col2)
        if (sprtarg > 0) abline(h = (1 - sprtarg), col = col4, lty = 2)
        abline(h = 0, col = "grey")
        abline(h = 1, col = "grey")
      }

      if (2 %in% subplots) {
        if (plot) minus_spr_timeseries()
        if (print) {
          file <- "SPR2_minusSPRseries.png"
          caption <- "Timeseries of 1-SPR"
          plotinfo <- pngfun(file = file, caption = caption)
          minus_spr_timeseries()
          dev.off()
        }
      }
    } # end check for nseasons == 1

    # get SPR ratio and uncertainty
    SPRratio <- derived_quants[grep(
      "^SPRratio_",
      derived_quants[["Label"]]
    ), ]
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

    # get B ratio and uncertainty
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
    B_SPR_endyr_corr <- replist$CoVar[
      replist$CoVar$label.i %in%
        paste0("Bratio_", endyr) &
        replist$CoVar$label.j %in%
          paste0("SPRratio_", endyr),
      "corr"
    ]

    # x-axis label for phase plot
    xlab <- paste0(labels[5], ": ", replist[["Bratio_label"]])
    # y-axis label and limits for time series and phase plot
    ylab <- paste0(labels[4], ": ", replist[["SPRratioLabel"]])
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
        plot(SPRratio[["Yr"]][SPRratio[["period"]] == "time"],
          SPRratio[["Value"]][SPRratio[["period"]] == "time"],
          xlab = labels[1], ylim = ylim, ylab = ylab, type = "n"
        )
      }
      lines(SPRratio[["Yr"]][SPRratio[["period"]] == "time"],
        SPRratio[["Value"]][SPRratio[["period"]] == "time"],
        type = "o", col = col2
      )
      abline(h = 0, col = "grey")
      abline(h = 1, col = col4)
      text((min(SPRratio[["Yr"]]) + 4), (1 + 0.02),
        "Management target",
        adj = 0
      )
      lines(SPRratio[["Yr"]][SPRratio[["period"]] == "time"],
        SPRratio[["upper"]][SPRratio[["period"]] == "time"],
        col = col2, lty = "dashed"
      )
      lines(SPRratio[["Yr"]][SPRratio[["period"]] == "time"],
        SPRratio[["lower"]][SPRratio[["period"]] == "time"],
        col = col2, lty = "dashed"
      )
    }
    if (3 %in% subplots) {
      if (plot) spr_ratio_timeseries()
      if (print) {
        file <- "SPR3_ratiointerval.png"
        caption <- paste(
          "Timeseries of SPR ratio:",
          replist[["SPRratioLabel"]]
        )
        plotinfo <- pngfun(file = file, caption = caption)
        spr_ratio_timeseries()
        dev.off()
      }
    }


    make.phase.plot.MLE <- function(x.max = 1.3,
                                    y.max = 1.3,
                                    period = "time") {
      # this function modified from make.phase.plot for Pacific Hake at
      # https://github.com/pacific-hake/hake-assessment/blob/master/R/figures-timeseries.R

      # Plots the relative fishing intensity in year t against
      # relative spawning biomass in year t

      # find years that are shared by both sets of outputs
      shared_yrs <- intersect(Bratio[["Yr"]][Bratio[["period"]] %in% period],
                              SPRratio[["Yr"]][SPRratio[["period"]] %in% period])
      Bratio_vals <- Bratio[["Value"]][Bratio[["Yr"]] %in% shared_yrs]
      SPRratio_vals <- SPRratio[["Value"]][SPRratio[["Yr"]] %in% shared_yrs]
      if(length(Bratio_vals) != length(SPRratio_vals)){
        message("Bratio and SPRratio vectors are different in length,",
                "skipping phase plot.")
        return()
      }
      plot(Bratio_vals,
        SPRratio_vals,
        type = "n",
        pch = 20,
        xlim = c(0, 1.1 * max(1, Bratio_vals)),
        ylim = c(0, 1.1 * max(1, SPRratio_vals[!is.na(SPRratio_vals)])),
        xlab = xlab,
        ylab = ylab,
        xaxs = "i",
        yaxs = "i"
      )
      colvec <- rev(rich.colors.short(n = length(Bratio_vals)))
      arrows(Bratio_vals[-length(Bratio_vals)],
        SPRratio_vals[-length(SPRratio_vals)],
        Bratio_vals[-1],
        SPRratio_vals[-1],
        length = 0.09,
        col = colvec
      )
      # add points for each year:
      points(Bratio_vals,
        SPRratio_vals,
        pch = 21,
        col = 1,
        bg = colvec
      )

      # add uncertainty intervals for final year:
      segments(
        x0 = Bratio[["Value"]][Bratio[["Yr"]] == endyr],
        y0 = SPRratio[["lower"]][SPRratio[["Yr"]] == endyr],
        x1 = Bratio[["Value"]][Bratio[["Yr"]] == endyr],
        y1 = SPRratio[["upper"]][SPRratio[["Yr"]] == endyr],
        col = rgb(0, 0, 0, 0.5)
      )
      segments(
        x0 = Bratio[["lower"]][Bratio[["Yr"]] == endyr],
        y0 = SPRratio[["Value"]][SPRratio[["Yr"]] == endyr],
        x1 = Bratio[["upper"]][Bratio[["Yr"]] == endyr],
        y1 = SPRratio[["Value"]][SPRratio[["Yr"]] == endyr],
        col = rgb(0, 0, 0, 0.5)
      )

      # get mean and variance-covariance matrix of bivariate normal
      # joint distribution based on normal approxmation from ADMB
      if (Bratio_endyr_SD > 0 &
        SPRratio_endyr_SD > 0 &
        !is.null(B_SPR_endyr_corr)) {
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
        e1 <- es$vec %*% diag(sqrt(es$val))
        r1 <- sqrt(qchisq(0.95, 2))
        theta <- seq(0, 2 * pi, len = 250)
        v1 <- cbind(r1 * cos(theta), r1 * sin(theta))
        pts <- t(mu - (e1 %*% t(v1)))

        # add polygon
        polygon(pts,
          col = gray(0, alpha = 0.2),
          border = NA
        )
      }
      # label first and final years:
      yr <- min(Bratio[["Yr"]][Bratio[["period"]] %in% period])
      text(
        x = Bratio_vals[1],
        y = SPRratio_vals[1],
        labels = yr,
        cex = 0.6,
        pos = 4,
        col = colvec[1]
      )
      yr <- max(Bratio[["Yr"]][Bratio[["period"]] %in% period])
      text(
        x = Bratio_vals[length(Bratio_vals)],
        y = SPRratio_vals[length(Bratio_vals)] + 0.015,
        labels = yr,
        pos = 2,
        cex = 0.6,
        col = colvec[length(colvec)]
      )

      # add lines at 1.0 in each dimension
      abline(
        h = 1,
        v = 1,
        lty = 2,
        col = rgb(0, 0, 0, 0.4)
      )
      # if denominator is B0, then add line at btarg
      if (replist[["Bratio_label"]] == "B/B_0" & replist[["btarg"]] > 0) {
        abline(
          v = replist[["btarg"]],
          lty = 2,
          col = rgb(0, 0, 0, 0.4)
        )
      }
      
      # add bigger points for first and final years
      points(Bratio_vals[1],
        SPRratio_vals[1],
        pch = 21,
        col = 1,
        bg = colvec[1],
        cex = 1.2
      )
      points(Bratio_vals[length(Bratio_vals)],
        SPRratio_vals[length(Bratio_vals)],
        pch = 21,
        col = 1,
        bg = colvec[length(Bratio_vals)],
        cex = 1.2
      )
    } # end make.phase.plot.MLE function

    if (4 %in% subplots) {
      if (btarg <= 0 | sprtarg <= 0) {
        message("skipped SPR phase plot because btarg or sprtarg <= 0")
      } else {
        timeseries[["Yr"]] <- timeseries[["Yr"]] + (timeseries[["Seas"]] - 1) / nseasons
        # !subsetting to season 1 only, initially just getting area 1
        ts <- timeseries[timeseries[["Seas"]] == 1 &
          timeseries[["Area"]] == 1 &
          timeseries[["Yr"]] <= endyr, ]
        # if there is more than 1 area, add them in now
        # this could be done using "aggregate" but this approach is more foolproof (hopefully)
        if (nareas > 1) {
          for (iarea in 2:nareas) {
            ts_area_i <- timeseries[timeseries[["Seas"]] == 1 &
              timeseries[["Area"]] == iarea &
              timeseries[["Yr"]] <= endyr, ]
            ts[["SpawnBio"]] <- ts[["SpawnBio"]] + ts_area_i[["SpawnBio"]]
          }
        }
        # divide spawning biomass by 2 for single-sex models
        if (nsexes == 1) {
          ts[["SpawnBio"]] <- ts[["SpawnBio"]] / 2
        }
        # calculate depletion
        depletionseries <- ts[["SpawnBio"]] / ts[["SpawnBio"]][1]
        # set axis limits
        # function to make the plot

        if (plot) make.phase.plot.MLE()
        if (print) {
          file <- "SPR4_phase.png"
          caption <- paste0(
            "Phase plot of biomass ratio vs. SPR ratio.<br> ",
            "Each point represents the biomass ratio at the ",
            "start of the year and the relative fishing ",
            "intensity in that same year. "
          )
          if (Bratio_endyr_SD > 0 &
            SPRratio_endyr_SD > 0 &
            !is.null(B_SPR_endyr_corr)) {
            caption <- paste0(
              caption,
              "Lines through the final point show 95% intervals ",
              "based on the asymptotic uncertainty for each ",
              "dimension. The shaded ellipse is a 95% region ",
              "which accounts for the estimated correlation ",
              "between the two quantities: ",
              round(B_SPR_endyr_corr, 3),
              "."
            )
          }
          plotinfo <- pngfun(file = file, caption = caption)
          make.phase.plot.MLE()
          dev.off()
        }
      } # end test for making phase plot
    } # end if 4 %in% subplots
    if (!is.null(plotinfo)) plotinfo[["category"]] <- "SPR"
    return(invisible(plotinfo))
  }
