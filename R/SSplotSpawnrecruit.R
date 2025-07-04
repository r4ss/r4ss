#' Plot spawner-recruit curve.
#'
#' Plot spawner-recruit curve based on output from Stock Synthesis model.
#'
#'
#' @template replist
#' @param subplots Vector of which subplots to show.  1=plot without labels,
#' 2=plot with year labels.
#' @param subplot Deprecated - use subplots.
#' @param add add to existing plot?
#' @template plot
#' @template print
#' @param xlim optional control of x range
#' @param ylim optional control of y range
#' @param labels vector containing x-axis label for models with spawning biomass
#' in metric tons, y-axis label, and alternative x-axis for models with a fecundity
#' relationship making spawning output not equal to spawning biomass.
#' @template bioscale
#' @template plotdir
#' @template pwidth
#' @template pheight
#' @template punits
#' @template ptsize
#' @template res
#' @template verbose
#' @param colvec vector of length 4 with colors for 3 lines and 1 set of points
#' (where the 4th value for the points is the color of the circle around the
#' background color provided by `ptcol`
#' @param ltyvec vector of length 4 with line types for the 3 lines and 1 set
#' of points, where the points are disconnected (lty=NA) by default
#' @param ptcol vector or single value for the color of the points, "default"
#' will by replaced by a vector of colors of length equal to
#' `nrow(replist[["recruit"]])`
#' @template legend
#' @template legendloc
#' @param minyr minimum year of recruitment deviation to show in plot
#' @param textmindev minimum recruitment deviation for label to be added so
#' only extreme devs are labeled (labels are added to first and last years as
#' well).  Default=0.7.
#' @param relative scale both axes so that B0 and R0 are at 1
#' to show spawning output and recruitment relative to the equilibrium
#' @param expected show line for expected recruitment (stock-recruit curve)
#' @param estimated show points for estimated recruitment values
#' (including deviations)
#' @param bias_adjusted show lines for bias adjusted expected recruitment
#' @param show_env add line for expected recruitment with environmental
#' variability
#' @param virg add point for equilibrium conditions (x=B0,y=R0)
#' @param init add point for initial conditions (x=B1,y=R1), only appears
#' if this point differs from virgin values
#' @param forecast include forecast years in the curve?
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso [SS_plots()], [SS_output()]
SSplotSpawnrecruit <-
  function(
    replist,
    subplots = 1:3,
    add = FALSE,
    plot = TRUE,
    print = FALSE,
    xlim = NULL,
    ylim = NULL,
    labels = c(
      "Spawning biomass (t)",
      "Recruitment (1,000s)",
      replist[["SpawnOutputLabel"]],
      expression(paste("Spawning output (relative to ", italic(B)[0], ")")),
      expression(paste("Recruitment (relative to  ", italic(R)[0], ")")),
      "Log recruitment deviation"
    ),
    bioscale = 1,
    plotdir = "default",
    pwidth = 6.5,
    pheight = 6.5,
    punits = "in",
    res = 300,
    ptsize = 10,
    verbose = TRUE,
    colvec = c("blue", "black", "black", gray(0, 0.7)),
    ltyvec = c(1, 2, 1, NA),
    ptcol = "default",
    legend = TRUE,
    legendloc = NULL,
    # line1="blue",line2="green3",line3="black",ptcol="red",
    minyr = "default",
    textmindev = 0.5,
    relative = FALSE,
    expected = TRUE,
    estimated = TRUE,
    bias_adjusted = TRUE,
    show_env = TRUE,
    virg = TRUE,
    init = TRUE,
    forecast = FALSE,
    subplot = lifecycle::deprecated()
  ) {
    # warn about soft deprecated arguments
    if (lifecycle::is_present(subplot)) {
      lifecycle::deprecate_warn(
        when = "1.45.1",
        what = "SSplotSpawnrecruit(subplot)",
        with = "SSplotSpawnrecruit(subplots)"
      )
      subplots <- subplot
    }

    # plot of spawner recruit curve

    # table to store information on each plot
    plotinfo <- NULL

    recruit <- replist[["recruit"]]
    if (is.null(recruit)) {
      message(
        "Skipping stock-recruit plots: no recruitment information available"
      )
      return()
    } else {
      if (3 %in% subplots && max(abs(recruit[["dev"]]), na.rm = TRUE) < 1e-6) {
        subplots <- setdiff(subplots, 3)
      }
    }
    nsexes <- replist[["nsexes"]]

    # set axis labels
    xlab <- labels[1]
    ylab <- labels[2]
    # check if spawning output rather than spawning biomass is plotted
    if (
      is.null(replist[["SpawnOutputUnits"]]) ||
        is.na(replist[["SpawnOutputUnits"]]) ||
        replist[["SpawnOutputUnits"]] == "numbers"
    ) {
      # quantity from test in SS_output
      xlab <- labels[3]
    }

    if (relative) {
      xlab <- labels[4]
      ylab <- labels[5]
    }

    if (plotdir == "default") {
      plotdir <- replist[["inputs"]][["dir"]]
    }
    if (minyr == "default") {
      minyr <- min(recruit[["Yr"]])
    }

    recruit <- recruit[
      recruit[["era"]] %in%
        c(
          "Early",
          "Main",
          "Fixed",
          "Late",
          ifelse(forecast, "Forecast", NA)
        ) &
        recruit[["Yr"]] >= minyr,
    ]

    timeseries <- replist[["timeseries"]]
    recruit[["spawn_bio"]] <- bioscale * recruit[["SpawnBio"]]
    timeseries[["SpawnBio"]] <- bioscale * timeseries[["SpawnBio"]]

    # x and y limits
    if (is.null(ylim)) {
      ylim <- c(
        0,
        1.1 *
          max(
            recruit[["pred_recr"]],
            recruit[["exp_recr"]],
            recruit[["bias_adjusted"]]
          )
      )
    }
    x <- recruit[["spawn_bio"]]
    if (is.null(xlim)) {
      xlim <- c(0, 1.1 * max(x))
    }

    # only add lines for environmentally dependent recruitment if it differs
    # from expected recruitment without environmental link
    show_env <- show_env & any(recruit[["with_env"]] != recruit[["exp_recr"]])

    # store virgin and initial values
    B0 <- sum(
      timeseries[["SpawnBio"]][timeseries[["Era"]] == "VIRG"],
      na.rm = TRUE
    )
    B1 <- sum(
      timeseries[["SpawnBio"]][timeseries[["Era"]] == "INIT"],
      na.rm = TRUE
    )
    R0 <- sum(
      timeseries[["Recruit_0"]][timeseries[["Era"]] == "VIRG"],
      na.rm = TRUE
    )
    R1 <- sum(
      timeseries[["Recruit_0"]][timeseries[["Era"]] == "INIT"],
      na.rm = TRUE
    )

    # work around for issue with Shepherd function producing 0 values in equilibrium
    # use first non-zero value for each
    if (B0 == 0) {
      B0 <- head(recruit[["spawn_bio"]][recruit[["spawn_bio"]] != 0], 1)
    }
    if (R0 == 0) {
      R0 <- head(recruit[["exp_recr"]][recruit[["exp_recr"]] != 0], 1)
    }
    if (B0 == B1 & R0 == R1) {
      init <- FALSE
    }

    # scaling factor for axes (relative to B0/R0 or absolute)
    if (relative) {
      x.mult <- 1 / B0
      y.mult <- 1 / R0
    } else {
      x.mult <- 1
      y.mult <- 1
    }

    # color for points
    if (ptcol[1] == "default") {
      ptcol <- rev(rich.colors.short(nrow(recruit) + 10, alpha = 0.8))[-(1:10)]
      color.caption <- paste(
        " Point colors indicate year, with warmer",
        "colors indicating earlier years and cooler colors",
        "in showing later years."
      )
    } else {
      color.caption <- ""
    }
    # prepare for legend
    if (legend) {
      legend_entries <- c(
        expected, # expected
        show_env, # with environmental link
        bias_adjusted, # bias adjusted
        estimated, # estimated points
        virg, # virgin
        init
      ) # initial equilibrium
      legend_col <- colvec[c(3, 1, 2, 4, 3, 3)][legend_entries]
      legend_bg <- c(NA, NA, NA, tail(ptcol)[1], NA, NA)[legend_entries]
      legend_lwd <- c(2, 1, 1, NA, NA, NA)[legend_entries]
      legend_lty <- ltyvec[c(3, 1, 2, 4, 3, 3)][legend_entries]
      legend_pch <- c(NA, NA, NA, 21, 3, 4)[legend_entries]
      legend_cex <- c(1, 1, 1, 1, 1.5, 1.5)[legend_entries]
      legend_lab <- c(
        "Exp. recruitment",
        "Exp. recruitment with env. link",
        "Exp. recruitment after bias adj.",
        "Estimated recruitments",
        "Unfished equilibrium",
        "Initial equilibrium"
      )[legend_entries]
    }
    StockRecruitCurve.fn <- function(text = FALSE) {
      ### a function to make the plots
      if (!add) {
        par(mar = c(4.5, 4.5, 1, 1))
        # make empty plot (if not adding to existing plot)
        plot(
          0,
          type = "n",
          xlim = xlim * x.mult,
          ylim = ylim * y.mult,
          xaxs = "i",
          yaxs = "i",
          xlab = xlab,
          ylab = ylab
        )
      }
      if (show_env) {
        # add line for expected recruitment with environmental variability
        lines(
          x[order(x)] * x.mult,
          recruit[["with_env"]][order(x)] * y.mult,
          lwd = 1,
          lty = ltyvec[1],
          col = colvec[1]
        )
      }
      if (expected) {
        # add line for expected recruitment
        lines(
          x[order(x)] * x.mult,
          recruit[["exp_recr"]][order(x)] * y.mult,
          lwd = 2,
          lty = ltyvec[3],
          col = colvec[3]
        )
      }
      if (bias_adjusted) {
        # add line for adjusted recruitment
        lines(
          x * x.mult,
          recruit[["bias_adjusted"]] * y.mult,
          lwd = 1,
          lty = ltyvec[2],
          col = colvec[2]
        )
      }
      if (estimated) {
        # add points for individual estimates
        points(
          x * x.mult,
          recruit[["pred_recr"]] * y.mult,
          pch = 21,
          col = colvec[4],
          bg = ptcol
        )
      }
      if (text) {
        # add text, but only label values with larger devs (in abs value)
        show <- abs(recruit[["dev"]]) > textmindev
        show[1] <- show[length(show)] <- TRUE # also include first & last years
        text(
          x[show] * x.mult,
          recruit[["pred_recr"]][show] * y.mult,
          labels = recruit[["Yr"]][show],
          pos = 2,
          cex = .7
        )
      }
      # add point for virgin biomass/recruitment (if requested)
      if (virg) {
        points(B0 * x.mult, R0 * y.mult, pch = 3, cex = 1.5)
      }
      # add point for initial biomass/recruitment (if requested)
      if (init) {
        points(B1 * x.mult, R1 * y.mult, pch = 4, cex = 1.5)
      }
      # add legend
      if (legend) {
        # add sub-function:
        legend.overlap <- function(x, y, ...) {
          # function to figure out if a legend with inputs given by ...
          # overlaps any of the points with coordinates x and y

          # run legend without plotting
          legend.out <- legend(..., plot = FALSE)
          # get coordinates of legend boundaries
          leg.left <- legend.out[["rect"]][["left"]]
          leg.right <- legend.out[["rect"]][["left"]] +
            legend.out[["rect"]][["w"]]
          leg.top <- legend.out[["rect"]][["top"]]
          leg.bottom <- legend.out[["rect"]][["top"]] -
            legend.out[["rect"]][["h"]]
          # test for overlap
          if (
            any(x >= leg.left & x <= leg.right & y >= leg.bottom & y <= leg.top)
          ) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
        # indicator if legend has been added successfully
        legend_added <- FALSE
        # if not ready to plot, look for best location
        if (is.null(legendloc)) {
          for (legendloc in c("topleft", "topright", "bottomright")) {
            has_overlap <- legend.overlap(
              x = x * x.mult,
              y = recruit[["pred_recr"]] * y.mult,
              legendloc,
              legend = legend_lab,
              col = legend_col,
              pt.bg = legend_bg,
              lwd = legend_lwd,
              lty = legend_lty,
              pch = legend_pch,
              bg = rgb(1, 1, 1, .6)
            )
            if (!has_overlap & !legend_added) {
              legend(
                legendloc,
                legend = legend_lab,
                col = legend_col,
                pt.bg = legend_bg,
                lwd = legend_lwd,
                lty = legend_lty,
                pch = legend_pch,
                bg = rgb(1, 1, 1, .6)
              )
              legend_added <- TRUE
            }
          } # end loop over legendloc values
          if (!legend_added) {
            warning(
              "Legend in spawner-recruit plot overlaps at least 1 point\n",
              "in the plot. Try running SSplotSpawnrecruit() with\n",
              "adjustments to 'ylim' and/or 'legendloc' inputs."
            )
            legendloc <- "topleft"
          }
        }
        # add legend at user-requested location or topleft with warning
        if (!legend_added) {
          legend(
            legendloc,
            legend = legend_lab,
            col = legend_col,
            pt.bg = legend_bg,
            lwd = legend_lwd,
            lty = legend_lty,
            pch = legend_pch,
            bg = rgb(1, 1, 1, .5)
          )
        }
      } # end commands to add legend
    } # end StockRecruitCurve.fn

    stock_vs_devs.fn <- function(text = FALSE) {
      ### a function to make the plots
      if (!add) {
        par(mar = c(4.5, 4.5, 1, 1))
        # maximum spawning output relative to unfished equilibrium (usually 1)
        xmax <- 1.05 * max(x / B0)
        # make empty plot (if not adding to existing plot)
        plot(
          0,
          type = "n",
          xlim = c(0, xmax),
          ylim = c(-1.1, 1.1) * max(abs(recruit[["dev"]]), na.rm = TRUE),
          las = 1,
          xaxs = "i",
          yaxs = "i",
          xlab = labels[4],
          ylab = labels[6]
        )
      }
      abline(h = 0, col = "grey")
      points(
        x / B0,
        recruit[["dev"]],
        pch = 21,
        bg = ptcol,
        col = colvec[4],
        cex = 1.5
      )
      if (text) {
        # add text, but only label values with larger devs (in abs value)
        show <- abs(recruit[["dev"]]) > textmindev
        show[1] <- show[length(show)] <- TRUE # also include first & last years
        text(
          x[show] / B0,
          recruit[["dev"]][show],
          labels = recruit[["Yr"]][show],
          pos = 2,
          cex = .7
        )
      }
      # add point for virgin biomass/recruitment (if requested)
      if (virg) {
        points(1, 0, pch = 3, cex = 2, lwd = 2)
      }
      # add point for initial biomass/recruitment (if requested)
      if (init) {
        points(B1 / B0, 0, pch = 4, cex = 2)
      }
      ## # add legend
      ## if(legend){
      ##   legend(legendloc, legend=legend_lab, col=legend_col, lwd=legend_lwd,
      ##          pch=legend_pch, bg=rgb(1,1,1,.9))
      ## }
    }
    if (plot) {
      if (1 %in% subplots) {
        StockRecruitCurve.fn()
      }
      if (2 %in% subplots) {
        StockRecruitCurve.fn(text = TRUE)
      }
      if (3 %in% subplots) {
        stock_vs_devs.fn(text = TRUE)
      }
    }
    if (print) {
      if (1 %in% subplots) {
        file <- "SR_curve.png"
        caption <- paste("Stock-recruit curve.", color.caption)
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
        StockRecruitCurve.fn()
        dev.off()
      }
      if (2 %in% subplots) {
        file <- "SR_curve2.png"
        caption <- paste0(
          "Stock-recruit curve with labels on first, last, and ",
          "years with (log) deviations > ",
          textmindev,
          ".",
          color.caption
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
        StockRecruitCurve.fn(text = TRUE)
        dev.off()
      }
      if (3 %in% subplots) {
        file <- "SR_resids.png"
        caption <- paste0(
          "Deviations around the stock-recruit curve. ",
          "Labels are on first, last, and ",
          "years with (log) deviations > ",
          textmindev,
          ".",
          color.caption
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
        stock_vs_devs.fn(text = TRUE)
        dev.off()
      }
    }
    if (!is.null(plotinfo)) {
      plotinfo[["category"]] <- "S-R"
    }
    return(invisible(plotinfo))
  }
