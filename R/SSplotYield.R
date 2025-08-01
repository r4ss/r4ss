#' Plot yield and surplus production.
#'
#' Plot yield and surplus production from Stock Synthesis output. Surplus
#' production is based on Walters et al. (2008).
#'
#'
#' @template replist
#' @param subplots vector controlling which subplots to create
#' Numbering of subplots is as follows:
#' \itemize{
#'   \item 1 yield curve
#'   \item 2 yield curve with reference points
#'   \item 3 surplus production vs. total biomass plots (Walters et al. 2008)
#'   \item 4 surplus production vs. spawning biomass plots (Forrest et al. 2023)
#'   \item 5 yield per recruit plot
#' }
#' @param refpoints character vector of which reference points to display in
#' subplot 2, from the options 'MSY', 'Btgt', and 'SPR'.
#' @param add add to existing plot? (not yet implemented)
#' @template plot
#' @template print
#' @template labels
#' @param col line color for equilibrium plot
#' @param col2 line color for dynamic surplus production plot
#' @param lty line type (only applied to equilibrium yield plot at this time)
#' @template lwd
#' @template cex.main
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @template plotdir
#' @template verbose
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso [SS_plots()], [SS_output()]
#' @references Walters, Hilborn, and Christensen, 2008, Surplus production
#' dynamics in declining and recovering fish populations. *Can. J. Fish. Aquat.
#' Sci.* 65: 2536-2551. <https://doi.org/10.1139/F08-170>.
#' @references Forrest, Kronlund, Cleary, and Grinnell. 2023. An
#' evidence-based approach for selecting a limit reference point for Pacific
#' herring (*Clupea pallasii*) stocks in British Columbia, Canada. *Can. J. Fish.
#' Aquat. Sci.* 80: 1071-1083. <https://doi.org/10.1139/cjfas-2022-0168>.
SSplotYield <-
  function(
    replist,
    subplots = 1:5,
    refpoints = c("MSY", "Btgt", "SPR", "Current"),
    add = FALSE,
    plot = TRUE,
    print = FALSE,
    labels = c(
      "Fraction of unfished spawning biomass", # 1
      "Equilibrium yield (t)", # 2
      "Total biomass (t)", # 3
      "Surplus production (t)", # 4
      "Yield per recruit (kg)", # 5
      "Spawning output" # 6
    ),
    col = "blue",
    col2 = "black",
    lty = 1,
    lwd = 2,
    cex.main = 1,
    pwidth = 6.5,
    pheight = 5.0,
    punits = "in",
    res = 300,
    ptsize = 10,
    plotdir = "default",
    verbose = TRUE
  ) {
    # table to store information on each plot
    plotinfo <- NULL

    # update axis label based on spawning output units
    # check if spawning output rather than spawning biomass is plotted
    if (
      is.null(replist[["SpawnOutputUnits"]]) ||
        is.na(replist[["SpawnOutputUnits"]]) ||
        replist[["SpawnOutputUnits"]] == "numbers"
    ) {
      # quantity from test in SS_output
      labels[1] <- gsub("biomass", "output", labels[1])
    }

    # extract quantities from replist
    equil_yield <- replist[["equil_yield"]]
    # remove value associated with SPRloop 3 based on this comment in Report.sso:
    # "value 3 uses endyr F, which has different fleet allocation than benchmark"
    equil_yield <- equil_yield[equil_yield[["SPRloop"]] != 3, ]
    # sort across the various iterations by increasing values of the
    # "SSB/Bzero" column
    # previously this was done in SS_output()
    if ("SSB/Bzero" %in% names(equil_yield)) {
      equil_yield <- dplyr::arrange(equil_yield, .data[["SSB/Bzero"]])
    }
    # column named changed from Catch to Tot_Catch in SSv3.30
    if ("Tot_Catch" %in% names(equil_yield)) {
      equil_yield[["Catch"]] <- equil_yield[["Tot_Catch"]]
    }
    nareas <- replist[["nareas"]]
    nseasons <- replist[["nseasons"]]
    timeseries <- replist[["timeseries"]]
    SSB0 <- replist[["derived_quants"]]["SSB_Virgin", "Value"]

    # function for yield curve
    yieldfunc <- function(refpoints = NULL) {
      if (!add) {
        # empty plot
        plot(
          0,
          type = "n",
          xlim = c(0, max(equil_yield[["SSB/Bzero"]], 1, na.rm = TRUE)),
          ylim = c(0, max(equil_yield[["Catch"]], na.rm = TRUE)),
          xlab = labels[1],
          ylab = labels[2]
        )
        abline(h = 0, col = "grey")
        abline(v = 0, col = "grey")
      }

      # add lines for reference points (if requested)
      lines(
        equil_yield[["SSB/Bzero"]],
        equil_yield[["Catch"]],
        lwd = lwd,
        col = col,
        lty = lty
      )
      colvec <- c(4, 2, 3, 1)
      if ("MSY" %in% refpoints) {
        lines(
          x = rep(replist[["derived_quants"]]["SSB_MSY", "Value"] / SSB0, 2),
          y = c(0, replist[["derived_quants"]]["Dead_Catch_MSY", "Value"]),
          col = colvec[1],
          lwd = 2,
          lty = 2
        )
      }
      if ("Btgt" %in% refpoints) {
        lines(
          x = rep(replist[["derived_quants"]]["SSB_Btgt", "Value"] / SSB0, 2),
          y = c(0, replist[["derived_quants"]]["Dead_Catch_Btgt", "Value"]),
          col = colvec[2],
          lwd = 2,
          lty = 2
        )
      }
      if ("SPR" %in% refpoints) {
        lines(
          x = rep(replist[["derived_quants"]]["SSB_SPR", "Value"] / SSB0, 2),
          y = c(0, replist[["derived_quants"]]["Dead_Catch_SPR", "Value"]),
          col = colvec[3],
          lwd = 2,
          lty = 2
        )
      }
      if ("Current" %in% refpoints) {
        which_val <- which(
          abs(equil_yield[["SSB/Bzero"]] - replist[["current_depletion"]]) ==
            min(abs(
              equil_yield[["SSB/Bzero"]] - replist[["current_depletion"]]
            ))
        )[1]
        lines(
          x = rep(replist[["current_depletion"]], 2),
          y = c(0, equil_yield[["Catch"]][which_val]),
          col = colvec[4],
          lwd = 2,
          lty = 2
        )
      }
      # legend
      which_lines <- c(
        "MSY" %in% refpoints,
        "Btgt" %in% refpoints,
        "SPR" %in% refpoints,
        "Current" %in% refpoints
      )
      if (any(which_lines)) {
        legend(
          "topright",
          bty = "n",
          lwd = 2,
          lty = 2,
          col = colvec[which_lines],
          legend = refpoints # c("MSY", "B target", "SPR target", "Current")
        )
      }
    }

    if (1 %in% subplots | 2 %in% subplots) {
      # test if data is available
      if (!is.null(equil_yield[[1]][1]) && any(!is.na(equil_yield[[1]]))) {
        # further test for bad values
        # (not sure the circumstances where this is needed)
        if (
          any(!is.na(equil_yield[["SSB/Bzero"]])) &
            any(!is.na(equil_yield[["Catch"]])) &
            any(!is.infinite(equil_yield[["SSB/Bzero"]]))
        ) {
          if (1 %in% subplots) {
            # make plot
            if (plot) {
              yieldfunc()
            }
            if (print) {
              file <- "yield1_yield_curve.png"
              caption <- "Yield curve"
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
              yieldfunc()
              dev.off()
            }
          }
          if (2 %in% subplots & !is.null(refpoints)) {
            # make plot
            if (plot) {
              yieldfunc(refpoints = refpoints)
            }
            if (print) {
              file <- "yield2_yield_curve_with_refpoints.png"
              caption <- "Yield curve with reference points"
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
              yieldfunc(refpoints = refpoints)
              dev.off()
            }
          }
        } else {
          message(
            "Skipped equilibrium yield plots: equil_yield has all NA values"
          )
        }
      } else {
        message(
          "Skipped equilibrium yield plots: no equil_yield results in this model"
        )
      }
    } # end equilibrium yield plots

    # make dataframe summarizing key value by year (across seasons when present)
    df <- timeseries |>
      # timeseries excluding equilibrium conditions or forecasts
      dplyr::filter(!Era %in% c("VIRG", "FORE")) |>
      # add up dead fish from all fleets
      dplyr::mutate(
        catch_tot = rowSums(pick(starts_with("dead(B)")), na.rm = TRUE)
      ) |>
      # sum by areas
      dplyr::group_by(Yr, Seas) |>
      dplyr::summarise(
        sum_Bio_all = sum(Bio_all),
        sum_SpawnBio = sum(SpawnBio),
        sum_catch_tot = sum(catch_tot)
      ) |>
      dplyr::ungroup() |>
      # average across seasons
      dplyr::group_by(Yr) |>
      dplyr::summarise(
        mean_Bio_all = mean(sum_Bio_all),
        mean_SpawnBio = mean(sum_SpawnBio, na.rm = TRUE),
        catch_tot = sum(sum_catch_tot)
      )

    # number of years to consider
    Nyrs <- nrow(df)

    # calculate surplus production as difference in total biomass adjusted for catch
    df[["sprod"]] <- NA
    df[["sprod"]][1:(Nyrs - 1)] <-
      df[["mean_Bio_all"]][2:Nyrs] -
      df[["mean_Bio_all"]][1:(Nyrs - 1)] +
      df[["catch_tot"]][1:(Nyrs - 1)]

    # remove any rows with NA values
    df <- df |> dplyr::filter(!is.na(sprod))

    # function to plot surplus production
    sprodfunc <- function(bio_col, xlab) {
      x <- df[[bio_col]]
      y <- df[["sprod"]]
      xlim <- c(0, max(x, na.rm = TRUE))
      ylim <- c(min(0, y, na.rm = TRUE), max(y, na.rm = TRUE))
      # make empty plot
      if (!add) {
        plot(
          0,
          ylim = ylim,
          xlim = xlim,
          xlab = xlab,
          ylab = labels[4],
          type = "n"
        )
      }
      # add lines
      lines(x, y, col = col2)
      # make arrows
      old_warn <- options()[["warn"]] # previous setting
      options(warn = -1) # turn off "zero-length arrow" warning
      s <- seq(length(y) - 1)
      arrows(
        x[s],
        y[s],
        x[s + 1],
        y[s + 1],
        length = 0.06,
        angle = 20,
        col = col2,
        lwd = 1.2
      )
      options(warn = old_warn) # returning to old value

      # add lines at 0 and 0
      abline(h = 0, col = "grey")
      abline(v = 0, col = "grey")
      # add blue point at start
      points(x[1], y[1], col = col2, bg = "white", pch = 21)
    } # end sprodfunc

    # function to plot time series of Yield per recruit
    YPR_timeseries <- function() {
      # exclude forecast years
      if ("Era" %in% names(sprseries)) {
        sub <- sprseries[["Era"]] != "FORE"
      } else {
        # older versions of SS didn't include the Era column
        sub <- sprseries[["Yr"]] <= replist[["endyr"]]
      }
      # plot a line
      plot(
        x = sprseries[["Yr"]][sub],
        y = sprseries[["YPR"]][sub],
        ylim = c(0, 1.1 * max(sprseries[["YPR"]][sub], na.rm = TRUE)),
        xlab = "Year",
        ylab = labels[5],
        type = "l",
        lwd = 2,
        col = "blue",
        yaxs = "i"
      )
    } # end YPR_timeseries function

    if (3 %in% subplots) {
      if (plot) {
        sprodfunc(bio_col = "mean_Bio_all", xlab = labels[3])
      }
      if (print) {
        file <- "yield3_surplus_production.png"
        caption <-
          paste(
            "Surplus production vs. total biomass plot. For interpretation, see<br>",
            "<blockquote>Walters, Hilborn, and  Christensen, 2008,",
            "Surplus production dynamics in declining and",
            "recovering fish populations. <i>Can. J. Fish. Aquat. Sci.</i>",
            "65: 2536-2551. <a href='https://doi.org/10.1139/F08-170'>https://doi.org/10.1139/F08-170</a>.</blockquote>"
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
        sprodfunc(bio_col = "mean_Bio_all", xlab = labels[3])
        dev.off()
      }
    }

    if (4 %in% subplots) {
      if (plot) {
        sprodfunc(bio_col = "mean_SpawnBio", xlab = labels[6])
      }
      if (print) {
        file <- "yield4_surplus_production.png"
        caption <-
          paste(
            "Surplus production vs. spawning biomass plot. For interpretation, see<br>",
            "<blockquote>Forrest, Kronlund, Cleary, and Grinnell. 2023. An",
            "evidence-based approach for selecting a limit reference point for Pacific",
            "herring (<i>Clupea pallasii</i>) stocks in British Columbia, Canada. <i>Can. J. Fish.",
            "Aquat. Sci.</i> 80: 1071-1083.",
            "<a href='https://doi.org/10.1139/cjfas-2022-0168'>https://doi.org/10.1139/cjfas-2022-0168</a>.</blockquote>"
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
        sprodfunc(bio_col = "mean_SpawnBio", xlab = labels[6])
        dev.off()
      }
    }

    if (5 %in% subplots) {
      # stuff related to subplot 4 (YPR timeseries)
      sprseries <- replist[["sprseries"]]
      if (is.null(sprseries)) {
        if (verbose) {
          message(
            "Skipping yield per recruit plot because SPR_SERIES not in output"
          )
        }
      } else {
        if (plot) {
          YPR_timeseries()
        }
        if (print) {
          file <- "yield5_YPR_timeseries.png"
          caption <- "Time series of yield per recruit (kg)"
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
          YPR_timeseries()
          dev.off()
        }
      } # end check for sprseries available
    } # end check for 4 in subplots

    if (!is.null(plotinfo)) {
      plotinfo[["category"]] <- "Yield"
    }
    return(invisible(plotinfo))
  } # end function
