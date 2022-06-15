#' Plot the summary F (or harvest rate).
#'
#' Plots the summary F (or harvest rate) as set up in the starter file Needs a
#' lot of work to be generalized
#'
#' @template replist
#' @param yrs Which years to include.
#' @param Ftgt Target F where horizontal line is shown.
#' @param ylab Y-axis label.
#' @template plot
#' @template print
#' @template plotdir
#' @template verbose
#' @param uncertainty Show 95% uncertainty intervals around point estimates?
#' @param add add to existing plot
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @template mar
#' @author Allan Hicks
#' @export
#' @seealso [SSplotTimeseries()]
SSplotSummaryF <- function(replist, yrs = "all", Ftgt = NA, ylab = "Summary Fishing Mortality",
                           plot = TRUE, print = FALSE, plotdir = "default", verbose = TRUE,
                           uncertainty = TRUE,
                           add = FALSE,
                           pwidth = 6.5, pheight = 5.0, punits = "in", res = 300, ptsize = 10,
                           mar = NULL) {
  # plots the summary F (or harvest rate) as set up in the starter file
  # needs a lot of work to be generalized

  # table to store information on each plot
  plotinfo <- NULL

  # set default plot margins (repeated from SSplotTimeseries()
  if (is.null(mar)) {
    mar <- c(5, 4, 2, 2) + 0.1
  }

  if (plotdir == "default") {
    plotdir <- replist[["inputs"]][["dir"]]
  }

  if (yrs[1] == "all") {
    yrs <- replist[["startyr"]]:replist[["endyr"]]
  }
  Ftot <- replist[["derived_quants"]][match(
    paste("F_", yrs, sep = ""),
    replist[["derived_quants"]][["Label"]]
  ), ]
  if (all(is.na(Ftot[["Value"]]))) {
    warning(
      "Skipping SSplotSummaryF because no real values found in DERIVED_QUANTITIES\n",
      "    Values with labels like F_2012 may not be real.\n"
    )
    return()
  }
  Fmax <- max(c(Ftot[["Value"]], Ftgt + 0.01), na.rm = TRUE)
  if (uncertainty) {
    uppFtot <- Ftot[["Value"]] + 1.96 * Ftot[["StdDev"]]
    lowFtot <- Ftot[["Value"]] - 1.96 * Ftot[["StdDev"]]
    Fmax <- max(c(uppFtot, Ftgt + 0.01), na.rm = TRUE)
  }
  plotfun <- function() {
    if (!add) {
      plot(0,
        type = "n", , xlab = "Year", ylab = ylab, xlim = range(yrs), ylim = c(0, Fmax),
        cex.lab = 1.0, cex.axis = 1.0, cex = 0.7,
        mar = mar
      )
      abline(h = 0, col = "grey")
    }
    if (uncertainty) {
      segments(
        x0 = as.numeric(substring(Ftot[["Label"]], 3, 6)),
        y0 = uppFtot,
        x1 = as.numeric(substring(Ftot[["Label"]], 3, 6)),
        y1 = lowFtot,
        col = gray(0.5)
      )
    }
    points(as.numeric(substring(Ftot[["Label"]], 3, 6)),
      Ftot[["Value"]],
      pch = 16, type = "p"
    )
    abline(h = Ftgt, col = "red")
  }
  if (plot) {
    plotfun()
  }
  if (print) {
    caption <- "Summary F (definition of F depends on setting in starter.ss)"
    file <- "ts_summaryF.png"
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
  if (verbose) {
    message("Plotting Summary F\n")
  }
  return(invisible(plotinfo))
}
