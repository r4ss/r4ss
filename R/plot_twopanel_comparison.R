#' Plot a two-panel comparison using SSplotComparisons
#'
#' Plot a two-panel time series comparison of
#' spawning biomass and relative spawning biomass (or any other subplots,
#' like recruitment and recdevs).
#' By sharing the same horizontal axis label and requiring only a single caption,
#' this makes it easier to show two quantities on a single page of a report.
#'
#' @param mods A list of model objects as created by [SS_output()]
#' grouped together as a list(), or by [SSgetoutput()]
#' @param legendlabels Labels for use in the legend. NULL value will
#' use the lingcod model id
#' @param filename file to write to (relative to the "figures" directory)
#' NULL value will use a filename like `compare_[id1]_[id2].png`
#' @param dir Where to put the PNG file. If NULL then will send to the current graphics device.
#' @param subplot1 Which of the [SSplotComparisons()] plots to use
#' for the top panel. By default will be either spawning biomass without
#' or with uncertainty (1 or 2) depending on whether input `hessian` is TRUE or FALSE.
#' @param subplot2 Which of the [SSplotComparisons()] plots to use
#' for the bottom panel. By default will be either fraction unfished without
#' or with uncertainty (3 or 4) depending on whether input `hessian` is TRUE or FALSE.
#' @param hessian TRUE/FALSE whether to include hessian or not.
#' @param endyrvec final year to include in the figure passed to
#' [SSplotComparisons()]
#' @param ylimAdj1 adjustment to y-axis limit for the first plot,
#' relative to highest point among all models shown
#' @param ylimAdj2 adjustment to y-axis limit for the second plot,
#' relative to highest point among all models shown
#' @template verbose
#' @param dots additional arguments that will get passed to
#' [SSplotComparisons()]
#'
#' @export
#' @author Ian G. Taylor
#' @examples
#' \dontrun{
#' plot_twopanel_comparison(
#'   mods = list(
#'     model1,
#'     model2
#'   ),
#'   legendlabels = c("Model 1", "Model 2"),
#' )
#'
#' plot_twopanel_comparison(
#'   mods = list(
#'     model1,
#'     model2
#'   ),
#'   legendlabels = c("Model 1", "Model 2"),
#'   endyrvec = c(2025, 2023), # models might have different ending years
#'   subplot1 = 10, # recruitment
#'   subplot2 = 12 # recdevs
#' )
#' }
plot_twopanel_comparison <- function(
  mods,
  legendlabels = NULL,
  filename = NULL,
  dir = NULL,
  subplot1 = NULL,
  subplot2 = NULL,
  hessian = TRUE,
  endyrvec = 2023,
  ylimAdj1 = 1.05,
  ylimAdj2 = 1.05,
  verbose = TRUE,
  ...
) {
  summary <- SSsummarize(mods, verbose = FALSE)

  # default file name
  if (is.null(filename)) {
    filename <- paste0("compare_", subplot1, "_and_", subplot2, ".png")
  }

  if (!is.null(dir)) {
    if (verbose) {
      cli::cli_alert_info("printing figure to {file.path(dir, filename)}")
    }
    png(
      file.path(dir, filename),
      width = 6.5,
      height = 7.0,
      units = "in",
      pointsize = 10,
      res = 300
    )
  }
  if (is.null(subplot1)) {
    subplot1 <- ifelse(hessian, 2, 1)
  }
  if (is.null(subplot2)) {
    subplot2 <- ifelse(hessian, 4, 3)
  }
  par(mfrow = c(2, 1), mar = c(1, 5, 1, 1), oma = c(3, 1, 0, 0))
  SSplotComparisons(
    summary,
    endyrvec = endyrvec,
    subplots = subplot1,
    legendlabels = legendlabels,
    new = FALSE,
    ylimAdj = ylimAdj1,
    verbose = FALSE,
    ...
  )
  SSplotComparisons(
    summary,
    endyrvec = endyrvec,
    subplots = subplot2,
    legend = FALSE,
    new = FALSE,
    ylimAdj = ylimAdj2,
    verbose = FALSE,
    ...
  )
  mtext("Year", side = 1, line = 1, outer = TRUE)

  if (!is.null(dir)) {
    dev.off()
  }
}
