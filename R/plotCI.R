#' Plot points with confidence intervals.
#'
#' Given a set of x and y values and upper and lower bounds, this function
#' plots the points with error bars. This was Written by Venables and modified
#' to add access to ylim and contents.
#'
#'
#' @param x The x coordinates of points in the plot
#' @param y The y coordinates of the points in the plot.
#' @param uiw The width of the upper portion of the confidence region.
#' @param liw The width of the lower portion of the confidence region.
#' @param ylo Lower limit of y range.
#' @param yhi Upper limit of y range.
#' @param \dots Additional inputs that will be passed to the function
#' `plot(x,y,ylim=ylim,...)`
#' @param sfrac Fraction of width of plot to be used for bar ends.
#' @param ymax Additional input for Upper limit of y range.
#' @param add Add points and intervals to existing plot? Default=FALSE.
#' @param col Color for the points and lines.
#' @param log Logarithmic scale for the y-axis? Should be "" or "y".
#' @author Bill Venables, Ian Stewart, Ian Taylor, John Wallace
plotCI <-
  function(x, y = NULL, uiw, liw = uiw, ylo = NULL, yhi = NULL,
           ..., sfrac = 0.01, ymax = NULL, add = FALSE, col = "black", log = "") {
    # Written by Venables; modified for access to ylim, contents, and color
    if (is.list(x)) {
      y <- x[["y"]]
      x <- x[["x"]]
    }
    if (is.null(y)) {
      if (is.null(x)) {
        stop("both x and y NULL")
      }
      y <- as.numeric(x)
      x <- seq(along = x)
    }
    ui <- y + uiw
    li <- y - liw
    ylim <- range(c(y, ui, li, ylo, yhi))
    # avoid -Inf ylim if log scale
    if (log == "y") {
      ylim[1] <- max(ylim[1], 0.0001)
    }
    if (!is.null(ymax)) ylim[2] <- ymax
    if (!add) plot(x, y, type = "n", ylim = ylim, col = col, log = log, ...)
    segments(x, li, x, ui, col = col)
    smidge <- diff(par("usr")[1:2]) * sfrac
    x2 <- c(x, x)
    ul <- c(li, ui)
    segments(x2 - smidge, ul, x2 + smidge, ul, col = col)
    points(x, y, col = col, ...)
    invisible(list(x = x, y = y))
  }
