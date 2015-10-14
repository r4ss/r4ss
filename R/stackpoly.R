#' modified from "stackpoly" by Jim Lemon from "plotrix" package
#' 
#' Plot one or more columns of numeric values as the top edges of polygons
#' instead of lines.
#' 
#' 
#' @param x A numeric data frame or matrix with the 'x' values. If 'y' is NULL,
#' these will become the 'y' values and the 'x' positions will be the integers
#' from 1 to dim(x)[1].
#' @param y The 'y' values.
#' @param main The title for the plot.
#' @param xlab x axis labels for the plot.
#' @param ylab y axis labels for the plot.
#' @param xat Where to put the optional xaxlabs.
#' @param xaxlab Optional labels for the x positions.
#' @param xlim Optional x limits.
#' @param ylim Optional y limits.
#' @param lty Line type for the polygon borders.
#' @param border Color for the polygon borders.
#' @param col Color to fill the polygons. If NULL, 'rainbow' will be called to
#' generate the colors. If NA, the polygons will not be filled.
#' @param axis4 option to add an axis on the right hand side.
#' @param x.hash values from x for which the bars have hash marks instead of solid fill
#' @param density density value for hashed areas
#' @param \dots Additional arguments passed to 'plot'.
#' @author Jim Lemon, Ian Taylor
#' @export
#' @references \url{http://cran.r-project.org/web/packages/plotrix/index.html}
#' @keywords hplot
stackpoly <- function (x, y, main="", xlab="", ylab="", xat=NA,
                       xaxlab=NA, xlim=NA, ylim=NA, lty=1, border=NA,
                       col=NA, axis4=F, x.hash=NULL, density=20, ...)
## modified version of function "stackpoly" by Jim Lemon from "plotrix"
## see http://cran.r-project.org/web/packages/plotrix/index.html
{
    ydim <- dim(y)
    x <- matrix(rep(x, ydim[2]), ncol = ydim[2])
    y <- t(unlist(apply(as.matrix(y), 1, cumsum)))
    if (is.na(xlim[1])) xlim <- range(x)
    if (is.na(ylim[1])) ylim <- c(0,1.1*max(y))
    plot(0, main = main, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim,
         type = "n", xaxs = "i", yaxs = "i", axes = T,...)
    plotlim <- par("usr")
    if (is.na(col[1]))
        col = rainbow(ydim[2])
    else if (length(col) < ydim[2])
        col <- rep(col, length.out = ydim[2])
    if (length(lty) < ydim[2])
        lty <- rep(lty, length.out = ydim[2])
    for (pline in seq(ydim[2], 1, by = -1)) {
        if (pline == 1) {
          if(x[1]%in%x.hash){
            polygon(c(x[1], x[, pline], x[ydim[1]]),
                    c(plotlim[3], y[, pline], plotlim[3]),
                    border = border, col = col[pline],
                    lty = lty[pline],
                    density=density)
          }else{
            polygon(c(x[1], x[, pline], x[ydim[1]]),
                    c(plotlim[3], y[, pline], plotlim[3]),
                    border = border, col = col[pline],
                    lty = lty[pline])
          }
            
        } else {
          if(x[1,pline]%in%x.hash){
            polygon(c(x[, pline], rev(x[, pline - 1])),
                    c(y[, pline], rev(y[, pline - 1])), border = border,
                    col = col[pline], lty = lty[pline],
                    density=density)
          }else{
            polygon(c(x[, pline], rev(x[, pline - 1])),
                    c(y[, pline], rev(y[, pline - 1])), border = border,
                    col = col[pline], lty = lty[pline])
          }
          
        }
    }
    if (axis4)  axis(4)
}
## end stackpoly
