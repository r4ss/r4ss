stackpoly <- function (x, y, main="", xlab="", ylab="", xat=NA,
                       xaxlab=NA, xlim=NA, ylim=NA, lty=1, border=NA,
                       col=NA, axis4=F, ...)
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
            polygon(c(x[1], x[, pline], x[ydim[1]]),
                    c(plotlim[3], y[, pline], plotlim[3]),
                    border = border, col = col[pline],
                    lty = lty[pline])
        }
        else polygon(c(x[, pline], rev(x[, pline - 1])),
                     c(y[, pline], rev(y[, pline - 1])), border = border,
                     col = col[pline], lty = lty[pline])
    }
    if (axis4)  axis(4)
}
## end stackpoly
