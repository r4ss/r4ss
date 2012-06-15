plotCI <-
function (x, y = NULL, uiw, liw = uiw, ylo = NULL, yhi = NULL,
     ..., sfrac = 0.01, ymax = NULL, add = FALSE, col = "black") {
     # Written by Venables; modified for access to ylim, contents, and color
     if (is.list(x)) {
         y <- x$y
         x <- x$x
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
     ylim <- range(c(y, ui, li, ylo, yhi, ymax))
     if (!add)
         plot(x, y, ylim = ylim, col= col, ...)
     else points(x, y, col= col, ...)
     smidge <- diff(par("usr")[1:2]) * sfrac
     segments(x, li, x, ui, col= col)
     x2 <- c(x, x)
     ul <- c(li, ui)
     segments(x2 - smidge, ul, x2 + smidge, ul, col= col)
     invisible(list(x = x, y = y))
}
