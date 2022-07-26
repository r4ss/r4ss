#' @import coda
#' @import kableExtra
#' @importFrom corpcor pseudoinverse
#' @importFrom grDevices adjustcolor col2rgb colorRampPalette
#'   dev.new dev.off graphics.off
#'   gray grey pdf png rainbow rgb topo.colors
#' @importFrom graphics abline arrows axis barplot box
#'   contour curve grid hist image layout
#'   legend lines matplot mtext pairs par
#'   persp plot points polygon rect segments
#'   symbols text title
#' @importFrom stats acf aggregate density dnorm loess
#'   median na.omit nlminb optim pnorm qbeta qchisq qlnorm
#'   qnorm qt quantile rnorm sd var window
#' @importFrom utils browseURL flush.console head
#'   packageDescription read.csv
#'   read.table tail write.csv write.table
#'
#' @examples
#' \dontrun{
#' # read in the report file using SS_output
#' myreplist <- SS_output(dir = "c:/SS/simple/")
#'
#' # make a collection of plots using SS_plots
#' SS_plots(replist = myreplist)
#' }
#'
#' @keywords internal
"_PACKAGE"
