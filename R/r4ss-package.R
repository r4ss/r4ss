#' @import coda
#' @import dplyr
#' @importFrom corpcor pseudoinverse
#' @importFrom grDevices adjustcolor col2rgb colorRampPalette
#'   dev.new dev.off graphics.off
#'   gray grey pdf png rainbow rgb topo.colors
#' @importFrom graphics abline arrows axis barplot box
#'   contour curve grid hist image layout
#'   legend lines matplot mtext pairs par
#'   persp plot points polygon rect segments
#'   symbols text title
#' @importFrom kableExtra cell_spec kable kable_styling scroll_box
#' @importFrom stats acf aggregate density dnorm loess
#'   median na.omit nlminb optim pnorm qbeta qchisq qlnorm
#'   qnorm qt quantile rnorm sd var window
#' @importFrom utils browseURL flush.console head
#'   packageDescription read.csv
#'   read.table tail write.csv write.table
#'
#' @examples
#' \dontrun{
#' # it's useful to create a variable for the directory with the model output
#' mydir <- file.path(
#'   path.package("r4ss"),
#'   file.path("extdata", "simple_small")
#' )
#'
#' # read the model output and print diagnostic messages
#' replist <- SS_output(
#'   dir = mydir,
#'   verbose = TRUE,
#'   printstats = TRUE
#' )
#'
#' # plots the results
#' SS_plots(replist)
#' }
#'
#' @keywords internal
"_PACKAGE"
