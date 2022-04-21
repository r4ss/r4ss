#' @docType package
#' @author Ian G. Taylor, Ian J. Stewart, Allan C. Hicks, Tommy M. Garrison,
#' Andre E. Punt, John R. Wallace, Chantel R. Wetzel, James T. Thorson,
#' Yukio Takeuchi, Kotaro Ono, Cole C. Monnahan, Christine C. Stawitz,
#' Z. Teresa A'mar, Athol R. Whitten, Kelli F. Johnson, Robbie L. Emmet,
#' Sean C. Anderson, Gwladys I. Lambert, Megan M. Stachura,
#' Andrew B. Cooper, Andi Stephens, Neil L. Klaer, Carey R. McGilliard,
#' Iago Mosqueira, Watal M. Iwasaki, Kathryn L. Doering, Andrea M. Havron,
#' Nathan Vaughan, LaTreese S. Denson, Ashleigh J. Novak, and Henning Winker
#'
#' Package maintainer: Ian G. Taylor <Ian.Taylor@@noaa.gov>
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
