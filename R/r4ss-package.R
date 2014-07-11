#' R tools for Stock Synthesis
#' 
#' A collection of R functions for use with Stock Synthesis, a fisheries stock
#' assessment modeling platform written in ADMB by Dr. Richard D.  Methot at
#' the NMFS Northwest Fisheries Science Center. The functions include tools for
#' summarizing and plotting results, manipulating files, visualizing model
#' parameterizations, and various other tasks.
#' 
#' \tabular{ll}{ Package: \tab r4ss\cr Type: \tab Package\cr Version: \tab
#' 1.22\cr Date: \tab 2014-07-08\cr License: \tab GPL-3\cr LazyLoad: \tab
#' yes\cr URL: \tab \url{https://github.com/r4ss/}\cr }
#' Should be compatible with Stock Synthesis versions 3.20 through 3.3.
#' 
#' @name r4ss-package
#' @aliases r4ss-package r4ss
#' @docType package
#' @author Ian Taylor, Ian Stewart, Allan Hicks, Tommy Garrison, Andre Punt,
#' John Wallace, Chantel Wetzel, James Thorson, Yukio Takeuchi, Kotaro Ono, 
#' and other contributors
#' Package maintainer: Ian Taylor <Ian.Taylor@@noaa.gov>
#' @references r4ss on GitHub: \url{https://github.com/r4ss}
#' \cr Download Stock Synthesis: \url{http://nft.nefsc.noaa.gov/}
#' @keywords package
#' @import tcltk coda maps
#' @importFrom corpcor pseudoinverse
#' @importFrom gplots hist2d
#' @importFrom gtools running
#' @importFrom pso psoptim
#' @importFrom RCurl getURL
#' @examples
#' 
#' \dontrun{
#' # read in the report file using SS_output
#' myreplist <- SS_output(dir='c:/SS/simple/')
#' 
#' # make a collection of plots using SS_plots
#' SS_plots(replist=myreplist)
#' }
#' 
NULL



