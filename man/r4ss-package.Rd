\name{r4ss-package}
\alias{r4ss-package}
\alias{r4ss}
\docType{package}
\title{
  R tools for Stock Synthesis
}
\description{
  A collection of R functions for use with Stock Synthesis, a fisheries
  stock assessment modeling platform written in ADMB by Dr. Richard D.
  Methot at the NMFS Northwest Fisheries Science Center. The functions
  include tools for summarizing and plotting results, manipulating files,
  visualizing model parameterizations, and various other tasks.
}
\details{
  \tabular{ll}{
    Package: \tab r4ss\cr
    Type: \tab Package\cr
    Version: \tab 1.19\cr
    Date: \tab 2012-10-25\cr
    License: \tab GPL-3\cr
    LazyLoad: \tab yes\cr
    URL: \tab \url{http://www.r-project.org}\cr \tab \url{http://code.google.com/p/r4ss/}\cr
  }
  Should be compatible with Stock Synthesis versions 3.11 through 3.24.
}
\author{
  Ian Taylor, Ian Stewart, Allan Hicks, Tommy Garrison, Andre Punt,
  John Wallace, Chantel Wetzel, and other contributors

  Package maintainer: Ian Taylor <Ian.Taylor@noaa.gov>
}
\references{
  More info on the R code: \url{http://code.google.com/p/r4ss/} \cr
  Download Stock Synthesis: \url{http://nft.nefsc.noaa.gov/}
}

\keyword{ package }
% \seealso{
% }
\examples{
\dontrun{
# source updated files from the web (not always needed, but often helps)
update_r4ss_files()

# read in the report file using SS_output
myreplist <- SS_output(dir='c:/SS/simple/')

# make a collection of plots using SS_plots
SS_plots(replist=myreplist)
}
}
