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
Version: \tab 1.11\cr
Date: \tab 2010-09-08\cr
License: \tab MIT license\cr
LazyLoad: \tab yes\cr
URL: \tab http://www.r-project.org\cr \tab http://code.google.com/p/r4ss/\cr
}
For use with Stock Synthesis version 3.10b or later.
}
\author{
Ian Taylor, Ian Stewart, Tommy Garrison, Andre Punt, John Wallace,
and other contributors

Package maintainer: Ian Taylor <Ian.Taylor@noaa.gov>
}
\references{
  More info on the R code: http://code.google.com/p/r4ss/ \cr
  Download Stock Synthesis: http://nft.nefsc.noaa.gov/
}

\keyword{ package }
% \seealso{
% }
\examples{
\dontrun{
# read in the report file using SSoutput
myreplist <- SSoutput(dir='c:/SS/SSv3.10b/Simple/')
# make a collection of plots using SSplots
SSplots(replist=myreplist)
}
}
