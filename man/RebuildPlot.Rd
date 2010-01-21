\name{DoProjectPlots}
\alias{DoProjectPlots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Make plots from Rebuilder program }
\description{
  Make a set of plots based on output from Andre Punt's Rebuilder program.
}
\usage{
DoProjectPlots(dirn="C:/NWFSC/NWFSC25D/", fileN="res.csv", ncols=200,
Plots=1:25, Options=1:9, LegLoc="bottomright")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dirn}{ Directory where rebuilder output is stored. }
  \item{fileN}{ Filename containing rebuilder output. Default="res.csv". }
  \item{ncols}{ Number of columns to read in output file (fileN). Deafult=200. }
  \item{Plots}{ Vector containing indices of plots to be created
    (currently 1 through 8). Default=1:25. }
  \item{Options}{ Vector to get specific strategies in the trajectory
    plots. Default=1:9. }
  \item{LegLoc}{ Location for the legend (for plots with a legend).
    Default="bottomright". }
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }
% \references{ ~put references to the literature/web site here ~ }
\author{ Andre Punt }
% \note{ ~~further notes~~ 

%  % ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ }
\examples{
\dontrun{
DoProjectPlots(dirn="c:/SS/mymodel/", Plots=1:8,
               Options=c(1,2,3,4,5,9), LegLoc="bottomleft") }
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ dplot }
\keyword{ hplot }

