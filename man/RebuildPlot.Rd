\name{DoProjectPlots}
\alias{DoProjectPlots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Make plots from Rebuilder program }
\description{
  Make a set of plots based on output from Andre Punt's Rebuilder program.
}
\usage{
DoProjectPlots(dirn="C:/NWFSC/NWFSC25D/",fileN=c("res.csv"),Titles="",
               ncols=200,Plots=list(1:25),Options=list(c(1:9)),
               LegLoc="bottomright",yearmax= -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dirn}{ Directory where rebuilder output is stored. }
  \item{fileN}{ Filename containing rebuilder output. Default="res.csv". }
  \item{Titles}{ Titles for plots when using multiple filenames.
    Default="". }
  \item{ncols}{ Number of columns to read in output file (fileN). Deafult=200. }
  \item{Plots}{ List containing indices of plots to be created
    (currently 1 through 8). Default=list(1:25). }
  \item{Options}{ Vector to get specific strategies in the trajectory
    plots. Default=list(1:9). }
  \item{LegLoc}{ Location for the legend (for plots with a legend).
    Default="bottomright". }
  \item{yearmax}{ Maximum year to show in the plots. Negative value =
    show all years. Default=-1. }
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
# example with one file
DoProjectPlots(dirn="c:/SS/mymodel/", Plots=1:8,
               Options=c(1,2,3,4,5,9), LegLoc="bottomleft")

# example with multiple files
 # Plots - set to get specific plots
 # Options - set to get specific strategies in the trajectory plots

 Titles <- c("Res1","Res2","Res3")
 Plots <- list(c(1:9),c(6:7))
 Options <- list(c(7:9,3),c(5))
 DoProjectPlots(fileN=c("res1.csv","res2.csv"),Titles=Titles,
                Plots=Plots,Options=Options,LegLoc="bottomleft",yearmax=-1)

}
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ dplot }
\keyword{ hplot }

