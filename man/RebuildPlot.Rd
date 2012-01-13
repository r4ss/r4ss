\name{DoProjectPlots}
\alias{DoProjectPlots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Make plots from Rebuilder program }
\description{
  Make a set of plots based on output from Andre Punt's Rebuilder program.
}
\usage{
DoProjectPlots(dirn="C:/myfiles/",fileN=c("res.csv"),Titles="",
  ncols=200,Plots=list(1:25),Options=list(c(1:9)),LegLoc="bottomright",
  yearmax=-1,Outlines=c(2,2),OutlineMulti=c(2,2),AllTraj=c(1,2,3,4),
  AllInd=c(1,2,3,4,5,6,7),BioType="Spawning biomass",CatchUnit="(mt)",
  BioUnit="(mt)",BioScalar=1,ColorsUsed="default",Labels="default",
  pdf=FALSE,pwidth=7,pheight=7,lwd=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dirn}{ Directory where rebuilder output files are stored. }
  \item{fileN}{ Vector of filenames containing rebuilder output. Default=c("res.csv"). }
  \item{Titles}{ Titles for plots when using multiple filenames. Default="". }
  \item{ncols}{ Number of columns to read in output file (fileN). Deafult=200. }
  \item{Plots}{ List to get specific plots (currently 1 through 8).
    Default=list(1:25). If there are multiple files, supply a list of vectors, e.g. list(c(1,5),c(2:5)) }
  \item{Options}{ List to get specific strategies in the trajectory
    plots. Default=list(c(1:9)).If there are multiple files, supply a list of vectors, e.g. list(c(1,5),c(2:5)) }
  \item{LegLoc}{ Location for the legend (for plots with a legend).
    Default="bottomright". }
  \item{yearmax}{ Maximum year to show in the plots. Set negative to show all years.
    Default=-1. }
  \item{Outlines}{ Number of rows, columns for some of the plots. Default=c(2,2).}
  \item{OutlineMulti}{ Number of rows, columns for other plots.
    Default=c(2,2).}
  \item{AllTraj}{ Vector of trajectories to show. Default=c(1,2,3,4). }
  \item{AllInd}{ Vector of individual plots to show. Default=c(1,2,3,4,5,6,7). }
  \item{BioType}{ Label for biomass type. Default="Spawning biomass". }
  \item{CatchUnit}{ Units of catch. Default="(mt)". }
  \item{BioUnit}{ Units of biomass. Default="(mt)". }
  \item{BioScalar}{ Scalar for biomass plot. Default=1. }
  \item{ColorsUsed}{ Optional vector for alternative line colors. Default="default". }
  \item{Labels}{ Optional vector for alternative legend labels. Default="default". }
  \item{pdf}{ Option to send figures to pdf file instead of plot window
    in Rgui. Default=FALSE. }
  \item{pwidth}{ Width of the plot window or PDF file (in inches). Default=7. }
  \item{pheight}{ Height of the plot window or PDF file (in inches). Default=7. }
  \item{lwd}{ Line width for many of the plot elements. Default=2. }
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
 DoProjectPlots(dirn="c:/myfiles/", Plots=1:8,
                Options=c(1,2,3,4,5,9), LegLoc="bottomleft")

# example with multiple files
 # Plots - set to get specific plots
 # Options - set to get specific strategies in the trajectory plots

 Titles <- c("Res1","Res2","Res3")
 Plots <- list(c(1:9),c(6:7))
 Options <- list(c(7:9,3),c(5,7))
 DoProjectPlots(fileN=c("res1.csv","res2.csv"),Titles=Titles,Plots=Plots,
                Options=Options,LegLoc="bottomleft",yearmax=-1,
                Outlines=c(2,2),OutlineMulti=c(3,3),AllTraj=c(1:4),
                AllInd=c(1:7),BioType="Spawning numbers",BioUnit="(lb)",
                BioScalar=1000,CatchUnit="(lb)",
                ColorsUse=rep(c("red","blue"),5),
                Labels=c("A","B","C","D","E","F"))
}
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ dplot }
\keyword{ hplot }

