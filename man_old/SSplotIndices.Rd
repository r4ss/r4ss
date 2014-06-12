\name{SSplotIndices}
\alias{SSplotIndices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot indices of abundance and associated quantities.
}
\description{
  Plot indices of abundance and associated quantities.
}
\usage{
SSplotIndices(replist,subplots=1:9,
  plot=TRUE,print=FALSE,
  fleets="all",fleetnames="default",
  smooth=TRUE,add=FALSE,datplot=FALSE,
  labels=c("Year",         
    "Index",               
    "Observed index",      
    "Expected index",      
    "Log index",           
    "Log observed index",  
    "Log expected index",  
    "Standardized index",  
    "Catchability (Q)",    
    "Time-varying catchability",
    "Vulnerable biomass",
    "Catchability vs. vulnerable biomass"),
  col1="default", col2="default", col3="blue", col4="red",
  pch1=1, pch2=16, cex=1,
  legend=TRUE, legendloc="topright", seasnames=NULL,
  pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
  addmain=TRUE, plotdir="default",
  minyr = NULL, maxyr = NULL, verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{SS_output}
  }
  \item{subplots}{
    vector controlling which subplots to create
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{fleets}{
    optional vector to subset fleets for which plots will be made
  }
  \item{fleetnames}{
    optional replacement for fleenames used in data file
  }
  \item{smooth}{
    add smoothed line to plots of observed vs. expected sample sizes
  }
  \item{add}{
    add to existing plot (not yet implemented)
  }
  \item{datplot}{
    make plot of data only?
  }
  \item{labels}{
    vector of labels for plots (titles and axis labels)
  }
  \item{col1}{
    vector of colors for points in each season for time series plot.
    Default is red for single season models and a rainbow using the
    rich.colors.short function for multiple seasons.
  }
  \item{col2}{
    vector of colors for points in each season for obs. vs. exp. plot.
    Default is blue for single season models and a rainbow using the
    rich.colors.short function for multiple seasons.
  }
  \item{col3}{
    color of line showing expected index in time series plot. Default is
    blue.
  }
  \item{col4}{
    color of smoother shown in obs. vs. exp. plots. Default is red.
  }
  \item{pch1}{
    single value or vector of plotting characters (pch parameter) for
    time-series plots of index fit. Default=1.
  }
  \item{pch2}{
    single value or vector of plotting characters (pch parameter) for
    sample size plots of index fit. Default=16.
  }
  \item{cex}{
    character expansion factor for points showing observed values. Default=1.
  }
  \item{legend}{
    add a legend to seasonal colors (only for seasonal models)
  }
  \item{legendloc}{
    add a legend to seasonal colors (default is "topright")
  }
  \item{seasnames}{
    optional vector of names for each season to replace defaults if a
    legend is used
  }
  \item{pwidth}{
    width of plot written to PNG file
  }
  \item{pheight}{
    height of plot written to PNG file
  }
  \item{punits}{
    units for PNG file
  }
  \item{res}{
    resolution for PNG file
  }
  \item{ptsize}{
    ptsize for PNG file
  }
  \item{cex.main}{
    character expansion for plot titles
  }
  \item{addmain}{
    switch which allows the plot title to be left off
  }
  \item{plotdir}{
    directory where PNG files will be written. by default it will
    be the directory where the model was run.
  }
  \item{minyr}{
    First year to show in plot (for zooming in on a subset of values)
  }
  \item{maxyr}{
    Last year to show in plot (for zooming in on a subset of values)
  }
  \item{verbose}{
    report progress to R GUI?
  }
}
% \details{
%   %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
%   %%  ~Describe the value returned
%   %%  If it is a LIST, use
%   %%  \item{comp1 }{Description of 'comp1'}
%   %%  \item{comp2 }{Description of 'comp2'}
%   %% ...
% }
% \references{
%   %% ~put references to the literature/web site here ~
% }
\author{
  Ian Stewart, Ian Taylor
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}, \code{\link{SS_output}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
