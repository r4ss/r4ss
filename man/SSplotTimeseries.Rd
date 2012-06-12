\name{SSplotTimeseries}
\alias{SSplotTimeseries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot timeseries data
}
\description{
Plot timeseries data contained in TIME_SERIES output from Stock
Synthesis report file. Some values have optional uncertainty intervals.
}
\usage{
SSplotTimeseries(replist, subplot, add = FALSE, areas = "all",
areacols = "default", areanames = "default", forecastplot = TRUE,
uncertainty = TRUE, bioscale = "default", minyr=NULL, maxyr=NULL,
plot = TRUE, print = FALSE, plotdir = "default", verbose = TRUE,
btarg="default", minbthresh="default", xlab = "Year",
           labels=c("Total biomass (mt)", 
             "Total biomass (mt) at beginning of season", 
             "Summary biomass (mt)",      
             "Summary biomass (mt) at beginning of season", 
             "Spawning biomass (mt)",     
             "Spawning depletion",        
             "Spawning output (eggs)",    
             "Age-0 recruits (1,000s)",  
             "Fraction of total Age-0 recruits"),  
pwidth = 7, pheight = 7, punits = "in", res = 300, ptsize = 12,
cex.main = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{SS_output}
  }
  \item{subplot}{
    number controlling which subplot to create
  }
  \item{add}{
    add to existing plot? (not yet implemented)
  }
  \item{areas}{
    optional subset of areas to plot for spatial models
  }
  \item{areacols}{
    vector of colors by area. Default uses rich.colors by Arni Magnusson
  }
  \item{areanames}{
    names for areas. Default is to use Area1, Area2,...
  }
  \item{forecastplot}{
    add points from forecast years
  }
  \item{uncertainty}{
    add intervals around quantities for which uncertainty is available
  }
  \item{bioscale}{
    scaling for spawning biomass by default it will be set to 0.5 for
    single-sex models, and 1.0 for all others
  }
  \item{minyr}{
    optional input for minimum year to show in plots
  }
  \item{maxyr}{
    optional input for maximum year to show in plots
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{plotdir}{
    directory where PNG or PDF files will be written. by default it will
    be the directory where the model was run.
  }
  \item{verbose}{
    report progress to R GUI?
  }
  \item{btarg}{
    Target depletion to be used in plots showing
    depletion. May be omitted by setting to 0. "default" chooses value
    based on modeloutput.}
  \item{minbthresh}{
    Threshold depletion to be used in plots showing
    depletion. May be omitted by setting to 0. "default" assumes 0.25
    unless btarg in model output is 0.25 in which case minbthresh =
    0.125 (U.S. west coast flatfish).
  }
  \item{xlab}{
    x axis label for all plots
  }
  \item{labels}{
    vector of labels for plots (titles and axis labels)
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
  Ian Taylor, Ian Stewart
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
