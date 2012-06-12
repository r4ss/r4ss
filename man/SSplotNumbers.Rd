\name{SSplotNumbers}
\alias{SSplotNumbers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot numbers-at-age related data and fits.
}
\description{
  Plot numbers-at-age related data and fits from Stock Synthesis output.
  Plots include bubble plots, mean age, equilibrium age composition,
  sex-ratio, and ageing imprecision patterns.
}
\usage{
SSplotNumbers(replist, subplots = 1:9, plot = TRUE, print = FALSE,
areas = "all", areanames = "default", areacols = "default",
pntscalar = 2.6, period = c("B","M"), add = FALSE,
labels=c("Year",                    
  "Age",                            
  "True age (yr)",                  
  "SD of observed age (yr)",        
  "Mean observed age (yr)",         
  "Mean age (yr)",                  
  "mean age in the population",     
  "Ageing imprecision",             
  "Numbers at age at equilibrium",  
  "Equilibrium age distribution",   
  "Sex ratio of numbers at age (males/females)",
  "Length",                         
  "Mean length (cm)",               
  "mean length (cm) in the population",
  "expected numbers at age",       
  "Beginning of year",             
  "Middle of year",                
  "expected numbers at length",    
  "Sex ratio of numbers at length (males/females)",
  "Sex ratio of numbers at length (females/males)"),
pwidth = 7, pheight = 7, punits = "in", res = 300,
ptsize = 12, cex.main = 1, plotdir = "default", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{SSoutput}
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
  \item{areas}{
    optional subset of areas to plot for spatial models
  }
  \item{areanames}{
    names for areas. Default is to use Area1, Area2,...
  }
  \item{areacols}{
    vector of colors by area
  }
  \item{pntscalar}{
    maximum bubble size for bubble plots; each plot scaled independently
    based on this maximum size and the values plotted. Often some plots
    look better with one value and others with a larger or smaller
    value. Default=2.6
  }
  \item{period}{
    indicator of whether to make plots using numbers at age just from
    the beginning ("B") or middle of the year ("M") (new option starting
    with SSv3.11)
  }
  \item{add}{
    add to existing plot? (not yet implemented)
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
  \item{plotdir}{
    directory where PNG files will be written. by default it will
    be the directory where the model was run.
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
  \code{\link{SS_output}},
  \code{\link{SS_plots}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
