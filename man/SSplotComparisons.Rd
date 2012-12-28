\name{SSplotComparisons}
\alias{SSplotComparisons}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot model comparisons
}
\description{
  Creates a user-chosen set of plots comparing model output from a
  summary of multiple models, where the collection was created using the
  \code{SSsummarize} function.
}
\usage{
SSplotComparisons(summaryoutput,subplots=1:14,
  plot=TRUE,print=FALSE,
  models="all",
  endyrvec=NULL,
  indexfleets=NULL,
  indexUncertainty=FALSE,
  indexQlabel = TRUE,
  indexQdigits = 4, 
  indexSEvec="default",
  indexPlotEach=FALSE,         
  labels=c("Year",             
    "Spawning biomass (mt)",   
    "Spawning depletion",      
    "Age-0 recruits (1,000s)", 
    "Recruitment deviations",  
    "Index",                   
    "Log index",               
    "Density"),                
  col="default", shadecol="default",
  pch="default", lty=1, lwd=2,
  spacepoints = 10, staggerpoints = 1, 
  xlim="default", xaxs="r", yaxs="r",
  type="o", uncertainty=TRUE, shadealpha=0.1,
  legend=TRUE, legendlabels="default", legendloc="topright",
  btarg=0.4, minbthresh=0.25,
  pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
  plotdir=NULL,
  densitynames=c("SPB_Virgin",
                 "R0"),
  densityxlabs="default",
  densityscalex=1,
  densityscaley=1,
  densityadjust=1,
  fix0=TRUE,
  new=TRUE,
  add=FALSE,
  verbose=TRUE,
  mcmcVec="default")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{summaryoutput}{
    List created by \code{SSsummarize}
  }
  \item{subplots}{
    Vector of subplots to be created.
  }
  \item{plot}{
    Plot to active plot device?
  }
  \item{print}{
    Send plots to PNG files in directory specified by \code{plotdir}
  }
  \item{models}{
    Optional subset of the models described in \code{summaryoutput}.
    Either "all" or a vector of numbers indicating columns in summary
    tables.
  }
  \item{endyrvec}{
    Optional single year or vector of years representing the final year
    of values to show for each model
  }
  \item{indexfleets}{
    Vector of fleets for each model for which to compare indices of
    abundance. Only necessary if any model has more than one index.
  }
  \item{indexUncertainty}{
    Show uncertainty intervals on index data? Default=FALSE because if
    models have any extra standard deviations added, these intervals may
    differ across models.
  }
  \item{indexQlabel}{
    Add catchability to legend in plot of index fits (TRUE/FALSE)?
  }
  \item{indexQdigits}{
    Number of significant digits for catchability in legend
    (if \code{indexQlabel=TRUE})
  }
  \item{indexSEvec}{
    Optional replacement for the SE values in summaryoutput$indices to
    deal with the issue of differing uncertainty by models described
    above.
  } 
  \item{indexPlotEach}{
    TRUE plots the observed index for each model with colors,
    or FALSE just plots observed once in black dots.
  }
  \item{labels}{
    Vector of labels for plots (titles and axis labels)
  }
  \item{col}{
    Optional vector of colors to be used for lines. Input 'default'
    makes use of \code{rich.colors.short} function.
  }
  \item{shadecol}{
    Optional vector of colors to be used for shading uncertainty
    intervals. Input 'default' makes use of \code{rich.colors.short}
    function with alpha transparency.
  }
  \item{pch}{
    Optional vector of plot character values
  }
  \item{lty}{
    Optional vector of line types
  }
  \item{lwd}{
    Optional vector of line widths
  }
  \item{spacepoints}{
    Number of years between points shown on top of lines (for long
    timeseries, points every year get mashed together)
  }
  \item{staggerpoints}{
    Number of years to stagger the first point
    (if \code{spacepoints > 1}) for each line
    (so that adjacent lines have points in different years)
  }
  \item{xlim}{
    Optional x limits
  }
  \item{xaxs}{
    Choice of xaxs parameter (see ?par for more info)
  }
  \item{yaxs}{
    Choice of yaxs parameter (see ?par for more info)
  }
  \item{type}{
    Type parameter passed to points (default 'o' overplots points on top
    of lines)
  }
  \item{uncertainty}{
    Show plots with uncertainty intervals
  }
  \item{shadealpha}{
    Transparency parameter used to make default shadecol values (see
    ?rgb for more info)
  }
  \item{legend}{
    Add a legend?
  }
  \item{legendlabels}{
    Optional vector of labels to include in legend. Default is
    'model1','model2',etc.
  }
  \item{legendloc}{
    Location of legend. See ?legend for more info.
  }
  \item{btarg}{
    Target biomass value at which to show a line (set to 0 to remove)
  }
  \item{minbthresh}{
    Minimum biomass threshhold at which to show a line (set to 0 to
    remove)
  }
  \item{pwidth}{
    Width of plot written to PNG file
  }
  \item{pheight}{
    Height of plot written to PNG file
  }
  \item{punits}{
    Units for PNG file
  }
  \item{res}{
    Resolution for PNG file
  }
  \item{ptsize}{
    Point size for PNG file
  }
  \item{cex.main}{
    Character expansion for plot titles
  }
  \item{plotdir}{
    Directory where PNG or PDF files will be written. By default it will
    be the directory where the model was run.
  }
  \item{densitynames}{
    Vector of names (or subset of names) of parameters or derived
    quantities contained in summaryoutput$pars$Label or
    summaryoutput$quants$Label for which to make density plots
  }
  \item{densityxlabs}{
    Optional vector of x-axis labels to use in the density plots (must
    be equal in length to the printed vector of quantities that match
    the \code{densitynames} input)
  }
  \item{densityscalex}{
    Scalar for upper x-limit in density plots (values below 1 will cut
    off the right tail to provide better contrast among narrower
    distributions
  }
  \item{densityscaley}{
    Scalar for upper y-limit in density plots (values below 1 will cut
    off top of highest peaks to provide better contrast among broader
    distributions
  }
  \item{densityadjust}{
    Multiplier on bandwidth of kernel in density function used for
    smoothing MCMC posteriors. See 'adjust' in ?density for details.
  }
  \item{fix0}{
    Always include 0 in the density plots?
  }
  \item{new}{
    Create new empty plot window 
  }
  \item{new}{
    Allows single plot to be added to existing figure. This needs to be
    combined with specific 'subplots' input to make sure only one thing
    gets added.
  }
  \item{new}{
    Create new empty plot window
  }
  \item{verbose}{
    Report progress to R GUI?
  }
  \item{mcmcVec}{
    Vector of TRUE/FALSE values (or single value) indicating whether
    input values are from MCMC or to use normal distribution around MLE
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
  Ian Taylor
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}, \code{\link{SSsummarize}},
  \code{\link{SS_output}}, \code{\link{SSgetoutput}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
