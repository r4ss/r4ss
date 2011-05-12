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
SSplotComparisons(summaryoutput,subplots=1:13,
  plot=TRUE,print=FALSE,
  models="all",
  endyrvec=NULL,
  indexfleets=NULL,
  indexUncertainty=FALSE,
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
  xlim="default", xaxs="r", yaxs="r",
  type="o", uncertainty=TRUE, shadealpha=0.1,
  legend=TRUE, legendlabels="default", legendloc="topright",
  btarg=0.4, minbthresh=0.25,
  pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
  plotdir=NULL,
  densitynames=c("SPB_Virgin",
                 "SPB_2011",
                 "Bratio_2011",
                 "SR_R0",
                 "SR_LN(R0)",
                 "TotYield_MSY"),
  densityxlabs=c("B0 (mt)",
                 "Spawning Biomass in 2011 (mt)",
                 "depletion in 2011",
                 "log(R0)",
                 "MSY (mt)"),
  densityscalex=1,
  densityscaley=1,
  fix0=TRUE,
  new=TRUE,
  verbose=TRUE,
  mcmcVec="default")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{summaryoutput}{
    list created by \code{SSsummarize}
  }
  \item{subplots}{
    Vector of subplots to be created.
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    send plots to PNG files in directory specified by \code{plotdir}
  }
  \item{models}{
    optional subset of the models described in \code{summaryoutput}.
    Either "all" or a vector of numbers indicating columns in summary
    tables.
  }
  \item{endyrvec}{
    optional single year or vector of years representing the final year
    of values to show for each model
  }
  \item{indexfleets}{
    vector of fleets for each model for which to compare indices of
    abundance. Only necessary if any model has more than one index.
  }
  \item{indexUncertainty}{
    show uncertainty intervals on index data? Default=FALSE because if
    models have any extra standard deviations added, these intervals may
    differ across models.
  }
  \item{indexSEvec}{
    optional replacement for the SE values in summaryoutput$indices to
    deal with the issue of differing uncertainty by models described
    above.
  } 
  \item{indexPlotEach}{
    TRUE plots the observed index for each model with colors,
    or FALSE just plots observed once in black dots.
  }
  \item{labels}{
    vector of labels for plots (titles and axis labels)
  }
  \item{col}{
    optional vector of colors to be used for lines. Input 'default'
    makes use of \code{rich.colors.short} function.
  }
  \item{shadecol}{
    optional vector of colors to be used for shading uncertainty
    intervals. Input 'default' makes use of \code{rich.colors.short}
    function with alpha transparency.
  }
  \item{pch}{
    optional vector of plot character values
  }
  \item{lty}{
    optional vector of line types
  }
  \item{lwd}{
    optional vector of line widths
  }
  \item{xlim}{
    optional x limits
  }
  \item{xaxs}{
    choice of xaxs parameter (see ?par for more info)
  }
  \item{yaxs}{
    choice of yaxs parameter (see ?par for more info)
  }
  \item{type}{
    type parameter passed to points (default 'o' overplots points on top
    of lines)
  }
  \item{uncertainty}{
    show plots with uncertainty intervals
  }
  \item{shadealpha}{
    transparency parameter used to make default shadecol values (see
    ?rgb for more info)
  }
  \item{legend}{
    add a legend?
  }
  \item{legendlabels}{
    optional vector of labels to include in legend. Default is
    'model1','model2',etc.
  }
  \item{legendloc}{
    location of legend. See ?legend for more info.
  }
  \item{btarg}{
    target biomass value at which to show a line (set to 0 to remove)
  }
  \item{minbthresh}{
    minimum biomass threshhold at which to show a line (set to 0 to
    remove)
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
    directory where PNG or PDF files will be written. By default it will
    be the directory where the model was run.
  }
  \item{densitynames}{
    vector of names of parameters or derived quantities contained in
    summaryoutput$pars$Label or summaryoutput$quants$Label for which to
    make density plots
  }
  \item{densityxlabs}{
    vector of x-axis labels to use in the density plots (must be equal
    in length to \code{densitynames})
  }
  \item{densityscalex}{
    scalar for upper x-limit in density plots (values below 1 will cut
    off the right tail to provide better contrast among narrower
    distributions
  }
  \item{densityscaley}{
    scalar for upper y-limit in density plots (values below 1 will cut
    off top of highest peaks to provide better contrast among broader
    distributions
  }
  \item{fix0}{
    always include 0 in the density plots?
  }
  \item{new}{
    create new empty plot window
  }
  \item{verbose}{
    report progress to R GUI?
  }
  \item{mcmcVec}{
    vector of TRUE/FALSE values (or single value) indicating whether
    input values are from MCMC or to use normal distribution around MLE
  }
  
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  Ian Taylor
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}, \code{\link{SSsummarize}},
  \code{\link{SS_output}}, \code{\link{SSgetoutput}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
