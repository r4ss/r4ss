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
SSplotComparisons(summaryoutput,subplots=1:7,
           plot=TRUE,print=FALSE,
           models="all",
           labels=c("Year",             #1
             "Spawning biomass (mt)",   #2
             "Spawning depletion",      #3
             "Age-0 recruits (1,000s)", #4
             "Recruitment deviations",  #5
             "Index"),                  #6
           col="default", shadecol="default",
           pch="default", lty=1, lwd=2,
           xlim="default", xaxs='i', yaxs='r',
           type="o", uncertainty=TRUE, shadealpha=0.1,
           legend=TRUE, legendlabels="default", legendloc="topright",
           btarg=0.4, minbthresh=0.25,
           pwidth=7,pheight=7,punits="in",res=300,ptsize=12,cex.main=1,
           plotdir="workingdirectory",
           new=TRUE,
           verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{summaryoutput}{
    list created by \code{SSsummarize}
  }
  \item{subplots}{
    Vector of subplots to be created. Default=1:7.
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{models}{
    optional subset of the models described in \code{summaryoutput}.
    Either "all" or a vector of numbers indicating columns in summary
    tables.
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
    optional vector of labels to include in legend. Default is 'model1','model2',etc.
  }
  \item{legendloc}{
    location of legend. See ?legend for more info.
  }
  \item{btarg}{
    target biomass value at which to show a line (set to 0 to remove)
  }
  \item{minbthresh}{
    minimum biomass threshhold at which to show a line (set to 0 to remove)
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
    directory where PNG or PDF files will be written. by default it will
    be the directory where the model was run.
  }
  \item{new}{
    create new empty plot window (Windows only)
  }
  \item{verbose}{
    report progress to R GUI?
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
  \code{\link{SS_output}}, \code{\link{SSplotIndices}},
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
