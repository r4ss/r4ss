\name{SSplotMovementRates}
\alias{SSplotMovementRates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Show movement rates on a map.
}
\description{
  Make a map with colored spatial cells and add arrows representing
  movement rates between cells.
}
\usage{
SSplotMovementRates(replist, plot=TRUE, print=FALSE, subplots=1:2,
           plotdir="default",
           colvec="default", ylim="default", 
           legend=TRUE, legendloc="topleft",
           moveseas="all",
           pwidth=7,pheight=7,punits="in",res=300,
           ptsize=12,cex.main=1,
           verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{\link{SS_output}}
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{subplots}{
    which subplots to create
  }
  \item{plotdir}{
    where to put the plots (uses model directory by default)
  }
  \item{colvec}{
    vector of colors for each movement rate in the plot
  }
  \item{ylim}{
    optional input for y range of the plot. By default plot ranges from
    0 to 10\% above highest movement rate (not including fish staying in
    an area).
  }
  \item{legend}{
    add a legend designating which color goes with which pair of areas?
  }
  \item{legendloc}{
    location passed to legend function (if used)
  }
  \item{moveseas}{
    choice of season for which movemement rates are shown
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
    Character expansion parameter for plot titles
  }
  \item{verbose}{
    Print information on function progress.
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
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_output}},
  \code{\link{SSplotMovementRates}},
  \code{\link{IOTCmove}}
}
\examples{
  \dontrun{
    SSplotMovementRates(myreplist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
