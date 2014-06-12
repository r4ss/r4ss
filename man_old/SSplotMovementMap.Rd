\name{SSplotMovementMap}
\alias{SSplotMovementMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Show movement rates on a map.
}
\description{
  Make a map with colored spatial cells and add arrows representing
  movement rates between cells.
}
\usage{
SSplotMovementMap(replist=NULL, xlim, ylim,
           polygonlist, colvec, land="grey", xytable=NULL,
           moveage=5,moveseas=1,lwdscale=5,legend=TRUE,title=NULL,
           areanames=NULL,cex=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    optional list created by \code{\link{SS_output}}
  }
  \item{xlim}{
    range of longitude values in the map
  }
  \item{ylim}{
    range of latitude values in the map
  }
  \item{polygonlist}{
    a list of data frames, each with two columns representing the
    longitude and latitude values of the colored polygons. The order
    of elements in the list should match the numbering of areas in the
    SS model.
  }
  \item{colvec}{
    vector of colors for each polygon (if \code{replist} is provided)
  }
  \item{land}{
    color of landmasses in the map
  }
  \item{xytable}{
    data frame of latitude and longitude values which will be connected
    by the arrows representing movement rates. The order should match
    the order of areas in \code{polygonlist} and in the SS model. Not
    necessary if no arrows are shown on the map.
  }
  \item{moveage}{
    age for which movemement rates will be represented
  }
  \item{moveseas}{
    season for which movement rates will be represented
  }
  \item{lwdscale}{
    scaling factor for arrows in the plot. The largest rate of movement
    shown will be scaled to have a line width equal to this value.
  }
  \item{legend}{
    add a legend to show the movement rate associated with the widest
    arrows
  }
  \item{title}{
    optional title to be added above map
  }
  \item{areanames}{
    optional vector of names to be shown on map at coordinates matching
    xytable values
  }
  \item{cex}{
    character expansion to apply to text shown by areanames (if used)
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
\note{
  Inspired by plots of MULTIFAN-CL movement patterns presented by Adam
  Langley
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_output}},
  \code{\link{SSplotMovementRates}},
  \code{\link{IOTCmove}}
}
% \examples{
%   % \dontrun{
%   % }
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
