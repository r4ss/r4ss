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
SSplotMovementRates(replist=NULL, colvec="default", ylim="default",
  legend=TRUE, moveseas=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{\link{SS_output}}
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
  \item{moveseas}{
    choice of season for which movemement rates are shown
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
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  \dontrun{
    SSplotMovementRates(myreplist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
