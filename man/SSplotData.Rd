\name{SSplotData}
\alias{SSplotData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Timeline of presence/absence of data by type, year, and fleet.
}
\description{
  Plot shows graphical display of what data is being used in the model.
  Some data types may not yet be included. Note, this is based on output
  from the model, not the input data file.
}
\usage{
SSplotData(replist, fleetcol = "default", datatypes = "all", fleets =
"all", ghost = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{\link{SS_output}}
  }
  \item{fleetcol}{
    Either the string "default", or a vector of colors
    to use for each fleet.
  }
  \item{datatypes}{
    Either the string "all", or a vector including some subset of the
    following: "catch", "cpue", "lendbase", "sizedbase", "agedbase",
    "condbase", "ghostagedbase", "ghostcondbase", "ghostlendbase",
    "ladbase", "wadbase", "tagdbase1", "tagdbase2".
  }
  \item{fleets}{
    Either the string "all", or a vector of numerical values, like
    c(1,3), listing fleets or surveys to be included in the plot.
  }
  \item{ghost}{
    TRUE/FALSE indicator for whether to show presence of composition
    data from ghost fleets (data for which the fit is shown, but is not
    included in the likelihood calculations).
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
  Ian Taylor, Chantel Wetzel
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}, \code{\link{SS_output}}, \code{\link{SS_readdat}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
