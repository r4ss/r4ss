\name{SS_writeforecast}
\alias{SS_writeforecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  write Stock Synthesis forecast file from list object in R
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SS_writeforecast(mylist, dir = NULL, file = "forecast.ss",
nareas = 1, nfleets = 1, overwrite = FALSE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mylist}{
    List object created by \code{\link{SS_readforecast}}.
  }
  \item{dir}{
    Directory for new forecast file. Default=NULL (working directory).
  }
  \item{file}{
    Filename for new forecast file. Default="forecast.ss".
  }
  \item{nareas}{
    Number of areas in the model (necessary to get formatting correct).
  }
  \item{nfleets}{
    Number of fleets in the model (necessary to get formatting correct).
  }
  \item{overwrite}{
    Should existing files be overwritten? Default=FALSE.
  }
  \item{verbose}{
    Should there be verbose output while running the file? Default=TRUE.
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
