\name{SS_readforecast}
\alias{SS_readforecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  read forecast file
}
\description{
  read Stock Synthesis forecast file into list object in R
}
\usage{
SS_readforecast(file = "forecast.ss", Nfleets, Nareas, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
    Filename either with full path or relative to working directory.
  }
  \item{Nfleets}{
    Number of fleets.
  }
  \item{Nareas}{
    Number of areas.
  }
  \item{verbose}{
    Should there be verbose output while running the file?
  }
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
  Ian Taylor
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{SS_readstarter}},
 \code{\link{SS_readdat}}, \code{\link{SS_readctl}},
 \code{\link{SS_writestarter}}, \code{\link{SS_writeforecast}},
 \code{\link{SS_writedat}}, \code{\link{SS_writectl}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
