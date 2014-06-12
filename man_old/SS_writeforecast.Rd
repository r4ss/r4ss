\name{SS_writeforecast}
\alias{SS_writeforecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  write forecast file
}
\description{
  write Stock Synthesis forecast file from list object in R which was
  probably created using \code{\link{SS_readforecast}}
}
\usage{
SS_writeforecast(mylist, dir = NULL, file = "forecast.ss",
overwrite = FALSE, verbose = TRUE)
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
  \item{overwrite}{
    Should existing files be overwritten? Default=FALSE.
  }
  \item{verbose}{
    Should there be verbose output while running the file? Default=TRUE.
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
 \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
 \code{\link{SS_readdat}}, \code{\link{SS_readctl}},
 \code{\link{SS_writestarter}}, 
 \code{\link{SS_writedat}}, \code{\link{SS_writectl}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ manip }
