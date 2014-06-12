\name{selfit}
\alias{selfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  A function to visual parameterization of double normal and
  double logistic selectivity in Stock Synthesis }
\description{
  A GUI interface for exploring selectivity.
}
\usage{
selfit(minLength = 10, maxLength = 65, silent = FALSE, init = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{minLength}{ Minimum size to show }
  \item{maxLength}{ Maximum size to show }
  \item{silent}{ T/F switch to return fit at the end }
  \item{init}{ Optional initial values for the parameters }
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }
% \references{ ~put references to the literature/web site here ~ }
\author{ Tommy Garrison }
% \note{ ~~further notes~~ 

%  % ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{ \code{\link{sel.line}} }
\examples{
\dontrun{
selfit()
}
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ dplot }
\keyword{ hplot }
\keyword{ dynamic }
