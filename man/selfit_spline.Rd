\name{selfit_spline}
\alias{selfit_spline}
\title{  visualize parameterization of cubic spline selectivity in SS }
\description{
  A GUI interface for exploring spline selectivity.
}
\usage{
selfit_spline(n = 4, minBin = 10, maxBin = 65, knots = NULL,
slopevec = c(0.01, -0.01), params = NULL, dir = getwd(), silent = FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ Number of knots. }
  \item{minBin}{ Minimum length or age to show. }
  \item{maxBin}{ Maximum length or age to show. }
  \item{knots}{ Vector giving location of each knot. }
  \item{slopevec}{ Optional initial values parameters controlling slope
    at first and last knot. }
  \item{params}{ Optional initial values for the parameters controlling
    selectivity at each knot. }
  \item{dir}{ Directory in which the spline_selex executable is located
    (default = working directory). }
  \item{silent}{ TRUE/FALSE switch to return fit at the end. }
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
\author{ Ian Taylor }
% \note{ ~~further notes~~ 

%  % ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{ \code{\link{selfit}} }
\examples{
\dontrun{
selfit_spline()
}
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ dplot }
\keyword{ hplot }
\keyword{ dynamic }
