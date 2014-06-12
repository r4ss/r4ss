\name{sel.line}
\alias{sel.line}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ a function for drawing selecitivity curves }
\description{
  This function is primarily inteded for use by the selfit function.
}
\usage{
sel.line(x, model, sp, min.dist, max.dist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ vector of x values (age or length) }
  \item{model}{ selectivity model "Double_Normal" or "Double_Logistic" }
  \item{sp}{ vector of parameters }
  \item{min.dist}{ minimum value for selectivity }
  \item{max.dist}{  maximum value for selectivity  }
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
% \references{  }
\author{ Tommy Garrison }
% \note{ ~~further notes~~ 

%  % ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{ \code{\link{selfit}} }
\examples{
\dontrun{
plot(0, xlim = c(0, 50), ylim = c(0, 1),
xlab = 'Length', ylab = 'Selectivity', type = 'n',
xaxs = 'i', yaxs = 'i') 
sel.line(model = 'Double_Normal', min.dist = 10, max.dist = 50,
sp = c(25, -0.5, 3, 3, -5, 0))
}
}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ dplot }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
