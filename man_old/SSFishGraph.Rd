\name{SSFishGraph}
\alias{SSFishGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A function for converting Stock Synthesis output to the format
  used by FishGraph }
\description{
  Only skeleton of a function right now, needs work. Intended as a
  translator to convert the output from object created by
  \code{\link{SS_output}} to the format used by FishGraph.
}
\usage{
SSFishGraph(replist, title = "SSv3 output",
species = "some kind of fish")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{ Object created by SS_output }
  \item{title}{ Title of output }
  \item{species}{ Species name }
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
\references{ A website related to FishGraph is \url{http://r-forge.r-project.org/projects/fishgraph/} }
\author{ Ian Taylor }
% \note{ ~~further notes~~

%  % ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
% \examples{}
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ data }
\keyword{ manip }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
