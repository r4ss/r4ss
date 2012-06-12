\name{mountains}
\Rdversion{1.1}
\alias{mountains}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make shaded polygons with a mountain-like appearance
}
\description{
Designed to replicate like the cool-looking Figure 7 in Butterworth et
al. (2003).
}
\usage{
mountains(zmat, xvec=NULL, yvec=NULL, zscale=3, nshades=100,
           xaxs='i', yaxs='i', xlab="", ylab="", las=1, addbox=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zmat}{ a matrix where the rows represent the heights of each mountain range }
  \item{xvec}{ optional input for the x variable }
  \item{yvec}{ optional input for the y variable }
  \item{zscale}{ controls the height of the mountains relative to the
    y-axis and max(zmat) }
  \item{nshades}{ number of levels of shading }
  \item{xaxs}{ x-axis as internal or regular (see ?par for details) }
  \item{yaxs}{ y-axis as internal or regular (see ?par for details)  }
  \item{xlab}{ optional label for x-axis }
  \item{ylab}{ optional label for y-axis }
  \item{las}{ axis label style (see ?par for details). Default = 1 =
    horizontal axis labels.}
  \item{addbox}{ puts a box around the whole plot }
  \item{...}{ extra inputs passed to the plot command }
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
\references{
  Butterworth D.S., Ianelli J.N., Hilborn R. (2003) A statistical model
  for stock assessment of southern bluefin tuna with temporal changes in
  selectivity. South African Journal of Marine Science 25:331-362.
}
\author{
Ian Taylor
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% }
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.

% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{ hplot }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
