\name{rich.colors.short}
\alias{rich.colors.short}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Make a vector of colors.
}
\description{
  A subset of rich.colors by Arni Magnusson from the gplots package,
  with the addition of alpha transparency (which is now available in
  the gplots version as well)
}
\usage{
rich.colors.short(n, alpha = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
    Number of colors to generate.
  }
  \item{alpha}{
    Alpha transparency value for all colors in vector. Value is passed
    to rgb function.
  }
}
% \details{
%   %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
%   %%  ~Describe the value returned
%   %%  If it is a LIST, use
%   %%  \item{comp1 }{Description of 'comp1'}
%   %%  \item{comp2 }{Description of 'comp2'}
%   %% ...
% }
% \references{
%   %% ~put references to the literature/web site here ~
% }
\author{
  Arni Magnusson, Ian Taylor
}
% \note{
%   %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
%   %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }
