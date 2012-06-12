\name{plotCI}
\alias{plotCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot points with confidence intervals.
}
\description{
  Given a set of x and y values and upper and lower bounds, this function
  plots the points with error bars. This was Written by Venables and
  modified to add access to ylim and contents.
}
\usage{
plotCI(x, y = NULL, uiw, liw = uiw, ylo = NULL, yhi = NULL, ...,
sfrac = 0.01, ymax = NULL, add = FALSE, col = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    The x coordinates of points in the plot
  }
  \item{y}{
    The y coordinates of the points in the plot.
  }
  \item{uiw}{
    The width of the upper portion of the confidence region.
  }
  \item{liw}{
    The width of the lower portion of the confidence region.
  }
  \item{ylo}{
    Lower limit of y range.
  }
  \item{yhi}{
    Upper limit of y range.
  }
  \item{\dots}{
    Additional inputs that will be passed to the function \code{plot(x,y,ylim=ylim,...)}
  }
  \item{sfrac}{
    Fraction of width of plot to be used for bar ends.
  }
  \item{ymax}{
    Additional input for Upper limit of y range.
  }
  \item{add}{
    Add points and intervals to existing plot? Default=FALSE.
  }
  \item{col}{
    Color for the points and lines.
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
  Bill Venables, Ian Stewart, Ian Taylor, John Wallace
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
\keyword{ aplot }
\keyword{ hplot }% __ONLY ONE__ keyword per line
