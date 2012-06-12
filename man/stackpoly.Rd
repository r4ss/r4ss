\name{stackpoly}
\alias{stackpoly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  function "stackpoly" by Jim Lemon from "plotrix" package
}
\description{
  Plot one or more columns of numeric values as the top edges of polygons
  instead of lines.
}
\usage{
stackpoly(x, y, main = "", xlab = "", ylab = "", xat = NA, xaxlab = NA,
xlim = NA, ylim = NA, lty = 1, border = NA, col = NA, axis4 = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A numeric data frame or matrix with the 'x' values. If 'y' is NULL, these
    will become the 'y' values and the 'x' positions will be the integers from 1 to
    dim(x)[1].
  }
  \item{y}{
    The 'y' values.
  }
  \item{main}{
    The title for the plot.
  }
  \item{xlab}{
    x axis labels for the plot.
  }
  \item{ylab}{
    y axis labels for the plot.
  }
  \item{xat}{
    Where to put the optional xaxlabs.
  }
  \item{xaxlab}{
    Optional labels for the x positions.
  }
  \item{xlim}{
    Optional x limits.
  }
  \item{ylim}{
    Optional y limits.
  }
  \item{lty}{
    Line type for the polygon borders.
  }
  \item{border}{
    Color for the polygon borders.
  }
  \item{col}{
    Color to fill the polygons. If NULL, 'rainbow' will be
    called to generate the colors. If NA, the polygons will not
    be filled.
  }
  \item{axis4}{
    option to add an axis on the right hand side
  }
  \item{\dots}{
    Additional arguments passed to 'plot'.
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
\references{
  \url{http://cran.r-project.org/web/packages/plotrix/index.html}
}
\author{
  Jim Lemon
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
%   %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
