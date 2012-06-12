\name{bubble3}
\alias{bubble3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a bubble plot.
}
\description{
  Bubble plot based on function vaguely based on \code{bubble} by Edzer
  Pebesma in gstat package. By default, positive values have closed bubbles and
  negative values have open bubbles.
}
\usage{
bubble3(x, y, z, col = c(1, 1), maxsize = 3, do.sqrt = TRUE, main = "",
cex.main = 1, xlab = "", ylab = "", minnbubble = 8, xlimextra = 1,
add = FALSE, las = 1, allopen = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Vector of x-values.
  }
  \item{y}{
    Vector of y-values.
  }
  \item{z}{
    Vector of bubble sizes.
  }
  \item{col}{
    Vector of two color values for positive and negative bubbles.
    Default=c(1,1).
  }
  \item{maxsize}{
    Size of largest bubble. Default=3.
  }
  \item{do.sqrt}{
    Should size be based on the area? (Diameter proportional to
    sqrt(z)). Default=TRUE.
  }
  \item{main}{
    Title of plot. Default="".
  }
  \item{cex.main}{
    Charecter expansion for title. Default=1.
  }
  \item{xlab}{
    X-axis label.
  }
  \item{ylab}{
    Y-axis label.
  }
  \item{minnbubble}{
    Minimum number of unique x values below which extra space is added
    to horizontal axis (to make plot look better). Default = 8.
  }
  \item{xlimextra}{
    Extra space (see minnbubble above). Default = 1.
  }
  \item{add}{
    Add bubbles to existing plot? Default=FALSE.
  }
  \item{las}{
    Style of axis labels (see ?par for more info).
  }
  \item{allopen}{
    Should all bubbles be open (instead of just negative values)?
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
  Ian Stewart and Ian Taylor
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
\keyword{ aplot }
\keyword{ hplot }
