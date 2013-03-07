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
bubble3(x, y, z, col = 1, cexZ1=5, maxsize = NULL, do.sqrt = TRUE,
legend=TRUE, legend.z="default", legend.yadj=1.1, main = "",
cex.main = 1, xlab = "", ylab = "", minnbubble = 8,
xlim = NULL, ylim = NULL, axis1 = TRUE, xlimextra = 1,
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
    Color for bubbles.
  }
  \item{cexZ1}{
    Character expansion (cex) value for a proportion of 1.0.
  }
  \item{maxsize}{
    Size of largest bubble. Prefered option is now an expansion factor
    for a bubble with z=1 (see \code{cexZ1} above).
  }
  \item{do.sqrt}{
    Should size be based on the area? (Diameter proportional to
    sqrt(z)). Default=TRUE.
  }
  \item{legend}{
    Add a legend to the plot?
  }
  \item{legend.z}{
    If a legend is added, what z values will be shown. Default is
    c(-3,-2,-1,.1,1,2,3) for Pearson-like quantities and a smaller range
    for proportions that are all less than 1.
  }
  \item{legend.yadj}{
    If a legend is added, how much should the y-axis be expanded to make
    space for it.
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
  \item{xlim}{
    Optional limits on x-range.
  }
  \item{ylim}{
    Optional limits on y-range.
  }
  \item{axis1}{
    Show the horizontal axis on plot? Option allows turning off for use
    in multi-figure plots.
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
