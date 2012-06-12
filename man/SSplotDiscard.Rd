\name{SSplotDiscard}
\alias{SSplotDiscard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot fit to discard fraction.
}
\description{
  Plot fit to discard fraction from Stock Synthesis output file.
}
\usage{
SSplotDiscard(replist, add = FALSE, plot = TRUE, print = FALSE,
plotdir = "default", fleets = "all", fleetnames = "default",
labels = c("Year", "Discard fraction", "Total discards", "for"),
yhi = 1, col1="blue", col2="red",
pwidth = 7, pheight = 7, punits = "in", res = 300,
ptsize = 12, cex.main = 1, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{\link{SS_output}}
  }
  \item{add}{
    add to existing plot (not yet implemented)
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{plotdir}{
    directory where PNG files will be written. by default it will
    be the directory where the model was run.
  }
  \item{fleets}{
    optional vector to subset fleets for which plots will be made
  }
  \item{fleetnames}{
    optional replacement for fleenames used in data file
  }
  \item{labels}{
    vector of labels for plots (titles and axis labels)
  }
  \item{yhi}{
    maximum y-value to include in plot (all data included regardless). Default = 1.
  }
  \item{col1}{
    first color to use in plot (for expected values)
  }
  \item{col2}{
    second color to use in plot (for observations and intervals)
  }
  \item{pwidth}{
    width of plot written to PNG file
  }
  \item{pheight}{
    height of plot written to PNG file
  }
  \item{punits}{
    units for PNG file
  }
  \item{res}{
    resolution for PNG file
  }
  \item{ptsize}{
    ptsize for PNG file
  }
  \item{cex.main}{
    character expansion for plot titles
  }
  \item{verbose}{
    report progress to R GUI?
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
  Ian Taylor, Ian Stewart
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
