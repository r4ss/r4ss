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
SSplotDiscard(replist, subplots = 1:2, plot = TRUE, print = FALSE,
plotdir = "default", fleets = "all", fleetnames = "default",
datplot = FALSE,
labels = c("Year", "Discard fraction", "Total discards", "for"),
yhi = 1, col1="blue", col2="red",
pwidth = 7, pheight = 7, punits = "in", res = 300,
ptsize = 12, cex.main = 1, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    List created by \code{\link{SS_output}}
  }
  \item{subplots}{
    Vector of which plots to make (1 = data only, 2 = with fit). If
    \code{plotdat = FALSE} then subplot 1 is not created, regardless of
    choice of \code{subplots}.
  }
  \item{plot}{
    Plot to active plot device?
  }
  \item{print}{
    Print to PNG files?
  }
  \item{plotdir}{
    Directory where PNG files will be written. by default it will
    be the directory where the model was run.
  }
  \item{fleets}{
    Optional vector to subset fleets for which plots will be made
  }
  \item{fleetnames}{
    Optional replacement for fleenames used in data file
  }
  \item{datplot}{
    Make data-only plot of discards? This can override the choice of
    \code{subplots}.
  }
  \item{labels}{
    Vector of labels for plots (titles and axis labels)
  }
  \item{yhi}{
    Maximum y-value to include in plot (all data included regardless). Default = 1.
  }
  \item{col1}{
    First color to use in plot (for expected values)
  }
  \item{col2}{
    Second color to use in plot (for observations and intervals)
  }
  \item{pwidth}{
    Width of plot written to PNG file
  }
  \item{pheight}{
    Height of plot written to PNG file
  }
  \item{punits}{
    Units for PNG file
  }
  \item{res}{
    Resolution for PNG file
  }
  \item{ptsize}{
    Point size for PNG file
  }
  \item{cex.main}{
    Character expansion for plot titles
  }
  \item{verbose}{
    Report progress to R GUI?
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
