\name{SSplotYield}
\alias{SSplotYield}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot yield and surplus production.
}
\description{
  Plot yield and surplus production from Stock Synthesis output. Surplus
  production is based on Walters et al. (2008).
}
\usage{
SSplotYield(replist, subplots = 1:2, add = FALSE, plot = TRUE, print =
FALSE, labels = c("Relative depletion", "Equilibrium yield (mt)",
"Total biomass (mt)", "Surplus production (mt)"),
col = "blue", lty = 1, lwd = 2, cex.main = 1,
pwidth = 7, pheight = 7, punits = "in", res = 300, ptsize = 12,
plotdir = "default", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    list created by \code{SS_output}
  }
  \item{subplots}{
    vector controlling which subplots to create
  }
  \item{add}{
    add to existing plot? (not yet implemented)
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{labels}{
    vector of labels for plots (titles and axis labels)
  }
  \item{col}{
    line color (only applied to equilbrium yield plot at this time)
  }
  \item{lty}{
    line type (only applied to equilbrium yield plot at this time)
  }
  \item{lwd}{
    line width (only applied to equilbrium yield plot at this time)
  }
  \item{cex.main}{
    character expansion for plot titles
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
  \item{plotdir}{
    directory where PNG files will be written. by default it will
    be the directory where the model was run.
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
\references{ Walters, Hilborn, and  Christensen, 2008, Surplus
  production dynamics in declining and recovering fish populations.
  Can. J. Fish. Aquat. Sci. 65: 2536-2551
}
\author{
  Ian Stewart, Ian Taylor
}
% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}, \code{\link{SS_output}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
