\name{SSplotCatch}
\alias{SSplotCatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot catch related quantities.
}
\description{
  Plot catch related quantities from Stock Synthesis output. Plots include
  harvest rate, continuous F, landings, and discard fraction.
}
\usage{
SSplotCatch(replist, subplots = 1:15, add = FALSE, areas = 1,
plot = TRUE, print = FALSE, type = "l", fleetlty = 1,
fleetpch = 1, fleetcols = "default", fleetnames = "default", lwd = 3,
areacols = "default", areanames = "default", minyr = NULL, maxyr = NULL,
annualcatch = TRUE, forecastplot = TRUE, plotdir = "default",
showlegend = TRUE, legendloc = "topleft", xlab = "Year",
labels = c("Harvest rate/Year", "Continuous F", "Landings",
"Total catch", "Predicted Discards", "Discard fraction", "(mt)",
"(numbers x1000)", "Observed and expected",
"aggregated across seasons"),
catchasnumbers = FALSE, pwidth = 7, pheight = 7, punits = "in",
res = 300, ptsize = 12, cex.main = 1, verbose = TRUE)
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
  \item{areas}{
    optional subset of areas to plot for spatial models
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{type}{
    type parameter passed to plot function. Default "l" is lines only.
    Other options include "o" for overplotting points on lines.
  }
  \item{fleetlty}{
    vector of line type by fleet
  }
  \item{fleetpch}{
    vector of plot character by fleet
  }
  \item{fleetcols}{
    vector of colors by fleet
  }
  \item{fleetnames}{
    optional replacement for fleenames used in data file
  }
  \item{lwd}{
    line width
  }
  \item{areacols}{
    vector of colors by area. Default uses rich.colors by Arni Magnusson
  }
  \item{areanames}{
    names for areas. Default is to use Area1, Area2,...
  }
  \item{minyr}{
    optional input for minimum year to show in plots
  }
  \item{maxyr}{
    optional input for maximum year to show in plots
  }
  \item{annualcatch}{
    include plot of catch aggregated across seasons within each year
  }
  \item{forecastplot}{
    add points from forecast years
  }
  \item{plotdir}{
    directory where PNG or PDF files will be written. by default it will
    be the directory where the model was run.
  }
  \item{showlegend}{
    put legend on plot
  }
  \item{legendloc}{
    location of legend (see ?legend for more info)
  }
  \item{xlab}{
    x-label for all plots
  }
  \item{labels}{
    vector of labels for plots (titles and axis labels)
  }
  \item{catchasnumbers}{
    is catch in numbers instead of biomass?
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
    report progress to R console?
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
  \code{\link{SS_plots}}, \code{\link{SS_output}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
\keyword{ hplot }% __ONLY ONE__ keyword per line
