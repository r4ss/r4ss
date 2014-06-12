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
catchasnumbers = NULL, catchbars = TRUE, pwidth = 7, pheight = 7,
punits = "in", res = 300, ptsize = 12, cex.main = 1, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replist}{
    List created by \code{SS_output}
  }
  \item{subplots}{
    Vector controlling which subplots to create
  }
  \item{add}{
    Add to existing plot? (not yet implemented)
  }
  \item{areas}{
    Optional subset of areas to plot for spatial models
  }
  \item{plot}{
    Plot to active plot device?
  }
  \item{print}{
    Print to PNG files?
  }
  \item{type}{
    Type parameter passed to plot function. Default "l" is lines only.
    Other options include "o" for overplotting points on lines.
  }
  \item{fleetlty}{
    Vector of line type by fleet
  }
  \item{fleetpch}{
    Vector of plot character by fleet
  }
  \item{fleetcols}{
    Vector of colors by fleet
  }
  \item{fleetnames}{
    Optional replacement for fleenames used in data file
  }
  \item{lwd}{
    Line width
  }
  \item{areacols}{
    Vector of colors by area. Default uses rich.colors by Arni Magnusson
  }
  \item{areanames}{
    Names for areas. Default is to use Area1, Area2,...
  }
  \item{minyr}{
    Optional input for minimum year to show in plots
  }
  \item{maxyr}{
    Optional input for maximum year to show in plots
  }
  \item{annualcatch}{
    Include plot of catch aggregated across seasons within each year
  }
  \item{forecastplot}{
    Add points from forecast years
  }
  \item{plotdir}{
    Directory where PNG or PDF files will be written. By default it will
    be the directory where the model was run.
  }
  \item{showlegend}{
    Put legend on plot
  }
  \item{legendloc}{
    Location of legend (see ?legend for more info)
  }
  \item{xlab}{
    x-label for all plots
  }
  \item{labels}{
    Vector of labels for plots (titles and axis labels)
  }
  \item{catchasnumbers}{
    Is catch in numbers instead of biomass? Should be set automatically
    if set to NULL. If fleets include a mix of biomass and numbers, then
    catch plots should be interpreted carefully.
  }
  \item{catchbars}{
    Show catch by fleet as barplot instead of stacked polygons?
    (default=TRUE)
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
    ptsize for PNG file
  }
  \item{cex.main}{
    Character expansion for plot titles
  }
  \item{verbose}{
    Report progress to R console?
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
