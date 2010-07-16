\name{SSplotTimeseries}
\alias{SSplotTimeseries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot timeseries data
}
\description{
Plot timeseries data contained in TIME_SERIES output from Stock
Synthesis report file. Some values have optional uncertainty intervals.
}
\usage{
SSplotTimeseries(replist, subplots = 1:5, add = FALSE, areas = "all",
areacols = 1:10, areanames = "default", forecastplot = TRUE, uncertainty
= TRUE, bioscale = "default", plot = TRUE, print = FALSE, plotdir =
"default", verbose = FALSE, xlab = "Year",
labels = c("Total biomass (mt)", "Summary biomass (mt)",
"Spawning biomass (mt)", "Spawning depletion", "Spawning output (eggs)",
"Age-0 recruits (1,000s)"),
pwidth = 7, pheight = 7, punits = "in", res = 300, ptsize = 12, cex.main
= 1)
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
  \item{areacols}{
    vector of colors by area
  }
  \item{areanames}{
    names for areas. Default is to use Area1, Area2,...
  }
  \item{forecastplot}{
    add points from forecast years
  }
  \item{uncertainty}{
    add intervals around quantities for which uncertainty is available
  }
  \item{bioscale}{
    scaling for spawning biomass by default it will be set to 0.5 for
    single-sex models, and 1.0 for all others
  }
  \item{plot}{
    plot to active plot device?
  }
  \item{print}{
    print to PNG files?
  }
  \item{plotdir}{
    directory where PNG or PDF files will be written. by default it will
    be the directory where the model was run.
  }
  \item{verbose}{
    report progress to R GUI?
  }
  \item{xlab}{
    x axis label for all plots
  }
  \item{labels}{
    vector of labels for plots (titles and axis labels)
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
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  Ian Taylor, Ian Stewart
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{SS_plots}}, \code{\link{SS_output}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
