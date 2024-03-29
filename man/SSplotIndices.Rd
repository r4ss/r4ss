% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSplotIndices.R
\name{SSplotIndices}
\alias{SSplotIndices}
\title{Plot indices of abundance and associated quantities.}
\usage{
SSplotIndices(
  replist,
  subplots = c(1:10, 12),
  plot = TRUE,
  print = FALSE,
  fleets = "all",
  fleetnames = "default",
  smooth = TRUE,
  add = FALSE,
  datplot = TRUE,
  labels = c("Year", "Index", "Observed index", "Expected index", "Log index",
    "Log observed index", "Log expected index", "Standardized index", "Catchability (Q)",
    "Time-varying catchability", "Vulnerable biomass",
    "Catchability vs. vulnerable biomass", "Residual", "Deviation"),
  fleetcols = NULL,
  col1 = "default",
  col2 = "default",
  col3 = "blue",
  col4 = "red",
  pch1 = 21,
  pch2 = 16,
  cex = 1,
  bg = "white",
  legend = TRUE,
  legendloc = "topright",
  seasnames = NULL,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  mainTitle = FALSE,
  plotdir = "default",
  minyr = NULL,
  maxyr = NULL,
  maximum_ymax_ratio = Inf,
  show_input_uncertainty = TRUE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{replist}{A list object created by \code{\link{SS_output}()}.}

\item{subplots}{vector controlling which subplots to create
Numbering of subplots is as follows, where subplot 9 (comparison of all indices) is
provided first:
\itemize{
\item 1  index data by fleet
\item 2  index data with fit by fleet
\item 3  observed vs expected index values with smoother
\item 4  index data by fleet on a log scale (lognormal error only)
\item 5  index data with fit by fleet on a log scale (lognormal error only)
\item 6  log(observed) vs log(expected) with smoother (lognormal error only)
\item 7  time series of time-varying catchability (only if actually time-varying)
\item 8  catchability vs. vulnerable biomass (if catchability is not constant)
\item 9  comparison of all indices
\item 10  index residuals based on total uncertainty
\item 11  index residuals based on input uncertainty (not currently provided)
\item 12  index deviations (independent of index uncertainty)
}}

\item{plot}{Plot to active plot device?}

\item{print}{Print to PNG files?}

\item{fleets}{Either the string "all", or a vector of numerical values, like
c(1,3), listing fleets or surveys to be included in the plot.}

\item{fleetnames}{Optional replacement for fleetnames used in data file.}

\item{smooth}{add smoothed line to plots of observed vs. expected sample
sizes}

\item{add}{add to existing plot (not yet implemented)}

\item{datplot}{make plot of data only?}

\item{labels}{Vector of labels for plots (titles and axis labels).}

\item{fleetcols}{vector of colors for all fleets (including those
with no index data)}

\item{col1}{vector of colors for points in each season for time series plot.
Default is red for single season models and a rainbow using the
rich.colors.short function for multiple seasons.}

\item{col2}{vector of colors for points in each season for obs. vs. exp.
plot.  Default is blue for single season models and a rainbow using the
rich.colors.short function for multiple seasons.}

\item{col3}{color of line showing expected index in time series plot.
Default is blue.}

\item{col4}{color of smoother shown in obs. vs. exp. plots. Default is red.}

\item{pch1}{single value or vector of plotting characters (pch parameter)
for time-series plots of index fit. Default=21.}

\item{pch2}{single value or vector of plotting characters (pch parameter)
for sample size plots of index fit. Default=16.}

\item{cex}{character expansion factor for points showing observed values.
Default=1.}

\item{bg}{Background color for points with pch=21.}

\item{legend}{add a legend to seasonal colors (only for seasonal models)}

\item{legendloc}{Location of legend. Either a string like "topleft" or a
vector of two numeric values representing the fraction of the maximum in
the x and y dimensions, respectively. See \code{help("legend")} for more info on
the string options.}

\item{seasnames}{optional vector of names for each season to replace
defaults if a legend is used}

\item{pwidth}{Default width of plots printed to files in units of
\code{punits}.}

\item{pheight}{Height of plots printed to png files in units of \code{punits}.
Default is designed to allow two plots per page, with \code{pheight_tall} used
for plots that work best with a taller format and a single plot per page.}

\item{punits}{Units for \code{pwidth} and \code{pheight}. Can be "px"
(pixels), "in" (inches), "cm" (centimeters), or "mm" (millimeters).
The default is \code{punits="in"}.}

\item{res}{Resolution of plots printed to files.
The default is \code{res = 300}.}

\item{ptsize}{Point size for plotted text in plots printed to files (see
\code{help("png")} in R for details).}

\item{cex.main}{Character expansion for plot titles.
The default is \code{cex.main=1}.}

\item{mainTitle}{Logical indicating if a title should be included at the top
(not yet implemented for all plots).}

\item{plotdir}{Directory where PNG files will be written.}

\item{minyr}{First year to show in plot (for zooming in on a subset of
values)}

\item{maxyr}{Last year to show in plot (for zooming in on a subset of
values)}

\item{maximum_ymax_ratio}{Maximum allowed value for ymax (specified
as ratio of y), which overrides any
value of ymax that is greater (default = Inf)}

\item{show_input_uncertainty}{Switch controlling whether to add thicker
uncertainty interval lines indicating the input uncertainty relative to
the total uncertainty which may result from estimating a parameter for
extra standard deviations. This is only added for the plots with index
fit included (the data-only plots only show the input uncertainty).}

\item{verbose}{A logical value specifying if output should be printed
to the screen.}

\item{\dots}{Extra arguments to pass to calls to \code{plot}}
}
\description{
Plot indices of abundance with or without model fit as well as other diagnostic
plots such as observed vs. expected index and plots related to time-varying
catchability (if present).
}
\seealso{
\code{\link[=SS_plots]{SS_plots()}}, \code{\link[=SS_output]{SS_output()}}
}
\author{
Ian Stewart, Ian Taylor, James Thorson
}
