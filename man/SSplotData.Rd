% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSplotData.R
\name{SSplotData}
\alias{SSplotData}
\title{Timeline of presence/absence of data by type, year, and fleet.}
\usage{
SSplotData(
  replist,
  plot = TRUE,
  print = FALSE,
  plotdir = "default",
  subplots = 1:2,
  fleetcol = "default",
  datatypes = "all",
  fleets = "all",
  fleetnames = "default",
  ghost = FALSE,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  margins = c(5.1, 2.1, 2.1, 8.1),
  cex = 2,
  lwd = 12,
  maxsize = 1,
  alphasize = 1,
  mainTitle = FALSE,
  verbose = TRUE,
  subplot = lifecycle::deprecated()
)
}
\arguments{
\item{replist}{A list object created by \code{\link{SS_output}()}.}

\item{plot}{Plot to active plot device?}

\item{print}{Print to PNG files?}

\item{plotdir}{Directory where PNG files will be written.}

\item{subplots}{vector controlling which subplots to create
Currently there are only 2 subplots:
\itemize{
\item 1 equal size points showing presence/absence of data type by year/fleet
\item 2 points scaled to indicate quantity or precision of data
}}

\item{fleetcol}{Either the string "default", or a vector of colors to use
for each fleet. If tagging data or environmental data are included,
an additional color needs to be
added for the tag releases which are not assigned to a fleet.}

\item{datatypes}{Either the string "all", or a vector including some subset
of the following: "catch", "cpue", "lendbase", "sizedbase", "agedbase",
"condbase", "ghostagedbase", "ghostcondbase", "ghostlendbase", "ladbase",
"wadbase", "mnwgt", "discard", "tagrelease", "tagdbase1", and "morphcompdbase".}

\item{fleets}{Either the string "all", or a vector of numerical values, like
c(1,3), listing fleets or surveys to be included in the plot.}

\item{fleetnames}{Optional replacement for fleetnames used in data file.}

\item{ghost}{TRUE/FALSE indicator for whether to show presence of
composition data from ghost fleets (data for which the fit is shown, but is
not included in the likelihood calculations).}

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

\item{margins}{margins of plot (passed to par() function), which may need to
be increased if fleet names run off right-hand margin}

\item{cex}{Character expansion for points showing isolated years of data}

\item{lwd}{Line width for plot elements.}

\item{maxsize}{The size (cex) of the largest bubble in the datasize
plot. Default is 1.}

\item{alphasize}{The transparency of the bubbles in the datasize
plot. Defaults to 1 (no transparency). Useful for models with lots of
overlapping points.}

\item{mainTitle}{Logical indicating if a title should be included at the top
(not yet implemented for all plots).}

\item{verbose}{A logical value specifying if output should be printed
to the screen.}

\item{subplot}{Deprecated. Use subplots instead.}
}
\description{
Plot shows graphical display of what data is being used in the model.  Some
data types may not yet be included. Note, this is based on output from the
model, not the input data file.
}
\seealso{
\code{\link[=SS_plots]{SS_plots()}}, \code{\link[=SS_output]{SS_output()}},
\code{\link[=SS_readdat]{SS_readdat()}}
}
\author{
Ian Taylor, Chantel Wetzel, Cole Monnahan
}
