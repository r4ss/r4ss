% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSplotPars.R
\name{SSplotPars}
\alias{SSplotPars}
\title{Plot distributions of priors, posteriors, and estimates.}
\usage{
SSplotPars(
  replist,
  plotdir = NULL,
  xlab = "Parameter value",
  ylab = "Density",
  showmle = TRUE,
  showpost = TRUE,
  showprior = TRUE,
  showinit = TRUE,
  showdev = FALSE,
  showlegend = TRUE,
  fitrange = FALSE,
  xaxs = "i",
  xlim = NULL,
  ylim = NULL,
  verbose = TRUE,
  debug = FALSE,
  nrows = 4,
  ncols = 2,
  ltyvec = c(1, 1, 3, 4),
  colvec = c("blue", "red", "black", "gray60", rgb(0, 0, 0, 0.5)),
  add = FALSE,
  plot = TRUE,
  print = FALSE,
  pwidth = 6.5,
  pheight = 6.5,
  punits = "in",
  ptsize = 10,
  res = 300,
  strings = NULL,
  exact = FALSE,
  newheaders = NULL
)
}
\arguments{
\item{replist}{A list object created by \code{\link{SS_output}()}.}

\item{plotdir}{Directory where PNG files will be written.}

\item{xlab}{Label on horizontal axis.}

\item{ylab}{Label on vertical axis.}

\item{showmle}{Show MLE estimate and asymptotic variance estimate with blue
lines?}

\item{showpost}{Show posterior distribution as bar graph if MCMC results
are available in \code{replist}?}

\item{showprior}{Show prior distribution as black line?}

\item{showinit}{Show initial value as red triangle?}

\item{showdev}{Include devs in the plot?}

\item{showlegend}{Show the legend?}

\item{fitrange}{Fit range tightly around MLE & posterior distributions,
instead of full parameter range?}

\item{xaxs}{Parameter input for x-axis. See \code{?par} for more info.}

\item{xlim}{Optional x-axis limits to be applied to all plots.
Otherwise, limits are based on the model results.}

\item{ylim}{Optional y-axis limits to be applied to all plots.
Otherwise, limits are based on the model results.}

\item{verbose}{A logical value specifying if output should be printed
to the screen.}

\item{debug}{Provide additional messages to help with debugging when the
function fails.}

\item{nrows}{How many rows in multi-figure plot.}

\item{ncols}{How many columns in multi-figure plot.}

\item{ltyvec}{Vector of line types used for lines showing MLE and prior
distributions and the median of the posterior distribution.}

\item{colvec}{Vector of colors used for lines and polygons showing MLE,
initial value, prior, posterior, and median of the posterior.}

\item{add}{Add to existing plot?}

\item{plot}{Plot to active plot device?}

\item{print}{Print to PNG files?}

\item{pwidth}{Default width of plots printed to files in units of
\code{punits}.}

\item{pheight}{Height of plots printed to png files in units of \code{punits}.
Default is designed to allow two plots per page, with \code{pheight_tall} used
for plots that work best with a taller format and a single plot per page.}

\item{punits}{Units for \code{pwidth} and \code{pheight}. Can be "px"
(pixels), "in" (inches), "cm" (centimeters), or "mm" (millimeters).
The default is \code{punits="in"}.}

\item{ptsize}{Point size for plotted text in plots printed to files (see
\code{help("png")} in R for details).}

\item{res}{Resolution of plots printed to files.
The default is \code{res = 300}.}

\item{strings}{Subset parameters included in the plot using substring from
parameter names (i.e. "SR" will get "SR_LN(R0)" and "SR_steep" if they are both
estimated quantities in this model).}

\item{exact}{Should strings input match parameter names exactly?  Otherwise
substrings are allowed.}

\item{newheaders}{Optional vector of headers for each panel to replace the
parameter names.}
}
\description{
Make multi-figure plots of prior, posterior, and estimated asymptotic
parameter distributions. MCMC not required to make function work.
}
\examples{
\dontrun{
# read model results
model <- SS_output(dir = "c:/SS/Simple/")
# make default plots where parameter distribution plots will appear
# in the "pars" tab
SS_plots(model)

# create just the "pars" tab with control of the inputs that are
# passed to SSplotPars
SS_plots(model,
  plot = 25, showmle = TRUE, showpost = TRUE,
  showprior = TRUE, showinit = TRUE, showdev = FALSE, fitrange = FALSE
)

# call SSplotPars directly
SSplotPars(replist = model)

# Create plot in custom location. Note that strings can be partial match.
# File name will be "parameter_distributions.png"
# or "parameter_distributions_pageX.png" when they don't all fit on one page
SSplotPars(
  replist = model, strings = c("steep", "R0"),
  nrows = 2, ncols = 1, plot = FALSE, print = TRUE,
  plotdir = file.path(model[["inputs"]][["dir"]], "distribution_plots")
)
}

}
\author{
Ian G. Taylor, Cole C. Monnahan
}
