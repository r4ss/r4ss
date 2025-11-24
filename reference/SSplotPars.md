# Plot distributions of priors, posteriors, and estimates.

Make multi-figure plots of prior, posterior, and estimated asymptotic
parameter distributions. MCMC not required to make function work.

## Usage

``` r
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
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- plotdir:

  Directory where PNG files will be written.

- xlab:

  Label on horizontal axis.

- ylab:

  Label on vertical axis.

- showmle:

  Show MLE estimate and asymptotic variance estimate with blue lines?

- showpost:

  Show posterior distribution as bar graph if MCMC results are available
  in `replist`?

- showprior:

  Show prior distribution as black line?

- showinit:

  Show initial value as red triangle?

- showdev:

  Include devs in the plot?

- showlegend:

  Show the legend?

- fitrange:

  Fit range tightly around MLE & posterior distributions, instead of
  full parameter range?

- xaxs:

  Parameter input for x-axis. See
  [`?par`](https://rdrr.io/r/graphics/par.html) for more info.

- xlim:

  Optional x-axis limits to be applied to all plots. Otherwise, limits
  are based on the model results.

- ylim:

  Optional y-axis limits to be applied to all plots. Otherwise, limits
  are based on the model results.

- verbose:

  A logical value specifying if output should be printed to the screen.

- debug:

  Provide additional messages to help with debugging when the function
  fails.

- nrows:

  How many rows in multi-figure plot.

- ncols:

  How many columns in multi-figure plot.

- ltyvec:

  Vector of line types used for lines showing MLE and prior
  distributions and the median of the posterior distribution.

- colvec:

  Vector of colors used for lines and polygons showing MLE, initial
  value, prior, posterior, and median of the posterior.

- add:

  Add to existing plot?

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- pwidth:

  Default width of plots printed to files in units of `punits`.

- pheight:

  Height of plots printed to png files in units of `punits`. Default is
  designed to allow two plots per page, with `pheight_tall` used for
  plots that work best with a taller format and a single plot per page.

- punits:

  Units for `pwidth` and `pheight`. Can be "px" (pixels), "in" (inches),
  "cm" (centimeters), or "mm" (millimeters). The default is
  `punits="in"`.

- ptsize:

  Point size for plotted text in plots printed to files (see
  [`help("png")`](https://rdrr.io/r/grDevices/png.html) in R for
  details).

- res:

  Resolution of plots printed to files. The default is `res = 300`.

- strings:

  Subset parameters included in the plot using substring from parameter
  names (i.e. "SR" will get "SR_LN(R0)" and "SR_steep" if they are both
  estimated quantities in this model).

- exact:

  Should strings input match parameter names exactly? Otherwise
  substrings are allowed.

- newheaders:

  Optional vector of headers for each panel to replace the parameter
  names.

## Author

Ian G. Taylor, Cole C. Monnahan

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
