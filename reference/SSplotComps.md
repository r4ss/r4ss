# Plot composition data and fits.

Plot composition data and fits from Stock Synthesis output. Multi-figure
plots depend on `make_multifig`.

## Usage

``` r
SSplotComps(
  replist,
  subplots = c(1:10, 21, 24),
  kind = "LEN",
  sizemethod = 1,
  aalyear = -1,
  aalbin = -1,
  plot = TRUE,
  print = FALSE,
  fleets = "all",
  fleetnames = "default",
  sexes = "all",
  yupper = 0.4,
  datonly = FALSE,
  samplesizeplots = TRUE,
  compresidplots = TRUE,
  bub = FALSE,
  showyears = TRUE,
  showsampsize = TRUE,
  showeffN = TRUE,
  aggregates_by_mkt = FALSE,
  sampsizeline = FALSE,
  effNline = FALSE,
  minnbubble = 3,
  pntscalar = NULL,
  scalebubbles = FALSE,
  cexZ1 = 1.5,
  bublegend = TRUE,
  colvec = c(rgb(1, 0, 0, 0.7), rgb(0, 0, 1, 0.7), rgb(0.1, 0.1, 0.1, 0.7)),
  linescol = c(rgb(0, 0.5, 0, 0.7), rgb(0.8, 0, 0, 0.7), rgb(0, 0, 0.8, 0.7)),
  xlas = 0,
  ylas = NULL,
  axis1 = NULL,
  axis2 = NULL,
  axis1labs = NULL,
  sizebinlabs = NULL,
  blue = rgb(0, 0, 1, 0.7),
  red = rgb(1, 0, 0, 0.7),
  pwidth = 6.5,
  pheight = 6.5,
  punits = "in",
  ptsize = 10,
  res = 300,
  plotdir = "default",
  cex.main = 1,
  linepos = 1,
  fitbar = FALSE,
  do.sqrt = TRUE,
  smooth = TRUE,
  cohortlines = c(),
  labels = c("Length (cm)", "Age (yr)", "Year", "Observed sample size",
    "Effective sample size", "Proportion", "cm", "Frequency", "Weight", "Length", "(t)",
    "(numbers x1000)", "Stdev (Age)", "Conditional AAL plot, ", "Size bin"),
  printmkt = TRUE,
  printsex = TRUE,
  maxrows = 6,
  maxcols = 4,
  maxrows2 = 4,
  maxcols2 = 4,
  rows = 1,
  cols = 1,
  andre_oma = c(3, 0, 3, 0),
  andrerows = 4,
  fixdims = TRUE,
  fixdims2 = FALSE,
  maxneff = 5000,
  verbose = TRUE,
  scalebins = FALSE,
  addMeans = TRUE,
  mainTitle = FALSE,
  ...
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- subplots:

  vector controlling which subplots to create Numbering of subplots is
  as follows, where subplots 21 to 24 (aggregated across years) are
  provided first, and subplots 1 to 10 are all repeated for each fleet

  - 1 index data by fleet

  - 1 multi-panel composition plot

  - 2 single panel bubble plot for numbers at length or age

  - 3 multi-panel bubble plots for conditional age-at-length

  - 4 multi-panel plot of fit to conditional age-at-length for specific
    years

  - 5 Pearson residuals for A-L key

  - 6 multi-panel plot of point and line fit to conditional
    age-at-length for specific length bins

  - 7 sample size plot

  - 8 TA1.8 Francis plot for marginal data with Dirichlet-Multinomial
    and no Francis adjustment

  - 9 TA1.8 Francis weighting plot for marginal data

  - 10 TA1.8 Francis plot for conditional data with
    Dirichlet-Multinomial and no Francis adjustment

  - 11 TA1.8 Francis weighting plot for conditional data

  - 12 Andre's mean age and std. dev. in conditional AAL

  - 21 composition by fleet aggregating across years

  - 22 composition by fleet aggregating across years within each season

  - 23 composition by fleet aggregating across seasons within a year

  - 24 bubble plot comparison of length or age residuals

- kind:

  indicator of type of plot can be "LEN", "SIZE", "AGE", "cond",
  "GSTAGE", "GSTLEN", "L@A", or "W@A".

- sizemethod:

  if kind = "SIZE" then this switch chooses which of the generalized
  size bin methods will be plotted.

- aalyear:

  Years to plot multi-panel conditional age-at-length fits for all
  length bins; must be in a "c(YYYY,YYYY)" format. Useful for checking
  the fit of a dominant year class, critical time period, etc.
  Default=-1.

- aalbin:

  The length bin for which multi-panel plots of the fit to conditional
  age-at-length data will be produced for all years. Useful to see if
  growth curves are ok, or to see the information on year classes move
  through the conditional data. Default=-1.

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- fleets:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing fleets or surveys to be included in the plot.

- fleetnames:

  Optional replacement for fleetnames used in data file.

- sexes:

  which sexes to show plots for. Default="all" which will include males,
  females, and unsexed. This option is not fully implemented for all
  plots.

- yupper:

  upper limit on ymax for polygon/histogram composition plots

- datonly:

  make plots of data without fits?

- samplesizeplots:

  make sample size plots?

- compresidplots:

  make plots of residuals for fit to composition data?

- bub:

  make bubble plot for numbers at age or size?

- showyears:

  Add labels for years to sample size plots?

- showsampsize:

  add sample sizes to plot

- showeffN:

  add effective sample sizes to plot

- aggregates_by_mkt:

  separate plots of aggregates across years into different plots for
  each market category (retained, discarded)?

- sampsizeline:

  show line for input sample sizes on top of conditional age-at-length
  plots (TRUE/FALSE, still in development)

- effNline:

  show line for effective sample sizes on top of conditional
  age-at-length plots (TRUE/FALSE, still in development)

- minnbubble:

  number of unique x values before adding buffer. see ?bubble3 for more
  info.

- pntscalar:

  This scalar defines the maximum bubble size for bubble plots. This
  option is still available but a better choice is to use cexZ1 which
  allow the same scaling throughout all plots.

- scalebubbles:

  scale data-only bubbles by sample size, not just proportion within
  sample? Default=FALSE.

- cexZ1:

  Character expansion (cex) for point associated with value of 1.

- bublegend:

  Add legend with example bubble sizes to bubble plots.

- colvec:

  Vector of length 3 with colors for females, males, unsexed fish

- linescol:

  Color for lines on top of polygons

- xlas:

  label style (las) input for x-axis. Default 0 has horizontal labels,
  input 2 would provide vertical labels.

- ylas:

  label style (las) input for y-axis. Default NULL has horizontal labels
  when all labels have fewer than 6 characters and vertical otherwise.
  Input 0 would force vertical labels, and 1 would force horizontal.

- axis1:

  optional position of bottom axis values

- axis2:

  optional position of left size axis values

- axis1labs:

  optional vector of labels for axis1 (either NULL or needs to match
  length of axis1)

- sizebinlabs:

  Vector of size bin labels corresponding to the generalized size
  frequency method

- blue:

  What color to use for males in bubble plots (default is slightly
  transparent blue)

- red:

  What color to use for females in bubble plots (default is slightly
  transparent red)

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

- plotdir:

  Directory where PNG files will be written.

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

- linepos:

  should lines be added before points (linepos=1) or after (linepos=2)?

- fitbar:

  show fit to bars instead of points

- do.sqrt:

  scale bubbles based on sqrt of size vector. see ?bubble3 for more
  info.

- smooth:

  add loess smoother to observed vs. expected index plots and input vs.
  effective sample size?

- cohortlines:

  optional vector of birth years for cohorts for which to add growth
  curves to numbers at length bubble plots

- labels:

  Vector of labels for plots (titles and axis labels).

- printmkt:

  show market categories in plot titles?

- printsex:

  show sex in plot titles?

- maxrows:

  maximum (or fixed) number or rows of panels in the plot

- maxcols:

  maximum (or fixed) number or columns of panels in the plot

- maxrows2:

  maximum number of rows for conditional age at length plots

- maxcols2:

  maximum number of columns for conditional age at length plots

- rows:

  number or rows to return to as default for next plots to come or for
  single plots

- cols:

  number or cols to return to as default for next plots to come or for
  single plots

- andre_oma:

  Outer margins passed to Andre's multi-panel conditional age-at-length
  plots.

- andrerows:

  Number of rows of Andre's conditional age-at-length plots within each
  page. Default=3.

- fixdims:

  fix the dimensions at maxrows by maxcols or resize based on number of
  years of data

- fixdims2:

  fix the dimensions at maxrows by maxcols in aggregate plots or resize
  based on number of fleets

- maxneff:

  the maximum value to include on plots of input and effective sample
  size. Occasionally a calculation of effective N blows up to very large
  numbers, rendering it impossible to observe the relationship for other
  data. Default=5000.

- verbose:

  A logical value specifying if output should be printed to the screen.

- scalebins:

  Rescale expected and observed proportions by dividing by bin width for
  models where bins have different widths? Caution!: May not work
  correctly in all cases.

- addMeans:

  Add parameter means in addition to medians for MCMC posterior
  distributions in which the median and mean differ.

- mainTitle:

  Logical indicating if a title should be included at the top (not yet
  implemented for all plots).

- ...:

  additional arguments that will be passed to the `par` command in the
  [`make_multifig()`](https://r4ss.github.io/r4ss/reference/make_multifig.md)
  function.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`make_multifig()`](https://r4ss.github.io/r4ss/reference/make_multifig.md)

## Author

Ian Taylor
