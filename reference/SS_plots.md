# plot many quantities related to output from Stock Synthesis

Creates a user-chosen set of plots, including biological quantities,
time series, and fits to data. Plots are sent to R GUI, single PDF file,
or multiple PNG files. This is now just a wrapper which calls on
separate functions to make all the plots.

## Usage

``` r
SS_plots(
  replist = NULL,
  plot = 1:26,
  pdf = FALSE,
  png = TRUE,
  html = png,
  printfolder = "plots",
  dir = "default",
  fleets = "all",
  areas = "all",
  fleetnames = "default",
  fleetcols = "default",
  fleetlty = 1,
  fleetpch = 1,
  lwd = 1,
  areacols = NULL,
  areanames = "default",
  verbose = TRUE,
  uncertainty = TRUE,
  forecastplot = FALSE,
  datplot = TRUE,
  Natageplot = TRUE,
  samplesizeplots = TRUE,
  compresidplots = TRUE,
  comp.yupper = 0.4,
  sprtarg = "default",
  btarg = "default",
  minbthresh = "default",
  pntscalar = NULL,
  bub.scale.pearson = 1.5,
  bub.scale.dat = 3,
  pntscalar.nums = 2.6,
  pntscalar.tags = 2.6,
  minnbubble = 8,
  aalyear = -1,
  aalbin = -1,
  aalresids = TRUE,
  maxneff = 5000,
  cohortlines = c(),
  smooth = TRUE,
  showsampsize = TRUE,
  showeffN = TRUE,
  sampsizeline = FALSE,
  effNline = FALSE,
  showlegend = TRUE,
  pwidth = 6.5,
  pheight = 4,
  pheight_tall = 6.5,
  punits = "in",
  ptsize = 10,
  res = 300,
  mainTitle = FALSE,
  cex.main = 1,
  selexlines = 1:6,
  rows = 1,
  cols = 1,
  maxrows = 6,
  maxcols = 4,
  maxrows2 = 4,
  maxcols2 = 4,
  andrerows = 4,
  tagrows = 3,
  tagcols = 3,
  parrows = 4,
  parcols = 2,
  fixdims = TRUE,
  new = TRUE,
  SSplotDatMargin = 8,
  filenotes = NULL,
  catchasnumbers = NULL,
  catchbars = TRUE,
  legendloc = "topleft",
  minyr = -Inf,
  maxyr = Inf,
  sexes = "all",
  scalebins = FALSE,
  scalebubbles = FALSE,
  tslabels = NULL,
  catlabels = NULL,
  maxsize = 1,
  showmle = TRUE,
  showpost = TRUE,
  showprior = TRUE,
  showinit = TRUE,
  showdev = FALSE,
  fitrange = FALSE,
  ...
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- plot:

  Plot sets to be created, see list of plots below. Use to specify only
  those plot sets of interest, e.g., c(1,2,5,10). Plots for data not
  available in the model run will automatically be skipped, whether
  called or not. Current grouping of plots is as follows:

  1.  Biology

  2.  Selectivity and retention

  3.  Timeseries

  4.  Recruitment deviations

  5.  Recruitment bias adjustment

  6.  Spawner-recruit

  7.  Catch

  8.  SPR

  9.  Discards

  10. Mean weight

  11. Indices

  12. Numbers at age

  13. Length comp data (and generalized size comp data)

  14. Age comp data

  15. Conditional age-at-length data

  16. Length comp fits (and generalized size comp fits)

  17. Age comp fits

  18. Conditional age-at-length fits

  19. Francis and Punt conditional age-at-length comp fits

  20. Mean length-at-age and mean weight-at-age

  21. Tags

  22. Yield

  23. Movement

  24. Data range

  25. Parameter distributions

  26. Diagnostic tables

- pdf:

  Send plots to PDF file instead of R GUI?

- png:

  Send plots to PNG files instead of R GUI?

- html:

  Run [`SS_html()`](https://r4ss.github.io/r4ss/reference/SS_html.md) on
  completion? By default has same value as `png`.

- printfolder:

  The sub-directory under 'dir' (see below) in which the PNG files will
  be located. The default sub-directory is "plots". The directory will
  be created if it doesn\\t exist. If 'printfolder' is set to "", it is
  ignored and the PNG files will be located in the directory specified
  by 'dir'.

- dir:

  The directory in which a PDF file (if requested) will be created and
  within which the printfolder sub-directory (see above) will be created
  if png=TRUE. By default it will be the same directory that the report
  file was read from by the `SS_output` function. Alternatives to the
  default can be either relative (to the working directory) or absolute
  paths. The function will attempt to create the directory it doesn't
  exist, but it does not do so recursively.

- fleets:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing fleets or surveys to be included in the plot.

- areas:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing areas for which plots should be made in a multi-area model. By
  default, plots will be made for all areas (excepting cases where the
  function has not yet been updated for multi-area models).
  Default="all".

- fleetnames:

  Optional replacement for fleetnames used in data file.

- fleetcols:

  Either the string "default", or a vector of colors to use for each
  fleet. Default="default".

- fleetlty:

  Vector of line types used for each fleet in some plots. Default=1.

- fleetpch:

  Vector of point types used for each fleet in some plots. Default=1.

- lwd:

  Line width for plot elements.

- areacols:

  Optional vector of colors for each area if model has multiple areas.
  NULL value will be replaced by a default set of areas.

- areanames:

  Optional vector of names for each area used in titles.
  Default="default".

- verbose:

  A logical value specifying if output should be printed to the screen.

- uncertainty:

  Include values in plots showing estimates of uncertainty (requires
  positive definite hessian in model? Default=TRUE.

- forecastplot:

  Include forecast years in the timeseries plots and plots of
  time-varying quantities?

- datplot:

  Plot the data by itself? This is useful in document preparation, but
  doesn't change across alternative model runs with the same data, so
  can be committed to save time once the plots have been created once.
  Setting datplot=FALSE is equivalent to leaving off plots 15 and 16.
  Default=TRUE.

- Natageplot:

  Plot the expected numbers at age bubble plots and mean-age time
  series? Default=TRUE.

- samplesizeplots:

  Show sample size plots? Default=TRUE.

- compresidplots:

  Show residuals for composition plots?

- comp.yupper:

  Upper limit on ymax for polygon/histogram composition plots. This
  avoids scaling all plots to have max=1 if there is a vector with only
  a single observed fish in it. Default=0.4.

- sprtarg:

  Specify the F/SPR proxy target. Default=0.4.

- btarg:

  Target %unfished to be used in plots showing %unfished. May be omitted
  by setting to NA.

- minbthresh:

  Threshold depletion to be used in plots showing depletion. May be
  omitted by setting to NA.

- pntscalar:

  This scalar defines the maximum bubble size for bubble plots. This
  option is still available but a better choice is to use
  bub.scale.pearson and bub.scale.dat, which are allow the same scaling
  throughout all plots.

- bub.scale.pearson:

  Character expansion (cex) value for a proportion of 1.0 in bubble plot
  of Pearson residuals. Default=1.5.

- bub.scale.dat:

  Character expansion (cex) value for a proportion of 1.0 in bubble plot
  of composition data. Default=3.

- pntscalar.nums:

  This scalar defines the maximum bubble size for numbers-at-age and
  numbers-at-length plots.

- pntscalar.tags:

  This scalar defines the maximum bubble size for tagging plots.

- minnbubble:

  This defines the minimum number of years below which blank years will
  be added to bubble plots to avoid cropping. Default=8.

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

- aalresids:

  Plot the full set of conditional age-at-length Pearson residuals? Turn
  to FALSE if plots are taking too long and you don't want them.

- maxneff:

  The maximum value to include on plots of input and effective sample
  size. Occasionally a calculation of effective N blows up to very large
  numbers, rendering it impossible to observe the relationship for other
  data. Default=5000.

- cohortlines:

  Optional vector of birth years for cohorts for which to add growth
  curves to numbers at length bubble plots. Default=c().

- smooth:

  Add loess smoother to observed vs. expected index plots and input vs.
  effective sample size? Default=TRUE.

- showsampsize:

  Display sample sizes on composition plots? Default=TRUE.

- showeffN:

  Display effective sample sizes on composition plots? Default=TRUE.

- sampsizeline:

  show line for input sample sizes on top of conditional age-at-length
  plots (TRUE/FALSE, still in development)

- effNline:

  show line for effective sample sizes on top of conditional
  age-at-length plots (TRUE/FALSE, still in development)

- showlegend:

  Display legends in various plots?

- pwidth:

  Default width of plots printed to files in units of `punits`.

- pheight:

  Height of plots printed to png files in units of `punits`. Default is
  designed to allow two plots per page, with `pheight_tall` used for
  plots that work best with a taller format and a single plot per page.

- pheight_tall:

  Height of tall plots printed to png files in units of `punits`, where
  the tall plots are a subset of the plots which typically work best in
  a taller format.

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

- mainTitle:

  Logical indicating if a title should be included at the top (not yet
  implemented for all plots).

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

- selexlines:

  Vector controlling which lines should be shown on selectivity plots if
  the model includes retention. Default=1:5.

- rows:

  Number of rows to use for single panel plots. Default=1.

- cols:

  Number of columns to use for single panel plots. Default=1.

- maxrows:

  Maximum number of rows to for multi-panel plots.

- maxcols:

  Maximum number of columns for multi-panel plots.

- maxrows2:

  Maximum number of rows for conditional age-at-length multi-panel
  plots.

- maxcols2:

  Maximum number of rows for conditional age-at-length multi-panel
  plots.

- andrerows:

  Number of rows of Andre's conditional age-at-length plots within each
  page.

- tagrows:

  Number of rows for tagging-related plots.

- tagcols:

  Number of columns for tagging-related plots.

- parrows:

  Number of rows for parameter distribution plots.

- parcols:

  Number of columns for parameter distribution plots.

- fixdims:

  Control whether multi-panel plots all have dimensions equal to maxrows
  by maxcols, or resized within those limits to fit number of plots.
  Default=TRUE.

- new:

  Open a new window or add to existing plot windows. Default=TRUE.

- SSplotDatMargin:

  Size of right-hand margin in data plot (may be too small if fleet
  names are long)

- filenotes:

  Optional vector of character strings to be added to intro HTML page
  (if created) with notes about the model.

- catchasnumbers:

  Is catch input in numbers instead of biomass? Default=F.

- catchbars:

  show catch by fleet as barplot instead of stacked polygons
  (default=TRUE)

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- minyr:

  First year to show in time-series and time-varying plots

- maxyr:

  Last year to show in time-series and time-varying plots. This can
  either be an alternative to, or redundant with, the forecastplot
  input.

- sexes:

  Which sexes to show in composition plots. Default="all".

- scalebins:

  Rescale expected and observed proportions in composition plots by
  dividing by bin width for models where bins have different widths?
  Caution!: May not work correctly in all cases.

- scalebubbles:

  scale data-only bubbles by sample size, not just proportion within
  sample? Default=FALSE.

- tslabels:

  Either NULL to have default labels for timeseries plots or a vector of
  appropriate length with labels for each figure

- catlabels:

  Either NULL to have default labels for catch plots or a vector of
  appropriate length with labels for each figure

- maxsize:

  The size of the largest bubble in the datasize plot. Default is 1.0.

- showmle:

  Show MLE estimate and asymptotic variance estimate with blue lines in
  the parameter distribution plots?

- showpost:

  Show posterior distribution as bar graph in parameter distribution
  plots (requires MCMC results to be available in `replist`)?

- showprior:

  Show prior distribution as black line in the parameter distribution
  plots?

- showinit:

  Show initial value as red triangle in the parameter distribution
  plots?

- showdev:

  Include devs in the parameter distribution plots?

- fitrange:

  Fit range in parameter distribution plots tightly around MLE and
  posterior distributions instead of full parameter range?

- ...:

  Additional arguments that will be passed to some subfunctions.

## References

Walters, Hilborn, and Christensen, 2008, Surplus production dynamics in
declining and recovering fish populations. Can. J. Fish. Aquat. Sci. 65:
2536-2551.

## See also

[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md),
[`SSplotBiology()`](https://r4ss.github.io/r4ss/reference/SSplotBiology.md),
[`SSplotCatch()`](https://r4ss.github.io/r4ss/reference/SSplotCatch.md),
[`SSplotComps()`](https://r4ss.github.io/r4ss/reference/SSplotComps.md),
[`SSplotDiscard()`](https://r4ss.github.io/r4ss/reference/SSplotDiscard.md),
[`SSplotIndices()`](https://r4ss.github.io/r4ss/reference/SSplotIndices.md),
[`SSplotMnwt()`](https://r4ss.github.io/r4ss/reference/SSplotMnwt.md),
[`SSplotNumbers()`](https://r4ss.github.io/r4ss/reference/SSplotNumbers.md),
[`SSplotRecdevs()`](https://r4ss.github.io/r4ss/reference/SSplotRecdevs.md),
[`SSplotSelex()`](https://r4ss.github.io/r4ss/reference/SSplotSelex.md),
[`SSplotSpawnrecruit()`](https://r4ss.github.io/r4ss/reference/SSplotSpawnrecruit.md),
[`SSplotSPR()`](https://r4ss.github.io/r4ss/reference/SSplotSPR.md),
[`SSplotTags()`](https://r4ss.github.io/r4ss/reference/SSplotTags.md),
[`SSplotTimeseries()`](https://r4ss.github.io/r4ss/reference/SSplotTimeseries.md),
[`SSplotYield()`](https://r4ss.github.io/r4ss/reference/SSplotYield.md)

## Author

Ian Stewart, Ian Taylor
