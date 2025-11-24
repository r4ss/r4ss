# plot model comparisons

Creates a user-chosen set of plots comparing model output from a summary
of multiple models, where the collection was created using the
`SSsummarize` function.

## Usage

``` r
SSplotComparisons(
  summaryoutput,
  subplots = 1:20,
  plot = TRUE,
  print = FALSE,
  png = print,
  pdf = FALSE,
  models = "all",
  endyrvec = NULL,
  indexfleets = NULL,
  indexUncertainty = TRUE,
  indexQlabel = TRUE,
  indexQdigits = 4,
  indexSEvec = NULL,
  indexPlotEach = FALSE,
  labels = c("Year", "Spawning biomass (t)", "Fraction of unfished spawning biomass",
    "Age-0 recruits (1,000s)", "Recruitment deviations", "Index", "Log index",
    "SPR-related quantity", "Density", "Management target",
    "Minimum stock size threshold", "Spawning output", "Harvest rate",
    "Summary biomass (t)", "Age X+ biomass (t)"),
  col = NULL,
  shadecol = NULL,
  pch = NULL,
  lty = 1,
  lwd = 2,
  spacepoints = 10,
  staggerpoints = 1,
  initpoint = 0,
  tickEndYr = TRUE,
  shadeForecast = TRUE,
  xlim = NULL,
  ylimAdj = 1.05,
  xaxs = "i",
  yaxs = "i",
  type = "o",
  uncertainty = TRUE,
  shadealpha = 0.1,
  legend = TRUE,
  legendlabels = summaryoutput[["modelnames"]],
  legendloc = "topright",
  legendorder = NULL,
  legendncol = 1,
  sprtarg = NULL,
  btarg = NULL,
  minbthresh = NULL,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  plotdir = NULL,
  filenameprefix = "",
  densitynames = c("SSB_Virgin", "SR_LN(R0)"),
  densityxlabs = NULL,
  rescale = TRUE,
  densityscalex = 1,
  densityscaley = 1,
  densityadjust = 1,
  densitysymbols = TRUE,
  densitytails = TRUE,
  densitymiddle = FALSE,
  densitylwd = 1,
  fix0 = TRUE,
  new = TRUE,
  add = FALSE,
  par = list(mar = c(5, 4, 1, 1) + 0.1),
  verbose = TRUE,
  mcmcVec = FALSE,
  show_equilibrium = TRUE,
  ...
)
```

## Arguments

- summaryoutput:

  List created by `SSsummarize`

- subplots:

  Vector of subplots to be created Numbering of subplots is as follows:

  - 1 spawning biomass

  - 2 spawning biomass with uncertainty intervals

  - 3 biomass ratio (hopefully equal to fraction of unfished)

  - 4 biomass ratio with uncertainty

  - 18 summary biomass

  - 19 summary biomass with uncertainty

  - 5 SPR ratio

  - 6 SPR ratio with uncertainty

  - 7 F value

  - 8 F value with uncertainty

  - 9 recruits

  - 10 recruits with uncertainty

  - 11 recruit devs

  - 12 recruit devs with uncertainty

  - 13 index fits

  - 14 index fits on a log scale

  - 15 phase plot

  - 16 densities

  - 17 cumulative densities

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- png:

  Has same result as `print`, included for consistency with `SS_plots`.

- pdf:

  Write output to PDF file? Can't be used in conjunction with `png` or
  `print`.

- models:

  Optional subset of the models described in `summaryoutput`. Either
  "all" or a vector of numbers indicating columns in summary tables.

- endyrvec:

  Optional single year or vector of years representing the final year of
  values to show for each model. By default it is set to the ending year
  specified in each model. If the number of models is subset using the
  `models` input then `endyr` needs to be shortened as well.

- indexfleets:

  Fleet numbers for each model to compare indices of abundance. Can take
  different forms:

  - NULL: (default) create a separate plot for each index as long as the
    fleet numbering is the same across all models.

  - integer: create a single comparison plot for the chosen index

  - vector of length equal to number of models: a single fleet number
    for each model to be compared in a single plot

  - list: list of fleet numbers associated with indices within each
    model to be compared, where the list elements are each a vector of
    the same length but the names of the list elements don't matter and
    can be absent.

- indexUncertainty:

  Show uncertainty intervals on index data? Default=FALSE because if
  models have any extra standard deviations added, these intervals may
  differ across models.

- indexQlabel:

  Add catchability to legend in plot of index fits (TRUE/FALSE)?

- indexQdigits:

  Number of significant digits for catchability in legend (if
  `indexQlabel = TRUE`)

- indexSEvec:

  Optional replacement for the SE values in `summaryoutput[["indices"]]`
  to deal with the issue of differing uncertainty by models described
  above.

- indexPlotEach:

  TRUE plots the observed index for each model with colors, or FALSE
  just plots observed once in black dots.

- labels:

  Vector of labels for plots (titles and axis labels).

- col:

  Optional vector of colors to be used for lines. Input NULL makes use
  of `rich.colors.short` function.

- shadecol:

  Optional vector of colors to be used for shading uncertainty
  intervals. The default (NULL) is to use the same colors provided by
  `col` (either the default or a user-chosen input) and make them more
  transparent by applying the `shadealpha` input as an alpha
  transparency value (using the
  [`adjustcolor()`](https://rdrr.io/r/grDevices/adjustcolor.html)
  function)

- pch:

  Optional vector of plot character values

- lty:

  Optional vector of line types

- lwd:

  Optional vector of line widths

- spacepoints:

  Number of years between points shown on top of lines (for long
  timeseries, points every year get mashed together)

- staggerpoints:

  Number of years to stagger the first point (if `spacepoints > 1`) for
  each line (so that adjacent lines have points in different years)

- initpoint:

  Year value for first point to be added to lines. Points added to plots
  are those that satisfy (Yr-initpoint)%%spacepoints ==
  (staggerpoints\*iline)%%spacepoints

- tickEndYr:

  TRUE/FALSE switch to turn on/off extra axis mark at final year in
  timeseries plots.

- shadeForecast:

  TRUE/FALSE switch to turn on off shading of years beyond the maximum
  ending year of the models

- xlim:

  Optional x limits

- ylimAdj:

  Multiplier for ylim parameter. Allows additional white space to fit
  legend if necessary. Default=1.05.

- xaxs:

  Choice of xaxs parameter (see ?par for more info)

- yaxs:

  Choice of yaxs parameter (see ?par for more info)

- type:

  Type parameter passed to points (default 'o' overplots points on top
  of lines)

- uncertainty:

  Show plots with uncertainty intervals? Either a single TRUE/FALSE
  value, or a vector of TRUE/FALSE values for each model, or a set of
  integers corresponding to the choice of models.

- shadealpha:

  Transparency adjustment used to make default shadecol values
  (implemented as `adjustcolor(col=col, alpha.f=shadealpha)`)

- legend:

  Add a legend?

- legendlabels:

  Optional vector of labels to include in legend. Default is
  `summaryoutput[["modelnames"]]`.

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- legendorder:

  Optional vector of model numbers that can be used to have the legend
  display the model names in an order that is different than that which
  is represented in the summary input object.

- legendncol:

  Number of columns for the legend.

- sprtarg:

  Target value for SPR-ratio where line is drawn in the SPR plots and
  phase plot.

- btarg:

  Target biomass value at which to show a line (set to 0 to remove)

- minbthresh:

  Minimum biomass threshold at which to show a line (set to 0 to remove)

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

- res:

  Resolution of plots printed to files. The default is `res = 300`.

- ptsize:

  Point size for plotted text in plots printed to files (see
  [`help("png")`](https://rdrr.io/r/grDevices/png.html) in R for
  details).

- plotdir:

  Directory where PNG files will be written.

- filenameprefix:

  Additional text to append to PNG or PDF file names. It will be
  separated from default name by an underscore.

- densitynames:

  Vector of names (or subset of names) of parameters or derived
  quantities contained in `summaryoutput[["pars"]][["Label"]]` or
  `summaryoutput[["quants"]][["Label"]]` for which to make density plots

- densityxlabs:

  Optional vector of x-axis labels to use in the density plots (must be
  equal in length to the printed vector of quantities that match the
  `densitynames` input)

- rescale:

  TRUE/FALSE control of automatic rescaling of units into thousands,
  millions, or billions

- densityscalex:

  Scalar for upper x-limit in density plots (values below 1 will cut off
  the right tail to provide better contrast among narrower distributions

- densityscaley:

  Scalar for upper y-limit in density plots (values below 1 will cut off
  top of highest peaks to provide better contrast among broader
  distributions

- densityadjust:

  Multiplier on bandwidth of kernel in density function used for
  smoothing MCMC posteriors. See 'adjust' in ?density for details.

- densitysymbols:

  Add symbols along lines in density plots. Quantiles are
  `c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)`.

- densitytails:

  Shade tails outside of 95% interval darker in density plots?

- densitymiddle:

  Shade middle inside of 95% interval darker in density plots?

- densitylwd:

  Line width for density plots

- fix0:

  Always include 0 in the density plots?

- new:

  Create new empty plot window

- add:

  Allows single plot to be added to existing figure. This needs to be
  combined with specific 'subplots' input to make sure only one thing
  gets added.

- par:

  list of graphics parameter values passed to the `par` function

- verbose:

  A logical value specifying if output should be printed to the screen.

- mcmcVec:

  Vector of TRUE/FALSE values (or single value) indicating whether input
  values are from MCMC or to use normal distribution around MLE

- show_equilibrium:

  Whether to show the equilibrium values for SSB. For some model
  comparisons, these might not be comparable and thus useful to turn
  off. Defaults to TRUE.

- ...:

  Additional arguments passed to
  [`add_legend()`](https://r4ss.github.io/r4ss/reference/add_legend.md),
  which can include `legendcex`, `legendsp`, and `pt.cex`.

## See also

Other model comparison functions:
[`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md),
[`SSsummarize()`](https://r4ss.github.io/r4ss/reference/SSsummarize.md),
[`SStableComparisons()`](https://r4ss.github.io/r4ss/reference/SStableComparisons.md)

## Author

Ian G. Taylor, John R. Wallace

## Examples

``` r
if (FALSE) { # \dontrun{
# directories where models were run need to be defined
dir1 <- "c:/SS/mod1"
dir2 <- "c:/SS/mod2"

# read two models
mod1 <- SS_output(dir = dir1)
mod2 <- SS_output(dir = dir2)

# create list summarizing model results
mod.sum <- SSsummarize(list(mod1, mod2))

# plot comparisons
SSplotComparisons(mod.sum, legendlabels = c("First model", "Second model"))

# Example showing comparison of MLE to MCMC results where the mcmc would have
# been run in the subdirectory 'c:/SS/mod1/mcmc'
mod1 <- SS_output(dir = "c:/SS/mod1", dir.mcmc = "mcmc")
# pass the same model twice to SSsummarize in order to plot it twice
mod.sum <- SSsummarize(list(mod1, mod1))
# compare MLE to MCMC
SSplotComparisons(mod.sum,
  legendlabels = c("MCMC", "MLE"),
  mcmcVec = c(TRUE, FALSE)
)
} # }
```
