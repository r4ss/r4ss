# Plot timeseries data

Plot timeseries data contained in TIME_SERIES output from Stock
Synthesis report file. Some values have optional uncertainty intervals.

## Usage

``` r
SSplotTimeseries(
  replist,
  subplot,
  add = FALSE,
  areas = "all",
  areacols = NULL,
  areanames = "default",
  forecastplot = TRUE,
  uncertainty = TRUE,
  bioscale = 1,
  minyr = -Inf,
  maxyr = Inf,
  plot = TRUE,
  print = FALSE,
  plotdir = "default",
  verbose = TRUE,
  btarg = "default",
  minbthresh = "default",
  xlab = "Year",
  labels = NULL,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  mainTitle = FALSE,
  mar = NULL
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- subplot:

  number controlling which subplot to create Numbering of subplots is as
  follows, where the spawning biomass plots (7 to 10) are provided first
  when this function is called by
  [`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md):

  - 1 Total biomass (t) with forecast

  - 2 Total biomass by area (spatial models only)

  - 3 Total biomass (t) at beginning of spawning season with forecast

  - 4 Summary biomass (t) with forecast

  - 5 Summary biomass (t) by area (spatial models only)

  - 6 Summary biomass (t) at beginning of season 1 with forecast

  - 7 Spawning output with forecast with ~95% asymptotic intervals

  - 8 Spawning output by area (spatial models only)

  - 9 Fraction of unfished spawning output with forecast with ~95%
    asymptotic intervals

  - 10 Fraction of unfished spawning output by area (spatial models
    only)

  - 11 Age-0 recruits (1,000s) with forecast with ~95% asymptotic
    intervals

  - 12 Age-0 recruits by area (spatial models only)

  - 13 Fraction of recruits by area (spatial models only)

  - 14 Age-0 recruits (1,000s) by birth season with forecast

  - 15 Fraction of total Age-0 recruits by birth season with forecast

- add:

  add to existing plot? (not yet implemented)

- areas:

  optional subset of areas to plot for spatial models

- areacols:

  Optional vector of colors for each area if model has multiple areas.
  NULL value will be replaced by a default set of areas.

- areanames:

  names for areas. Default is to use Area1, Area2,...

- forecastplot:

  add points from forecast years

- uncertainty:

  add intervals around quantities for which uncertainty is available

- bioscale:

  scaling for spawning biomass. Default = 1. Previously this was set to
  0.5 for single-sex models, and 1.0 for all others, but now single-sex
  models are assumed to use the -1 option for Nsexes in the data file so
  the scaling is done automatically by SS3.

- minyr:

  optional input for minimum year to show in plots

- maxyr:

  optional input for maximum year to show in plots

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- plotdir:

  Directory where PNG files will be written.

- verbose:

  A logical value specifying if output should be printed to the screen.

- btarg:

  Target depletion to be used in plots showing depletion. May be omitted
  by setting to 0. "default" chooses value based on modeloutput.

- minbthresh:

  Threshold depletion to be used in plots showing depletion. May be
  omitted by setting to 0. "default" assumes 0.25 unless btarg in model
  output is 0.25 in which case minbthresh = 0.125 (U.S. west coast
  flatfish).

- xlab:

  x axis label for all plots

- labels:

  Vector of labels for plots (titles and axis labels).

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

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

- mainTitle:

  Logical indicating if a title should be included at the top (not yet
  implemented for all plots).

- mar:

  Either NULL to allow the default (which depends on whether the main
  title is included or not) or a numerical vector of the form c(bottom,
  left, top, right) which gives the number of lines of margin to be
  specified on the four sides of the plot, which is passed to
  [`par()`](https://rdrr.io/r/graphics/par.html).

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian Taylor, Ian Stewart
