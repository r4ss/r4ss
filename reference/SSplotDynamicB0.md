# Plot Dynamic B0

Plots the spawning output with and without fishing mortality

## Usage

``` r
SSplotDynamicB0(
  replist,
  ylab = "Spawning biomass (t)",
  equilibrium = TRUE,
  forecast = FALSE,
  yrs = "all",
  plot = TRUE,
  print = FALSE,
  plotdir = "default",
  verbose = TRUE,
  uncertainty = TRUE,
  legend = TRUE,
  legendlabels = c("equilibrium", "without fishing", "with fishing"),
  legendloc = "bottom",
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  add = FALSE,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  mainTitle = FALSE,
  mar = NULL
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- ylab:

  Y-axis label. Default is "Spawning biomass (t)" which is replaced by
  `replist[["SpawnOutputLabel"]]` for models with
  `replist[["SpawnOutputUnits"]] == "numbers"`

- equilibrium:

  Show equilibrium in plot? Applies whether "yrs" is specified or not.

- forecast:

  Show forecast years in plot? Only applies if yrs = "all".

- yrs:

  Which years to include. Default "all" will show startyr to endyr + 1
  modified by the arguments `forecast`.

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- plotdir:

  Directory where PNG files will be written.

- verbose:

  A logical value specifying if output should be printed to the screen.

- uncertainty:

  Show 95% uncertainty intervals around point estimates? These intervals
  will only appear when uncertainty in the dynamic B0 estimates is
  available via the control file settings for "read specs for more
  stddev reporting".

- legend:

  Add a legend?

- legendlabels:

  Character vector with labels for the unfished equilibrium point (if
  `equilibrium = TRUE`) and the two lines showing spawning biomass or
  output without and with fishing.

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- col:

  Optional vector of colors to be used for the two lines (single value
  will apply to both lines).

- lty:

  Optional vector of line types to be used for the two lines (single
  value will apply to both lines).

- lwd:

  Optional vector of line widths to be used for the two lines. Single
  value will apply to both lines.

- add:

  add to existing plot

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

[`SSplotTimeseries()`](https://r4ss.github.io/r4ss/reference/SSplotTimeseries.md)

## Author

Ian G. Taylor
