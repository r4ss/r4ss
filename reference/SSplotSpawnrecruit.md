# Plot spawner-recruit curve.

Plot spawner-recruit curve based on output from Stock Synthesis model.

## Usage

``` r
SSplotSpawnrecruit(
  replist,
  subplots = 1:3,
  add = FALSE,
  plot = TRUE,
  print = FALSE,
  xlim = NULL,
  ylim = NULL,
  labels = c("Spawning biomass (t)", "Recruitment (1,000s)",
    replist[["SpawnOutputLabel"]], expression(paste("Spawning output (relative to ",
    italic(B)[0], ")")), expression(paste("Recruitment (relative to  ", italic(R)[0],
    ")")), "Log recruitment deviation"),
  bioscale = 1,
  plotdir = "default",
  pwidth = 6.5,
  pheight = 6.5,
  punits = "in",
  res = 300,
  ptsize = 10,
  verbose = TRUE,
  colvec = c("blue", "black", "black", gray(0, 0.7)),
  ltyvec = c(1, 2, 1, NA),
  ptcol = "default",
  legend = TRUE,
  legendloc = NULL,
  minyr = "default",
  textmindev = 0.5,
  relative = FALSE,
  expected = TRUE,
  estimated = TRUE,
  bias_adjusted = TRUE,
  show_env = TRUE,
  virg = TRUE,
  init = TRUE,
  forecast = FALSE,
  subplot = lifecycle::deprecated()
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- subplots:

  Vector of which subplots to show. 1=plot without labels, 2=plot with
  year labels.

- add:

  add to existing plot?

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- xlim:

  optional control of x range

- ylim:

  optional control of y range

- labels:

  vector containing x-axis label for models with spawning biomass in
  metric tons, y-axis label, and alternative x-axis for models with a
  fecundity relationship making spawning output not equal to spawning
  biomass.

- bioscale:

  scaling for spawning biomass. Default = 1. Previously this was set to
  0.5 for single-sex models, and 1.0 for all others, but now single-sex
  models are assumed to use the -1 option for Nsexes in the data file so
  the scaling is done automatically by SS3.

- plotdir:

  Directory where PNG files will be written.

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

- verbose:

  A logical value specifying if output should be printed to the screen.

- colvec:

  vector of length 4 with colors for 3 lines and 1 set of points (where
  the 4th value for the points is the color of the circle around the
  background color provided by `ptcol`

- ltyvec:

  vector of length 4 with line types for the 3 lines and 1 set of
  points, where the points are disconnected (lty=NA) by default

- ptcol:

  vector or single value for the color of the points, "default" will by
  replaced by a vector of colors of length equal to
  `nrow(replist[["recruit"]])`

- legend:

  Add a legend?

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- minyr:

  minimum year of recruitment deviation to show in plot

- textmindev:

  minimum recruitment deviation for label to be added so only extreme
  devs are labeled (labels are added to first and last years as well).
  Default=0.7.

- relative:

  scale both axes so that B0 and R0 are at 1 to show spawning output and
  recruitment relative to the equilibrium

- expected:

  show line for expected recruitment (stock-recruit curve)

- estimated:

  show points for estimated recruitment values (including deviations)

- bias_adjusted:

  show lines for bias adjusted expected recruitment

- show_env:

  add line for expected recruitment with environmental variability

- virg:

  add point for equilibrium conditions (x=B0,y=R0)

- init:

  add point for initial conditions (x=B1,y=R1), only appears if this
  point differs from virgin values

- forecast:

  include forecast years in the curve?

- subplot:

  Deprecated - use subplots.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian Stewart, Ian Taylor
