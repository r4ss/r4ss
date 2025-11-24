# Plot mean weight data and fits.

Plot mean weight data and fits from Stock Synthesis output. Intervals
are based on T-distributions as specified in model.

## Usage

``` r
SSplotMnwt(
  replist,
  subplots = 1:2,
  ymax = NULL,
  plot = TRUE,
  print = FALSE,
  fleets = "all",
  fleetnames = "default",
  datplot = FALSE,
  labels = c("Year", "discard", "retained catch", "whole catch",
    "Mean individual body weight (kg)", "Mean weight in", "for"),
  col1 = "blue",
  col2 = "black",
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  plotdir = "default",
  verbose = TRUE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- subplots:

  Vector of which plots to make (1 = data only, 2 = with fit). If
  `plotdat = FALSE` then subplot 1 is not created, regardless of choice
  of `subplots`.

- ymax:

  Optional input to override default ymax value.

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- fleets:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing fleets or surveys to be included in the plot.

- fleetnames:

  Optional replacement for fleetnames used in data file.

- datplot:

  Make data-only plot of discards? This can override the choice of
  `subplots`.

- labels:

  Vector of labels for plots (titles and axis labels).

- col1:

  first color to use in plot (for expected values)

- col2:

  second color to use in plot (for observations and intervals)

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

- plotdir:

  Directory where PNG files will be written.

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian Taylor, Ian Stewart
