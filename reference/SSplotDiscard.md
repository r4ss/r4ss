# Plot fit to discard fraction.

Plot fit to discard fraction from Stock Synthesis output file.

## Usage

``` r
SSplotDiscard(
  replist,
  subplots = 1:4,
  plot = TRUE,
  print = FALSE,
  plotdir = "default",
  fleets = "all",
  fleetnames = "default",
  datplot = FALSE,
  labels = c("Year", "Discard fraction", "Total discards", "for"),
  yhi = 1,
  ymax = NULL,
  col1 = "blue",
  col2 = "black",
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  verbose = TRUE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- subplots:

  Vector of which plots to make

  - 1 data only

  - 2 data with fit

  - 3 data only (log scale)

  - 4 data with fit (log scale)

  If `plotdat = FALSE` then subplots 1 and 3 are not created, regardless
  of choice of `subplots`.

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- plotdir:

  Directory where PNG files will be written.

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

- yhi:

  Maximum y-value which will always be included in the plot (all data
  included regardless). Default = 1 so that discard fractions are always
  plotted on a 0-1 range, but total discard amounts which are greater
  than this value will exceed it.

- ymax:

  Optional maximum y-value to include (useful if upper tails on discard
  amounts are very high)

- col1:

  First color to use in plot (for expected values)

- col2:

  Second color to use in plot (for observations and intervals)

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

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md)

## Author

Ian G. Taylor, Ian J. Stewart, Robbie L. Emmet
