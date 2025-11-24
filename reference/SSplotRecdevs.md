# Plot recruitment deviations

Plot recruitment deviations and associated quantities including derived
measures related to bias adjustment.

## Usage

``` r
SSplotRecdevs(
  replist,
  subplots = 1:3,
  plot = TRUE,
  print = FALSE,
  add = FALSE,
  uncertainty = TRUE,
  minyr = -Inf,
  maxyr = Inf,
  forecastplot = FALSE,
  col1 = "black",
  col2 = "blue",
  col3 = "green3",
  col4 = "red",
  legendloc = "topleft",
  labels = c("Year", "Asymptotic standard error estimate", "Log recruitment deviation",
    "Bias adjustment fraction, 1 - stddev^2 / sigmaR^2"),
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

  vector controlling which subplots to create

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- add:

  add to existing plot (not yet implemented)

- uncertainty:

  include plots showing uncertainty?

- minyr:

  optional input for minimum year to show in plots

- maxyr:

  optional input for maximum year to show in plots

- forecastplot:

  include points from forecast years?

- col1:

  first color used

- col2:

  second color used

- col3:

  third color used

- col4:

  fourth color used

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

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

- plotdir:

  Directory where PNG files will be written.

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_fitbiasramp()`](https://r4ss.github.io/r4ss/reference/SS_fitbiasramp.md)

## Author

Ian Taylor, Ian Stewart
