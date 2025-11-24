# Plot indices of abundance and associated quantities.

Plot indices of abundance with or without model fit as well as other
diagnostic plots such as observed vs. expected index and plots related
to time-varying catchability (if present).

## Usage

``` r
SSplotIndices(
  replist,
  subplots = c(1:10, 12),
  plot = TRUE,
  print = FALSE,
  fleets = "all",
  fleetnames = "default",
  smooth = TRUE,
  add = FALSE,
  datplot = TRUE,
  labels = c("Year", "Index", "Observed index", "Expected index", "Log index",
    "Log observed index", "Log expected index", "Standardized index", "Catchability (Q)",
    "Time-varying catchability", "Vulnerable biomass",
    "Catchability vs. vulnerable biomass", "Residual", "Deviation"),
  fleetcols = NULL,
  col1 = "default",
  col2 = "default",
  col3 = "blue",
  col4 = "red",
  pch1 = 21,
  pch2 = 16,
  cex = 1,
  bg = "white",
  legend = TRUE,
  legendloc = "topright",
  seasnames = NULL,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  mainTitle = FALSE,
  plotdir = "default",
  minyr = NULL,
  maxyr = NULL,
  maximum_ymax_ratio = Inf,
  show_input_uncertainty = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- subplots:

  vector controlling which subplots to create Numbering of subplots is
  as follows, where subplot 9 (comparison of all indices) is provided
  first:

  - 1 index data by fleet

  - 2 index data with fit by fleet

  - 3 observed vs expected index values with smoother

  - 4 index data by fleet on a log scale (lognormal error only)

  - 5 index data with fit by fleet on a log scale (lognormal error only)

  - 6 log(observed) vs log(expected) with smoother (lognormal error
    only)

  - 7 time series of time-varying catchability (only if actually
    time-varying)

  - 8 catchability vs. vulnerable biomass (if catchability is not
    constant)

  - 9 comparison of all indices

  - 10 index residuals based on total uncertainty

  - 11 index residuals based on input uncertainty (not currently
    provided)

  - 12 index deviations (independent of index uncertainty)

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- fleets:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing fleets or surveys to be included in the plot.

- fleetnames:

  Optional replacement for fleetnames used in data file.

- smooth:

  add smoothed line to plots of observed vs. expected sample sizes

- add:

  add to existing plot (not yet implemented)

- datplot:

  make plot of data only?

- labels:

  Vector of labels for plots (titles and axis labels).

- fleetcols:

  vector of colors for all fleets (including those with no index data)

- col1:

  vector of colors for points in each season for time series plot.
  Default is red for single season models and a rainbow using the
  rich.colors.short function for multiple seasons.

- col2:

  vector of colors for points in each season for obs. vs. exp. plot.
  Default is blue for single season models and a rainbow using the
  rich.colors.short function for multiple seasons.

- col3:

  color of line showing expected index in time series plot. Default is
  blue.

- col4:

  color of smoother shown in obs. vs. exp. plots. Default is red.

- pch1:

  single value or vector of plotting characters (pch parameter) for
  time-series plots of index fit. Default=21.

- pch2:

  single value or vector of plotting characters (pch parameter) for
  sample size plots of index fit. Default=16.

- cex:

  character expansion factor for points showing observed values.
  Default=1.

- bg:

  Background color for points with pch=21.

- legend:

  add a legend to seasonal colors (only for seasonal models)

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- seasnames:

  optional vector of names for each season to replace defaults if a
  legend is used

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

- plotdir:

  Directory where PNG files will be written.

- minyr:

  First year to show in plot (for zooming in on a subset of values)

- maxyr:

  Last year to show in plot (for zooming in on a subset of values)

- maximum_ymax_ratio:

  Maximum allowed value for ymax (specified as ratio of y), which
  overrides any value of ymax that is greater (default = Inf)

- show_input_uncertainty:

  Switch controlling whether to add thicker uncertainty interval lines
  indicating the input uncertainty relative to the total uncertainty
  which may result from estimating a parameter for extra standard
  deviations. This is only added for the plots with index fit included
  (the data-only plots only show the input uncertainty).

- verbose:

  A logical value specifying if output should be printed to the screen.

- ...:

  Extra arguments to pass to calls to `plot`

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian Stewart, Ian Taylor, James Thorson
