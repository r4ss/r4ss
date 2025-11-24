# Plot Spawning Potential Ratio (SPR) quantities.

Plot time series of SPR, 1-SPR, the chosen SPR ratio and the phase plot.

## Usage

``` r
SSplotSPR(
  replist,
  add = FALSE,
  plot = TRUE,
  print = FALSE,
  uncertainty = TRUE,
  subplots = 1:4,
  forecastplot = FALSE,
  col1 = "black",
  col2 = "blue",
  col3 = "green3",
  col4 = "red",
  sprtarg = "default",
  btarg = "default",
  minbthresh = "default",
  labels = c("Year", "SPR", "1-SPR", "Relative fishing intensity",
    "Fraction of unfished spawning output"),
  pwidth = 6.5,
  pheight = 5,
  pheight_tall = 5,
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

- add:

  add to existing plot (not yet implemented)

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- uncertainty:

  include plots showing uncertainty?

- subplots:

  vector controlling which subplots to create Numbering of subplots is
  as follows:

  1.  timeseries of SPR,

  2.  timeseries of 1 - SPR,

  3.  timeseries of SPR ratio (as specified in the starter file), and

  4.  phase plot of Biomass ratio vs SPR ratio (as specified in the
      starter file).

- forecastplot:

  Include forecast years in plot?

- col1:

  first color used

- col2:

  second color used

- col3:

  third color used

- col4:

  fourth color used

- sprtarg:

  F/SPR proxy target. "default" chooses based on model output, where
  models which have SPR_std_basis = 0 or 1 specified in the starter file
  will use the SPR target specified in the forecast file. Models which
  have SPR_std_basis = 2 will use SPR at MSY for the SPR target and
  models which have the SPR_std_basis = 3 will use SPR at Btarget for
  the SPR target in these plots. Zero or negative values of sprtarg
  input here will cause no horizontal line to be plotted.

- btarg:

  target depletion to be used in plots showing depletion. May be omitted
  by setting to NA. "default" chooses based on model output.

- minbthresh:

  minimum biomass threshold to be used in plots showing depletion. May
  be omitted by setting to NA. "default" chooses based on model output.

- labels:

  Vector of labels for plots (titles and axis labels).

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

Ian Stewart, Ian Taylor
