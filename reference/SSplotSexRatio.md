# Plot sex-ratio data and fits for two sex models

Plot sex-ratio data and fits from Stock Synthesis output. Multi-figure
plots depend on `make_multifig`. The confidence intervals around the
observed points are based on a Jeffreys interval calculated from the
adjusted input sample size (with a floor of 1).

## Usage

``` r
SSplotSexRatio(
  replist,
  kind = "AGE",
  sexratio.option = 2,
  CI = 0.75,
  plot = TRUE,
  print = FALSE,
  fleets = "all",
  fleetnames = "default",
  yupper = 4,
  datonly = FALSE,
  linescol = rgb(0.6, 0, 0.9, 0.7),
  lwd = 2,
  showsampsize = TRUE,
  showeffN = TRUE,
  axis1 = NULL,
  axis2 = NULL,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  ptsize = 10,
  res = 300,
  plotdir = "default",
  cex.main = 1,
  labels = c("Length (cm)", "Age (yr)", "Sex ratio (females:males)", "Fraction female"),
  maxrows = 6,
  maxcols = 6,
  rows = 1,
  cols = 1,
  fixdims = TRUE,
  verbose = TRUE,
  mainTitle = FALSE,
  ...
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- kind:

  indicator of type of plot can be "LEN", "SIZE", "AGE", "cond",
  "GSTAGE", "L@A", or "W@A".

- sexratio.option:

  code to choose among (1) female:male ratio or (2) fraction females out
  of the total

- CI:

  confidence interval for uncertainty

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- fleets:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing fleets or surveys to be included in the plot.

- fleetnames:

  Optional replacement for fleetnames used in data file.

- yupper:

  upper limit on ymax (only applies for sexratio.option == 1)

- datonly:

  make plots of data without fits?

- linescol:

  Color for line showing expected value (default is purple).

- lwd:

  Line width for plot elements.

- showsampsize:

  add sample sizes to plot

- showeffN:

  add effective sample sizes to plot

- axis1:

  position of bottom axis values

- axis2:

  position of left size axis values

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

- ptsize:

  Point size for plotted text in plots printed to files (see
  [`help("png")`](https://rdrr.io/r/grDevices/png.html) in R for
  details).

- res:

  Resolution of plots printed to files. The default is `res = 300`.

- plotdir:

  Directory where PNG files will be written.

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

- labels:

  Vector of labels for plots (titles and axis labels).

- maxrows:

  maximum (or fixed) number or rows of panels in the plot

- maxcols:

  maximum (or fixed) number or columns of panels in the plot plots

- rows:

  number or rows to return to as default for next plots to come or for
  single plots

- cols:

  number or cols to return to as default for next plots to come or for
  single plots

- fixdims:

  fix the dimensions at maxrows by maxcols or resize based on number of
  years of data

- verbose:

  A logical value specifying if output should be printed to the screen.

- mainTitle:

  Logical indicating if a title should be included at the top (not yet
  implemented for all plots).

- ...:

  additional arguments that will be passed to the plotting.

## References

Brown, L.; Cai, T. Tony; DasGupta, A. (2001). Interval Estimation for a
Binomial Proportion. Statistical Science. 16(2): 101-133.
http://www.jstor.org/stable/2676784.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`make_multifig_sexratio()`](https://r4ss.github.io/r4ss/reference/make_multifig_sexratio.md)

## Author

Cole Monnahan, Ian Taylor
