# Plot tagging data and fits

Plot observed and expected tag recaptures in aggregate and by tag group.

## Usage

``` r
SSplotTags(
  replist = replist,
  subplots = 1:10,
  latency = NULL,
  taggroups = NULL,
  rows = 1,
  cols = 1,
  tagrows = 3,
  tagcols = 3,
  plot = TRUE,
  print = FALSE,
  pntscalar = 2.6,
  minnbubble = 8,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  ptsize = 10,
  res = 300,
  cex.main = 1,
  col1 = rgb(0, 0, 1, 0.7),
  col2 = "red",
  col3 = "grey95",
  col4 = "grey70",
  labels = c("Year", "Frequency", "Tag Group", "Fit to tag recaptures by tag group",
    "Post-latency tag recaptures aggregated across tag groups",
    "Observed tag recaptures by year and tag group",
    "Residuals for post-latency tag recaptures: (obs-exp)/sqrt(exp)",
    "Observed and expected post-latency tag recaptures by year and tag group",
    "Summarized observed and expected numbers of recaptures by fleet",
    "Pearson residuals by tag group"),
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

- latency:

  period of tag mixing to exclude from plots (in future could be
  included in SS output)

- taggroups:

  which tag groups to include in the plots. Default=NULL causes all
  groups to be included.

- rows:

  number or rows of panels for regular plots

- cols:

  number or columns of panels for regular plots

- tagrows:

  number or rows of panels for multi-panel plots

- tagcols:

  number or columns of panels for multi-panel plots

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- pntscalar:

  maximum bubble size for balloon plots; each plot scaled independently
  based on this maximum size and the values plotted. Often some plots
  look better with one value and others with a larger or smaller value.
  Default=2.6

- minnbubble:

  minimum number of years below which blank years will be added to
  bubble plots to avoid cropping

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

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

- col1:

  color for bubbles

- col2:

  color for lines with expected values

- col3:

  shading color for observations within latency period

- col4:

  shading color for observations after latency period

- labels:

  Vector of labels for plots (titles and axis labels).

- plotdir:

  Directory where PNG files will be written.

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Andre E. Punt, Ian G. Taylor, Ashleigh J. Novak
