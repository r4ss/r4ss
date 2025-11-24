# Plot matrix of either length or observed age at true age

Distribution of length at age or observed age at true age is represented
as a histogram. Values are from the AGE_LENGTH_KEY and AGE_AGE_KEY
sections of Report.sso (ALK and AAK in the list created by SS_output)

## Usage

``` r
SSplotAgeMatrix(
  replist,
  option = 1,
  slices = NULL,
  scale = NULL,
  add = FALSE,
  col.grid = "grey90",
  col.bars = grey(0, alpha = 0.5),
  shift_hi = 0,
  shift_lo = 0,
  plot = TRUE,
  print = FALSE,
  labels = c("Age", "Length", "True age", "Observed age", "for ageing error type",
    "Distribution of", "at"),
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  mainTitle = TRUE,
  plotdir = "default"
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- option:

  Switch set to either 1 for length at true age or 2 for obs. age at
  true age

- slices:

  Optional input to choose which matrix (slice of the 3D-array) within
  AAK or ALK to plot. By default all slices will be shown. For ageing
  imprecision this should correspond to the ageing error matrix number.
  Distribution of length at age (ALK) is ordered by season, sub-season,
  and then morph. A future version could allow subsetting plots by these
  dimensions.

- scale:

  Multiplier for bars showing distribution. Species with many ages
  benefit from expanded bars. NULL value causes function to attempt
  automatic scaling.

- add:

  Add to existing plot

- col.grid:

  A character value specifying the color of the grid lines

- col.bars:

  The color of the filled polygons.

- shift_hi:

  A numeric value specifying the amount to shift the top of the polygon
  up.

- shift_lo:

  A numeric value specifying the amount to shift the bottom of the
  polygon up.

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

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

- plotdir:

  Directory where PNG files will be written.

## See also

[`SSplotNumbers()`](https://r4ss.github.io/r4ss/reference/SSplotNumbers.md)

## Author

Ian G. Taylor
