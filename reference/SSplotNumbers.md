# Plot numbers-at-age related data and fits.

Plot numbers-at-age related data and fits from Stock Synthesis output.
Plots include bubble plots, mean age, equilibrium age composition,
sex-ratio, and ageing imprecision patterns.

## Usage

``` r
SSplotNumbers(
  replist,
  subplots = c(1:10),
  plot = TRUE,
  print = FALSE,
  numbers.unit = 1000,
  areas = "all",
  areanames = "default",
  areacols = NULL,
  pntscalar = 2.6,
  bub.bg = gray(0.5, alpha = 0.5),
  bublegend = TRUE,
  period = c("B", "M"),
  meanlines = TRUE,
  add = FALSE,
  labels = c("Year", "Age", "True age (yr)", "SD of observed age (yr)",
    "Mean observed age (yr)", "Mean age (yr)", "mean age in the population",
    "Ageing imprecision", "Numbers at age at equilibrium",
    "Equilibrium age distribution", "Fraction female in numbers at age", "Length",
    "Mean length (cm)", "mean length (cm) in the population", "expected numbers at age",
    "Beginning of year", "Middle of year", "expected numbers at length",
    "Fraction female in numbers at length"),
  pwidth = 6.5,
  pheight = 6.5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  plotdir = "default",
  mainTitle = FALSE,
  verbose = TRUE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- subplots:

  vector controlling which subplots to create Numbering of subplots is
  as follows,

  - 1: Expected numbers at age

  - 2: Mean age in the population

  - 3: Fraction female in numbers at age

  - 4: Equilibrium age distribution

  - 5: Ageing imprecision: SD of observed age (plot using image()
    formerly included in this group but now replaced by better
    distribution plots)

  - 6: Expected numbers at length

  - 7: Mean length in the population

  - 8: Fraction female in numbers at length

  - 9: no plot yet

  - 10: Distribution of observed age at true age by ageing error type

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- numbers.unit:

  Units for numbers. Default (based on typical Stock Synthesis setup) is
  thousands (numbers.unit=1000).

- areas:

  optional subset of areas to plot for spatial models

- areanames:

  names for areas. Default is to use Area1, Area2,...

- areacols:

  Optional vector of colors for each area if model has multiple areas.
  NULL value will be replaced by a default set of areas.

- pntscalar:

  maximum bubble size for bubble plots; each plot scaled independently
  based on this maximum size and the values plotted. Often some plots
  look better with one value and others with a larger or smaller value.
  Default=2.6

- bub.bg:

  background color for bubbles (no control over black border at this
  time)

- bublegend:

  Add legend with example bubble sizes?

- period:

  indicator of whether to make plots using numbers at age just from the
  beginning ("B") or middle of the year ("M") (new option starting with
  SSv3.11)

- meanlines:

  add lines for mean age or length on top of bubble plots

- add:

  add to existing plot? (not yet implemented)

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

- mainTitle:

  Logical indicating if a title should be included at the top (not yet
  implemented for all plots).

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md),
[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md)

## Author

Ian Stewart, Ian Taylor
