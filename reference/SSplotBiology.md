# Plot biology related quantities.

Plot biology related quantities from Stock Synthesis model output,
including mean weight, maturity, fecundity, and spawning output.

## Usage

``` r
SSplotBiology(
  replist,
  plot = TRUE,
  print = FALSE,
  add = FALSE,
  subplots = 1:32,
  seas = 1,
  morphs = NULL,
  forecast = FALSE,
  minyr = -Inf,
  maxyr = Inf,
  colvec = c("red", "blue", "grey20"),
  areacols = NULL,
  ltyvec = c(1, 2),
  shadealpha = 0.1,
  imageplot_text = FALSE,
  imageplot_text_round = 0,
  legendloc = "topleft",
  plotdir = "default",
  labels = c("Length (cm)", "Age (yr)", "Maturity", "Mean weight (kg) in last year",
    replist[["SpawnOutputLabel"]], "Length (cm, beginning of the year)",
    "Natural mortality", "Female weight (kg)", "Female length (cm)", "Fecundity",
    "Default fecundity label", "Year", "Hermaphroditism transition rate",
    "Fraction females by age in ending year"),
  pwidth = 6.5,
  pheight = 5,
  pheight_tall = 6.5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  mainTitle = TRUE,
  verbose = TRUE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- add:

  add to existing plot

- subplots:

  vector controlling which subplots to create Numbering of subplots is
  as follows:

  - 1 growth curve only

  - 2 growth curve with CV and SD

  - 3 growth curve with maturity and weight

  - 4 distribution of length at age (still in development)

  - 5 length or wtatage matrix

  - 6 maturity

  - 7 fecundity from model parameters

  - 8 fecundity at weight from BIOLOGY section

  - 9 fecundity at length from BIOLOGY section

  - 10 spawning output at length

  - 11 spawning output at age

  - 21 Natural mortality (if age-dependent)

  - 22 Time-varying growth persp

  - 23 Time-varying growth contour

  - 24 plot time-series of any time-varying quantities (created if the
    MGparm_By_Year_after_adjustments table (report:7) is available in
    the Report.sso file)

  - 31 hermaphroditism transition probability

  - 32 sex ratio in ending year (only plotted when model has
    hermaphroditism)

  Additional plots not created by default

  - 101 diagram with labels showing female growth curve

  - 102 diagram with labels showing female growth curve & male offsets

  - 103 diagram with labels showing female CV = f(A) (offset type 2)

  - 104 diagram with labels showing female CV = f(A) & male offset (type
    2)

  - 105 diagram with labels showing female CV = f(A) (offset type 3)

  - 106 diagram with labels showing female CV = f(A) & male offset (type
    3)

- seas:

  which season to plot (values other than 1 only work in seasonal models
  but but maybe not fully implemented)

- morphs:

  Which morphs to plot (if more than 1 per sex)? By default this will be
  `replist[["mainmorphs"]]`

- forecast:

  Include forecast years in plots of time-varying biology?

- minyr:

  optional input for minimum year to show in plots

- maxyr:

  optional input for maximum year to show in plots

- colvec:

  vector of length 3 with colors for various points/lines

- areacols:

  Optional vector of colors for each area if model has multiple areas.
  NULL value will be replaced by a default set of areas.

- ltyvec:

  vector of length 2 with lty for females/males in growth plots values
  can be applied to other plots in the future

- shadealpha:

  Transparency parameter used to make default shadecol values (see ?rgb
  for more info)

- imageplot_text:

  Whether to add numerical text to the image plots when using weight at
  age. Defaults to FALSE.

- imageplot_text_round:

  The number of significant digits to which the image plot text is
  rounded. Defaults to 0, meaning whole numbers. If all your values are
  small and there's no contrast in the text, you might want to make this
  1 or 2.

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- plotdir:

  Directory where PNG files will be written.

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

- mainTitle:

  Logical indicating if a title should be included at the top (not yet
  implemented for all plots).

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian Stewart, Ian Taylor
