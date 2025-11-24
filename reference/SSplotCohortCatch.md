# Plot cumulative catch by cohort.

Cumulative catch contributions for each cohort are plotted based on
estimated catch-at-age matrix and weight-at-age values by fleet. Curves
are shown in units of both numbers and biomass.

## Usage

``` r
SSplotCohortCatch(
  replist,
  subplots = 1:2,
  add = FALSE,
  plot = TRUE,
  print = FALSE,
  cohortcols = "default",
  cohortfrac = 1,
  cohortvec = NULL,
  cohortlabfrac = 0.1,
  cohortlabvec = NULL,
  lwd = 3,
  plotdir = "default",
  xlab = "Year",
  labels = c("Age", "Cumulative catch by cohort (in numbers x1000)",
    "Cumulative catch by cohort (x1000 mt)"),
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

  Vector controlling which subplots to create

- add:

  Add to existing plot? (not yet implemented)

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- cohortcols:

  Vector of colors to show for each cohort. Default is range of colors
  shade indicating time period.

- cohortfrac:

  What fraction of the cohorts to include in plot. If value \< 1 is
  used, then cohorts are filtered to only include those with the highest
  maximum cumulative catch. Value will be overridden by `cohortvec`.

- cohortvec:

  Optional vector of birth years for cohorts to include in plot. Value
  overrides `cohortfrac`.

- cohortlabfrac:

  What fraction of the cohorts to label in plot. By default, top 10% of
  cohorts are labeled. Value will be overridden by `cohortlabvec`.

- cohortlabvec:

  Optional vector of birth years for cohorts to label in plot. Value
  overrides `cohortlabfrac`.

- lwd:

  Line width for plot elements.

- plotdir:

  Directory where PNG files will be written.

- xlab:

  x-label for all plots

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

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian Taylor
