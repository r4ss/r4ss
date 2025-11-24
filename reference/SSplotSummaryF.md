# Plot the summary F (or harvest rate).

Plots the summary F (or harvest rate) as set up in the starter file
Needs a lot of work to be generalized

## Usage

``` r
SSplotSummaryF(
  replist,
  yrs = "all",
  Ftgt = NA,
  ylab = "Summary Fishing Mortality",
  plot = TRUE,
  print = FALSE,
  plotdir = "default",
  verbose = TRUE,
  uncertainty = TRUE,
  add = FALSE,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  mar = NULL
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- yrs:

  Which years to include.

- Ftgt:

  Target F where horizontal line is shown.

- ylab:

  Y-axis label.

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- plotdir:

  Directory where PNG files will be written.

- verbose:

  A logical value specifying if output should be printed to the screen.

- uncertainty:

  Show 95% uncertainty intervals around point estimates?

- add:

  add to existing plot

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

- mar:

  Either NULL to allow the default (which depends on whether the main
  title is included or not) or a numerical vector of the form c(bottom,
  left, top, right) which gives the number of lines of margin to be
  specified on the four sides of the plot, which is passed to
  [`par()`](https://rdrr.io/r/graphics/par.html).

## See also

[`SSplotTimeseries()`](https://r4ss.github.io/r4ss/reference/SSplotTimeseries.md)

## Author

Allan Hicks
