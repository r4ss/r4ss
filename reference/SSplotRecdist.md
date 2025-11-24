# Plot of recruitment distribution among areas and seasons

Image plot shows fraction of recruitment in each combination of area and
season. This is based on the RECRUITMENT_DIST section of the Report.sso
file.

## Usage

``` r
SSplotRecdist(
  replist,
  plot = TRUE,
  print = FALSE,
  areanames = NULL,
  seasnames = NULL,
  xlab = "",
  ylab = "",
  main = "distribution of recruitment by area and season",
  period = c("Initial", "Benchmark", "End year"),
  sexes = 1:2,
  plotdir = "default",
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

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- areanames:

  optional vector to replace c("Area1","Area2",...)

- seasnames:

  optional vector to replace c("Season1","Season2",...)

- xlab:

  optional x-axis label (if the area names aren\\t informative enough)

- ylab:

  optional y-axis label (if the season names aren\\t informative enough)

- main:

  title for plot

- period:

  period of recruitment distribution to show among the options
  "Initial", "Benchmark", and "End year"

- sexes:

  either 1 to only plot female distribution, 2 for males, or 1:2 to make
  both plots

- plotdir:

  Directory where PNG files will be written.

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
[`SSplotRecdevs()`](https://r4ss.github.io/r4ss/reference/SSplotRecdevs.md)

## Author

Ian Taylor
