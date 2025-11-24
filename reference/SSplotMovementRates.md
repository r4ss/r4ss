# Plot movement rates from model output

Plots estimated movement rates in final year for each area/season with
movement as reported in Report.sso. If movement is time-varying, an
additional figure shows pattern across years (if the
MGparm_By_Year_after_adjustments table (report:7) is available in the
Report.sso file)

## Usage

``` r
SSplotMovementRates(
  replist,
  plot = TRUE,
  print = FALSE,
  subplots = 1:2,
  plotdir = "default",
  colvec = "default",
  ylim = "default",
  legend = TRUE,
  legendloc = "topleft",
  moveseas = "all",
  min.move.age = 0.5,
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

- subplots:

  which subplots to create.

- plotdir:

  Directory where PNG files will be written.

- colvec:

  vector of colors for each movement rate in the plot

- ylim:

  optional input for y range of the plot. By default plot ranges from 0
  to 10% above highest movement rate (not including fish staying in an
  area).

- legend:

  add a legend designating which color goes with which pair of areas?

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- moveseas:

  choice of season for which movement rates are shown

- min.move.age:

  Minimum age of movement (in future will come from Report file)

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

[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md),
`SSplotMovementRates()`,

## Author

Ian Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
SSplotMovementRates(myreplist)
} # }
```
