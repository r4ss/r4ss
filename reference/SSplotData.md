# Timeline of presence/absence of data by type, year, and fleet.

Plot shows graphical display of what data is being used in the model.
Some data types may not yet be included. Note, this is based on output
from the model, not the input data file.

## Usage

``` r
SSplotData(
  replist,
  plot = TRUE,
  print = FALSE,
  plotdir = "default",
  subplots = 1:2,
  fleetcol = "default",
  datatypes = "all",
  fleets = "all",
  fleetnames = "default",
  ghost = FALSE,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  margins = c(5.1, 2.1, 2.1, 8.1),
  cex = 2,
  lwd = 12,
  maxsize = 1,
  alphasize = 1,
  mainTitle = FALSE,
  verbose = TRUE,
  subplot = lifecycle::deprecated()
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

- plotdir:

  Directory where PNG files will be written.

- subplots:

  vector controlling which subplots to create Currently there are only 2
  subplots:

  - 1 equal size points showing presence/absence of data type by
    year/fleet

  - 2 points scaled to indicate quantity or precision of data

- fleetcol:

  Either the string "default", or a vector of colors to use for each
  fleet. If tagging data or environmental data are included, an
  additional color needs to be added for the tag releases which are not
  assigned to a fleet.

- datatypes:

  Either the string "all", or a vector including some subset of the
  following: "catch", "cpue", "lendbase", "sizedbase", "agedbase",
  "condbase", "ghostagedbase", "ghostcondbase", "ghostlendbase",
  "ladbase", "wadbase", "mnwgt", "discard", "tagrelease", "tagdbase1",
  and "morphcompdbase".

- fleets:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing fleets or surveys to be included in the plot.

- fleetnames:

  Optional replacement for fleetnames used in data file.

- ghost:

  TRUE/FALSE indicator for whether to show presence of composition data
  from ghost fleets (data for which the fit is shown, but is not
  included in the likelihood calculations).

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

- margins:

  margins of plot (passed to par() function), which may need to be
  increased if fleet names run off right-hand margin

- cex:

  Character expansion for points showing isolated years of data

- lwd:

  Line width for plot elements.

- maxsize:

  The size (cex) of the largest bubble in the datasize plot. Default is
  1.

- alphasize:

  The transparency of the bubbles in the datasize plot. Defaults to 1
  (no transparency). Useful for models with lots of overlapping points.

- mainTitle:

  Logical indicating if a title should be included at the top (not yet
  implemented for all plots).

- verbose:

  A logical value specifying if output should be printed to the screen.

- subplot:

  Deprecated. Use subplots instead.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md)

## Author

Ian Taylor, Chantel Wetzel, Cole Monnahan
