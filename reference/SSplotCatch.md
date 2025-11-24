# Plot catch related quantities.

Plot catch related quantities from Stock Synthesis output. Plots include
harvest rate, continuous F, landings, and discard fraction.

## Usage

``` r
SSplotCatch(
  replist,
  subplots = 1:16,
  add = FALSE,
  areas = 1,
  plot = TRUE,
  print = FALSE,
  type = "l",
  fleetlty = 1,
  fleetpch = 1,
  fleetcols = "default",
  fleetnames = "default",
  lwd = 3,
  areacols = NULL,
  areanames = "default",
  minyr = -Inf,
  maxyr = Inf,
  annualcatch = TRUE,
  forecastplot = FALSE,
  plotdir = "default",
  showlegend = TRUE,
  legendloc = "topleft",
  order = "default",
  xlab = "Year",
  labels = c("Harvest rate/Year", "Continuous F", "Landings", "Total catch",
    "Predicted discards", "Discard fraction", "(t)", "(numbers x1000)",
    "Observed and expected", "aggregated across seasons"),
  catchasnumbers = NULL,
  catchbars = TRUE,
  addmax = TRUE,
  ymax = NULL,
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

  Vector controlling which subplots to create Numbering of subplots is
  as follows,

  *Basic plots for all models*

  - 1 total catch as line plot (if no discards) or landings (if discards
    present)

  - 2 total catch as stacked bars (if no discards) or landings (if
    discards present)

  - 3 observed and expected landings (if different)

  - 9 harvest rate

  *Plots for models with discards*

  - 4 total catch (including discards)

  - 5 total catch (including discards) stacked

  - 6 discards

  - 7 discards stacked plot (depends on multiple fleets)

  - 8 discard fraction

  - 16 landings + dead discards

  *Plots for seasonal models*

  - 10 landings aggregated across seasons

  - 11 landings aggregated across seasons stacked

  - 12 total catch (if discards present) aggregated across seasons

  - 13 total catch (if discards present) aggregated across seasons
    stacked

  - 14 discards aggregated across seasons

  - 15 discards aggregated across seasons stacked

- add:

  Add to existing plot? (not yet implemented)

- areas:

  Optional subset of areas to plot for spatial models

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- type:

  Type parameter passed to plot function. Default "l" is lines only.
  Other options include "o" for overplotting points on lines.

- fleetlty:

  Vector of line type by fleet

- fleetpch:

  Vector of plot character by fleet

- fleetcols:

  Vector of colors by fleet

- fleetnames:

  Optional replacement for fleetnames used in data file.

- lwd:

  Line width for plot elements.

- areacols:

  Optional vector of colors for each area if model has multiple areas.
  NULL value will be replaced by a default set of areas.

- areanames:

  Names for areas. Default is to use Area1, Area2,...

- minyr:

  Optional input for minimum year to show in plots

- maxyr:

  Optional input for maximum year to show in plots

- annualcatch:

  Include plot of catch aggregated across seasons within each year

- forecastplot:

  Add points from forecast years

- plotdir:

  Directory where PNG files will be written.

- showlegend:

  Put legend on plot

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- order:

  Optional input to change the order of fleets in stacked plots.

- xlab:

  x-label for all plots

- labels:

  Vector of labels for plots (titles and axis labels).

- catchasnumbers:

  Is catch in numbers instead of biomass? Should be set automatically if
  set to NULL. If fleets include a mix of biomass and numbers, then
  catch plots should be interpreted carefully.

- catchbars:

  Show catch by fleet as barplot instead of stacked polygons?
  (default=TRUE)

- addmax:

  Add a point on the y-axis for the maximum catch (default=TRUE)

- ymax:

  Optional input for ymax value (can be used to add or subtract white
  space at the top of the figure)

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

Ian Taylor, Ian Stewart
