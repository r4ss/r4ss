# Plot selectivity

Plot selectivity, including retention and other quantities, with
additional plots for time-varying selectivity.

## Usage

``` r
SSplotSelex(
  replist,
  infotable = NULL,
  fleets = "all",
  fleetnames = "default",
  sizefactors = c("Lsel"),
  agefactors = c("Asel", "Asel2"),
  years = "endyr",
  minyr = -Inf,
  maxyr = Inf,
  season = 1,
  sexes = "all",
  selexlines = 1:6,
  subplots = 1:25,
  skipAgeSelex10 = TRUE,
  plot = TRUE,
  print = FALSE,
  add = FALSE,
  labels = c("Length (cm)", "Age (yr)", "Year", "Selectivity", "Retention",
    "Discard mortality"),
  col1 = "red",
  col2 = "blue",
  lwd = 2,
  spacepoints = 5,
  staggerpoints = 1,
  legendloc = "bottomright",
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  mainTitle = TRUE,
  mar = NULL,
  plotdir = "default",
  verbose = TRUE,
  subplot = lifecycle::deprecated()
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- infotable:

  Optional table of information controlling appearance of plot and
  legend. Is produced as output and can be modified and entered as
  input.

- fleets:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing fleets or surveys to be included in the plot.

- fleetnames:

  Optional replacement for fleetnames used in data file.

- sizefactors:

  Which elements of the factors column of SIZE_SELEX should be included
  in plot of selectivity across multiple fleets?

- agefactors:

  Which elements of the factors column of AGE_SELEX should be included
  in plot of selectivity across multiple fleets?

- years:

  Which years for selectivity are shown in multi-line plot (default =
  last year of model).

- minyr:

  optional input for minimum year to show in plots

- maxyr:

  optional input for maximum year to show in plots

- season:

  Which season (if seasonal model) for selectivity shown in multi-line
  plot (default = 1).

- sexes:

  Optional vector to subset sexes for which to make plots (1=females,
  2=males)

- selexlines:

  Vector to select which lines get plotted. values are 1.
  Selectivity, 2. Retention, 3. Discard mortality, 4. Keep.

- subplots:

  Vector controlling which subplots to create. Numbering of subplots is
  as follows,

  *Plots with all fleets grouped together*

  - 1 selectivity at length in end year for all fleets shown together

  - 2 selectivity at age in end year for all fleets shown together (this
    includes both age-based selectivity "Asel" and age values derived
    from length-based, "Asel2". You can choose only one using
    "agefactors" if needed.)

  *Plots of time-varying length-based selectivity*

  - 3 selectivity at length time-varying surface

  - 4 selectivity at length time-varying contour

  - 5 retention at length time-varying surface

  - 6 retention at length time-varying surface

  - 7 discard mortality time-varying surface

  - 8 discard mortality time-varying contour

  *Selectivity at length in end year by fleet*

  - 9 selectivity, retention, and discard mortality at length in ending
    year

  *Plots of time-varying age-based selectivity*

  - 11 selectivity at age time-varying surface

  - 12 selectivity at age time-varying contour

  *Selectivity at age in end year by fleet*

  - 13 selectivity at age in ending year if time-varying

  - 14 selectivity at age in ending year if NOT time-varying

  - 15 matrix of selectivity deviations for semi-parametric selectivity

  *Selectivity for both/either age or length*

  - 21 selectivity at age and length contour with overlaid growth curve

  - 22 selectivity with uncertainty if requested at end of control file

- skipAgeSelex10:

  Exclude plots for age selectivity type 10 (selectivity = 1.0 for all
  ages beginning at age 1)?

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- add:

  Add to existing plot (not yet implemented)

- labels:

  Vector of labels for plots (titles and axis labels).

- col1:

  color for female growth curve

- col2:

  color for male growth curve

- lwd:

  Line width for plot elements.

- spacepoints:

  number of years between points shown on top of lines (for long
  timeseries, points every year get mashed together)

- staggerpoints:

  number of years to stagger the first point (if `spacepoints > 1`) for
  each line (so that adjacent lines have points in different years)

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

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

- mar:

  Either NULL to allow the default (which depends on whether the main
  title is included or not) or a numerical vector of the form c(bottom,
  left, top, right) which gives the number of lines of margin to be
  specified on the four sides of the plot, which is passed to
  [`par()`](https://rdrr.io/r/graphics/par.html).

- plotdir:

  Directory where PNG files will be written.

- verbose:

  A logical value specifying if output should be printed to the screen.

- subplot:

  Deprecated. Use subplots instead.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian Stewart, Ian Taylor
