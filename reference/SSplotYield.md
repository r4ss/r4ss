# Plot yield and surplus production.

Plot yield and surplus production from Stock Synthesis output. Surplus
production is based on Walters et al. (2008).

## Usage

``` r
SSplotYield(
  replist,
  subplots = 1:5,
  refpoints = c("MSY", "Btgt", "SPR", "Current"),
  add = FALSE,
  plot = TRUE,
  print = FALSE,
  labels = c("Fraction of unfished spawning biomass", "Equilibrium yield (t)",
    "Total biomass (t)", "Surplus production (t)", "Yield per recruit (kg)",
    "Spawning output"),
  col = "blue",
  col2 = "black",
  lty = 1,
  lwd = 2,
  cex.main = 1,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  plotdir = "default",
  verbose = TRUE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- subplots:

  vector controlling which subplots to create Numbering of subplots is
  as follows:

  - 1 yield curve

  - 2 yield curve with reference points

  - 3 surplus production vs. total biomass plots (Walters et al. 2008)

  - 4 surplus production vs. spawning biomass plots (Forrest et al.
    2023)

  - 5 yield per recruit plot

- refpoints:

  character vector of which reference points to display in subplot 2,
  from the options 'MSY', 'Btgt', and 'SPR'.

- add:

  add to existing plot? (not yet implemented)

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- labels:

  Vector of labels for plots (titles and axis labels).

- col:

  line color for equilibrium plot

- col2:

  line color for dynamic surplus production plot

- lty:

  line type (only applied to equilibrium yield plot at this time)

- lwd:

  Line width for plot elements.

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

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

- plotdir:

  Directory where PNG files will be written.

- verbose:

  A logical value specifying if output should be printed to the screen.

## References

Walters, Hilborn, and Christensen, 2008, Surplus production dynamics in
declining and recovering fish populations. *Can. J. Fish. Aquat. Sci.*
65: 2536-2551. <https://doi.org/10.1139/F08-170>.

Forrest, Kronlund, Cleary, and Grinnell. 2023. An evidence-based
approach for selecting a limit reference point for Pacific herring
(*Clupea pallasii*) stocks in British Columbia, Canada. *Can. J. Fish.
Aquat. Sci.* 80: 1071-1083. <https://doi.org/10.1139/cjfas-2022-0168>.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian Stewart, Ian Taylor
