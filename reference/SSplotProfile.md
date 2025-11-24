# Plot likelihood profile results

Makes a plot of change in negative-log-likelihood for each likelihood
component that contributes more than some minimum fraction of change in
total.

## Usage

``` r
SSplotProfile(
  summaryoutput,
  plot = TRUE,
  print = FALSE,
  models = "all",
  profile.string = "steep",
  profile.label = NULL,
  exact = FALSE,
  ylab = "Change in -log-likelihood",
  components = c("TOTAL", "Catch", "Equil_catch", "Survey", "Discard", "Mean_body_wt",
    "Length_comp", "Age_comp", "Size_at_age", "SizeFreq", "Morphcomp", "Tag_comp",
    "Tag_negbin", "Recruitment", "InitEQ_Regime", "Forecast_Recruitment", "Parm_priors",
    "Parm_softbounds", "Parm_devs", "F_Ballpark", "Crash_Pen"),
  component.labels = c("Total", "Catch", "Equilibrium catch", "Index data", "Discard",
    "Mean body weight", "Length data", "Age data", "Size-at-age data",
    "Generalized size data", "Morph composition data", "Tag recapture distribution",
    "Tag recapture total", "Recruitment", "Initital equilibrium recruitment",
    "Forecast recruitment", "Priors", "Soft bounds", "Parameter deviations",
    "F Ballpark", "Crash penalty"),
  minfraction = 0.01,
  sort.by.max.change = TRUE,
  col = "default",
  pch = "default",
  lty = 1,
  lty.total = 1,
  lwd = 2,
  lwd.total = 3,
  cex = 1,
  cex.total = 1.5,
  xlim = "default",
  ymax = "default",
  xaxs = "r",
  yaxs = "r",
  type = "o",
  legend = TRUE,
  legendloc = "topright",
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  plotdir = NULL,
  add_cutoff = FALSE,
  cutoff_prob = 0.95,
  add_no_prior_line = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- summaryoutput:

  List created by the function
  [`SSsummarize()`](https://r4ss.github.io/r4ss/reference/SSsummarize.md).

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- models:

  Optional subset of the models described in `summaryoutput`. Either
  "all" or a vector of numbers indicating columns in summary tables.

- profile.string:

  Character string used to find parameter over which the profile was
  conducted. If `exact=FALSE`, this can be a substring of one of the SS3
  parameter labels found in the Report.sso file. For instance, the
  default input 'steep' matches the parameter 'SR_BH_steep'. If
  `exact=TRUE`, then profile.string needs to be an exact match to the
  parameter label.

- profile.label:

  Label for x-axis describing the parameter over which the profile was
  conducted. NULL value will be replaced by an informative label if the
  parameter label contains one of the follow strings: "steep", "R0",
  "NatM", "L_at_Amax", "sigmaR", or "LnQ".

- exact:

  Should the `profile.string` have to match the parameter label exactly,
  or is a substring OK.

- ylab:

  Label for y-axis. Default is "Change in -log-likelihood".

- components:

  Vector of likelihood components that may be included in plot. List is
  further refined by any components that are not present in model or
  have little change over range of profile (based on limit
  `minfraction`). Hopefully this doesn't need to be changed.

- component.labels:

  Vector of labels for use in the legend that matches the vector in
  `components`.

- minfraction:

  Minimum change in likelihood (over range considered) as a fraction of
  change in total likelihood for a component to be included in the
  figure.

- sort.by.max.change:

  Switch giving option to sort components in legend in order of maximum
  amount of change in likelihood (over range considered). Default=TRUE.

- col:

  Optional vector of colors for each line.

- pch:

  Optional vector of plot characters for the points.

- lty:

  Line type for the likelihood components.

- lty.total:

  Line type for the total likelihood.

- lwd:

  Line width for the likelihood components.

- lwd.total:

  Line width for the total likelihood.

- cex:

  Character expansion for the points representing the likelihood
  components.

- cex.total:

  Character expansion for the points representing the total likelihood.

- xlim:

  Range for x-axis. Change in likelihood is calculated relative to
  values within this range.

- ymax:

  Maximum y-value. Default is 10% greater than largest value plotted.

- xaxs:

  The style of axis interval calculation to be used for the x-axis (see
  ?par for more info)

- yaxs:

  The style of axis interval calculation to be used for the y-axis (see
  ?par for more info).

- type:

  Line type (see ?plot for more info).

- legend:

  Add a legend?

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

- plotdir:

  Directory where PNG files will be written.

- add_cutoff:

  Add dashed line at ~1.92 to indicate 95% confidence interval based on
  common cutoff of half of chi-squared of p=.95 with 1 degree of
  freedom: `0.5*qchisq(p=cutoff_prob, df=1)`. The probability value can
  be adjusted using the `cutoff_prob` below.

- cutoff_prob:

  Probability associated with `add_cutoff` above.

- add_no_prior_line:

  Add line showing total likelihood without the prior (only appears when
  profiled parameter that includes a prior)

- verbose:

  A logical value specifying if output should be printed to the screen.

- ...:

  Additional arguments passed to the `plot` command.

## Note

Someday the function
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md) will be
improved and made to work directly with this plotting function, but they
don't yet work well together. Thus, even if
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md) is used,
the output should be read using
[`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md)
or by multiple calls to
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

## See also

[`SSsummarize()`](https://r4ss.github.io/r4ss/reference/SSsummarize.md),
[`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md)

Other profile functions:
[`PinerPlot()`](https://r4ss.github.io/r4ss/reference/PinerPlot.md),
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md)

## Author

Ian G. Taylor, Ian J. Stewart
