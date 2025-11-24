# Make plot of likelihood contributions by fleet

This style of plot was officially named a "Piner Plot" at the CAPAM
Selectivity Workshop, La Jolla March 2013. This is in honor of Kevin
Piner's contributions to interpreting likelihood profiles. He's surely
not the first person to make such a plot but the name seems to have
stuck.

## Usage

``` r
PinerPlot(
  summaryoutput,
  plot = TRUE,
  print = FALSE,
  component = "Length_like",
  main = "Changes in length-composition likelihoods by fleet",
  models = "all",
  fleets = "all",
  fleetnames = "default",
  profile.string = "R0",
  profile.label = expression(log(italic(R)[0])),
  exact = FALSE,
  ylab = "Change in -log-likelihood",
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
  verbose = TRUE,
  fleetgroups = NULL,
  likelihood_type = "raw_times_lambda",
  minfraction = 0.01
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

- component:

  Which likelihood component to plot. Default is "Length_like".

- main:

  Title for plot. Should match component.

- models:

  Optional subset of the models described in `summaryoutput`. Either
  "all" or a vector of numbers indicating columns in summary tables.

- fleets:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing fleets or surveys to be included in the plot.

- fleetnames:

  Optional replacement for fleetnames used in data file.

- profile.string:

  Character string used to find parameter over which the profile was
  conducted. If `exact=FALSE`, this can be a substring of one of the SS
  parameter labels found in the Report.sso file. For instance, the
  default input 'R0' matches the parameter 'SR_LN(R0)'. If `exact=TRUE`,
  then profile.string needs to be an exact match to the parameter label.

- profile.label:

  Label for x-axis describing the parameter over which the profile was
  conducted.

- exact:

  Should the `profile.string` have to match the parameter label exactly,
  or is a substring OK.

- ylab:

  Label for y-axis. Default is "Change in -log-likelihood".

- col:

  Optional vector of colors for each line.

- pch:

  Optional vector of plot characters for the points.

- lty:

  Line total for the likelihood components.

- lty.total:

  Line type for the total likelihood.

- lwd:

  Line width for plot elements.

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

  Maximum y-value. Default is 10\\ plotted.

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

- verbose:

  A logical value specifying if output should be printed to the screen.

- fleetgroups:

  Optional character vector, with length equal to the number of declared
  fleets, where fleets with the same value are aggregated

- likelihood_type:

  choice of "raw" or "raw_times_lambda" (the default) determines whether
  or not likelihoods plotted are adjusted by lambdas (likelihood
  weights)

- minfraction:

  Minimum change in likelihood (over range considered) as a fraction of
  change in total likelihood for a component to be included in the
  figure.

## References

Kevin Piner says that he's not the originator of this idea so Athol
Whitten is going to add a reference here.

## See also

Other profile functions:
[`SSplotProfile()`](https://r4ss.github.io/r4ss/reference/SSplotProfile.md),
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md)

## Author

Ian G. Taylor, Kevin R. Piner, James T. Thorson
