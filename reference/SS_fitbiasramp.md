# Estimate bias adjustment for recruitment deviates

Uses standard error of estimated recruitment deviates to estimate the 5
controls (Methot and Taylor, 2011) for bias adjustment in Stock
Synthesis.

## Usage

``` r
SS_fitbiasramp(
  replist,
  verbose = FALSE,
  startvalues = NULL,
  method = "BFGS",
  twoplots = TRUE,
  transform = FALSE,
  plot = TRUE,
  print = FALSE,
  plotdir = "default",
  shownew = TRUE,
  oldctl = NULL,
  newctl = NULL,
  altmethod = "nlminb",
  exclude_forecast = FALSE,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  ptsize = 10,
  res = 300,
  cex.main = 1
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- verbose:

  A logical value specifying if output should be printed to the screen.

- startvalues:

  A vector of 5 values for the starting points in the minimization.
  Default=NULL.

- method:

  A method to apply to the 'optim' function. See ?optim for options.
  Default="BFGS". By default, optim is not used, and the optimization is
  based on the input `altmethod`.

- twoplots:

  Make a two-panel plot showing devs as well as transformed uncertainty,
  or just the second panel in the set? Default=TRUE.

- transform:

  An experimental option to treat the transform the 5 quantities to
  improve minimization. Doesn't work well. Default=FALSE.

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- plotdir:

  Directory where PNG files will be written.

- shownew:

  Include new estimated bias adjustment values on top of values used in
  the model? (TRUE/FALSE)

- oldctl:

  Optional name of existing control file to modify. Default=NULL.

- newctl:

  Optional name of new control file to create from old file with
  estimated bias adjustment values. Default=NULL.

- altmethod:

  Optimization tool to use in place of optim, either "nlminb" or
  "psoptim". If not equal to either of these, then optim is used.

- exclude_forecast:

  Exclude forecast values in the estimation of alternative bias
  adjustment inputs?

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

- ptsize:

  Point size for plotted text in plots printed to files (see
  [`help("png")`](https://rdrr.io/r/grDevices/png.html) in R for
  details).

- res:

  Resolution of plots printed to files. The default is `res = 300`.

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

## Details

Implementation of the bias adjustment ramp within Stock Synthesis
increases the likelihood that the estimated recruitment events, which
are log-normally distributed, are mean unbiased and comparable to
results from Markov chain Monte Carlo estimation routines (Methot and
Taylor, 2011). Options to account for the fact that data typically do
not equally represent all modelled time periods are as follows:

1.  fix the bias adjustment parameters at best-guess values informed by
    a previous assessment or model run;

2.  fix values based on data availability, such that the start of the
    ramp aligns with the availability of composition data, the ramp down
    begins the last year those data are informative about recruitment,
    and the adjustment level is informed by life history;

3.  set the adjustment level to 1.0 for all years to mimic how it was
    handled it Stock Synthesis prior to 2009; or

4.  set the adjustment level to 0.0 for all years, but this last option
    is not recommended because it will lead to biased results.

## References

Methot, R.D. and Taylor, I.G., 2011. Adjusting for bias due to
variability of estimated recruitments in fishery assessment models. Can.
J. Fish. Aquat. Sci., 68:1744-1760.

## See also

[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian Taylor
