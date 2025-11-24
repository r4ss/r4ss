# Create multi-figure sex ratio plots.

Modified version of
[`make_multifig()`](https://r4ss.github.io/r4ss/reference/make_multifig.md)
for multi-figure plots of sex ratio data with crude confidence intervals
(+/i 1 se) and fits from Stock Synthesis output.

## Usage

``` r
make_multifig_sexratio(
  dbase,
  sexratio.option = 2,
  CI = 0.75,
  sampsizeround = 1,
  maxrows = 6,
  maxcols = 6,
  rows = 1,
  cols = 1,
  fixdims = TRUE,
  main = "",
  cex.main = 1,
  xlab = "",
  ylab = "Fraction female",
  horiz_lab = "default",
  xbuffer = c(0.1, 0.1),
  ybuffer = "default",
  yupper = NULL,
  datonly = FALSE,
  showsampsize = TRUE,
  showeffN = TRUE,
  axis1 = NULL,
  axis2 = NULL,
  ptscex = 1,
  ptscol = gray(0.5),
  linescol = 4,
  lty = 1,
  lwd = 2,
  nlegends = 3,
  legtext = list("yr", "sampsize", "effN"),
  legx = "default",
  legy = "default",
  legadjx = "default",
  legadjy = "default",
  legsize = c(1.2, 1),
  legfont = c(2, 1),
  ipage = 0,
  multifig_oma = c(5, 5, 5, 2) + 0.1,
  ...
)
```

## Arguments

- dbase:

  element of list created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)
  passed from
  [`SSplotSexRatio()`](https://r4ss.github.io/r4ss/reference/SSplotSexRatio.md)

- sexratio.option:

  code to choose among (1) female:male ratio or (2) fraction females out
  of the total (the default)

- CI:

  confidence interval for uncertainty

- sampsizeround:

  rounding level for sample size values

- maxrows:

  maximum (or fixed) number or rows of panels in the plot

- maxcols:

  maximum (or fixed) number or columns of panels in the plot

- rows:

  number or rows to return to as default for next plots to come or for
  single plots

- cols:

  number or cols to return to as default for next plots to come or for
  single plots

- fixdims:

  fix the dimensions at maxrows by maxcols or resize based on number of
  elements in `yr` input.

- main:

  title of plot

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

- xlab:

  x-axis label

- ylab:

  y-axis label

- horiz_lab:

  axis labels set horizontal all the time (TRUE), never (FALSE) or only
  when relatively short ("default")

- xbuffer:

  extra space around points on the left and right as fraction of total
  width of plot

- ybuffer:

  extra space around points on the bottom and top as fraction of total
  height of plot. "default" will cause c(0,.15) for sexratio.option=1
  and c(.15, .3) for sexratio.option=2.

- yupper:

  upper limit on ymax (applied before addition of ybuffer)

- datonly:

  make plots of data without fits?

- showsampsize:

  add sample sizes to plot

- showeffN:

  add effective sample sizes to plot

- axis1:

  position of bottom axis values

- axis2:

  position of left size axis values

- ptscex:

  character expansion factor for points (default=1)

- ptscol:

  color for points/bars

- linescol:

  color for fitted model

- lty:

  line type

- lwd:

  Line width for plot elements.

- nlegends:

  number of lines of text to add as legends in each plot

- legtext:

  text in legend, a list of length=nlegends. values may be any of 1.
  "yr", 2. "sampsize", 3. "effN", or a vector of length = ptsx.

- legx:

  vector of length=nlegends of x-values of legends (default is first one
  on left, all after on right)

- legy:

  vector of length=nlegends of y-values of legends (default is top for
  all plots)

- legadjx:

  left/right adjustment of legends around legx

- legadjy:

  left/right adjustment of legends around legy

- legsize:

  font size for legends. default=c(1.2,1.0) (larger for year and normal
  for others)

- legfont:

  font type for legends, same as "font" under ?par

- ipage:

  which page of plots when covering more than will fit within maxrows by
  maxcols.

- multifig_oma:

  vector of outer margins. Can be input to SS_plots and will be passed
  to this function via the ... argument.

- ...:

  additional arguments (NOT YET IMPLEMENTED).

## Details

The SE of the sex ratio is crude and calculated as follows. First,
assume a multinomial which as MLEs of proportions. Then use the delta
method of the ratio F/M, using the MLE as the expected values and
analytical variances and covariance between F and M. After some algebra
this calculation reduces to:
`SE(F/M)= sqrt((f/m)^2*( (1-f)/(f*N) + (1-m)/(m*N) +2/N ))`. Confidence
intervals created from these should be considered very crude and would
not necessarily be appropriate for future alternative compositional
likelihoods.

This function was derived from make_multifig and hence has a lot of
overlap in functionality and arguments.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),[`SSplotSexRatio()`](https://r4ss.github.io/r4ss/reference/SSplotSexRatio.md)

## Author

Cole Monnahan. Adapted from
[`make_multifig()`](https://r4ss.github.io/r4ss/reference/make_multifig.md).
