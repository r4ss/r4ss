# Create multi-figure plots.

Function created as an alternative to lattice package for multi-figure
plots of composition data and fits from Stock Synthesis output.

## Usage

``` r
make_multifig(
  ptsx,
  ptsy,
  yr,
  linesx = 0,
  linesy = 0,
  ptsSD = 0,
  sampsize = 0,
  effN = 0,
  showsampsize = TRUE,
  showeffN = TRUE,
  sampsize_label = "N=",
  effN_label = "effN=",
  sampsizeround = 1,
  maxrows = 6,
  maxcols = 6,
  rows = 1,
  cols = 1,
  fixdims = TRUE,
  main = "",
  cex.main = 1,
  xlab = "",
  ylab = "",
  size = 1,
  cexZ1 = 1.5,
  bublegend = TRUE,
  maxsize = NULL,
  do.sqrt = TRUE,
  minnbubble = 8,
  allopen = TRUE,
  xbuffer = c(0.1, 0.1),
  ybuffer = c(0, 0.15),
  yupper = NULL,
  ymin0 = TRUE,
  xlas = 0,
  ylas = NULL,
  axis1 = NULL,
  axis2 = NULL,
  axis1labs = NULL,
  linepos = 1,
  type = "o",
  polygons = TRUE,
  bars = FALSE,
  barwidth = "default",
  ptscex = 1,
  ptscol = 1,
  ptscol2 = 1,
  colvec = c(rgb(1, 0, 0, 0.7), rgb(0, 0, 1, 0.7), rgb(0.1, 0.1, 0.1, 0.7)),
  linescol = c(rgb(0, 0.8, 0, 0.7), rgb(1, 0, 0, 0.7), rgb(0, 0, 1, 0.7)),
  lty = 1,
  lwd = 2,
  pch = 1,
  nlegends = 3,
  legtext = list("yr", "sampsize", "effN"),
  legx = "default",
  legy = "default",
  legadjx = "default",
  legadjy = "default",
  legsize = c(1.2, 1),
  legfont = c(2, 1),
  venusmars = TRUE,
  sampsizeline = FALSE,
  effNline = FALSE,
  sampsizemean = NULL,
  effNmean = NULL,
  ipage = 0,
  scalebins = FALSE,
  sexvec = NULL,
  multifig_colpolygon = grey(c(0.6, 0.8, 0.7), alpha = 0.7),
  multifig_oma = NULL,
  ...
)
```

## Arguments

- ptsx:

  vector of x values for points or bars

- ptsy:

  vector of y values for points or bars of same length as ptsx

- yr:

  vector of category values (years) of same length as ptsx

- linesx:

  optional vector of x values for lines

- linesy:

  optional vector of y values for lines

- ptsSD:

  optional vector of standard deviations used to plot error bars on top
  of each point under the assumption of normally distributed error

- sampsize:

  optional sample size vector of same length as ptsx

- effN:

  optional effective sample size vector of same length as ptsx

- showsampsize:

  show sample size values on plot?

- showeffN:

  show effective sample size values on plot?

- sampsize_label:

  label on sampsize

- effN_label:

  label on effN

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

- size:

  vector of bubbles sizes if making a bubble plot

- cexZ1:

  Character expansion (cex) for point associated with value of 1.

- bublegend:

  Add legend with example bubble sizes to bubble plots.

- maxsize:

  maximum size of bubbles

- do.sqrt:

  scale bubbles based on sqrt of size vector. see ?bubble3 for more
  info.

- minnbubble:

  number of unique x values before adding buffer. see ?bubble3 for more
  info.

- allopen:

  should all bubbles be open? see ?bubble3 for more info.

- xbuffer:

  extra space around points on the left and right as fraction of total
  width of plot

- ybuffer:

  extra space around points on the bottom and top as fraction of total
  height of plot

- yupper:

  upper limit on ymax (applied before addition of ybuffer)

- ymin0:

  fix minimum y-value at 0?

- xlas:

  label style (las) input for x-axis. Default 0 has horizontal labels,
  input 2 would provide vertical labels.

- ylas:

  label style (las) input for y-axis. Default NULL has horizontal labels
  when all labels have fewer than 6 characters and vertical otherwise.
  Input 0 would force vertical labels, and 1 would force horizontal.

- axis1:

  optional position of bottom axis values

- axis2:

  optional position of left size axis values

- axis1labs:

  optional vector of labels for axis1 (either NULL or needs to match
  length of axis1)

- linepos:

  should lines be added on top of points (linepos=1) or behind
  (linepos=2)? A value of linepos = 0 will result in no line.

- type:

  type of line/points used for observed values (see 'type' in ?plot for
  details) on top of a grey polygon. Default is "o" for overplotting
  points on lines.

- polygons:

  should polygons be added to the (turning off is required for sex-ratio
  plot)

- bars:

  should the ptsx/ptsy values be bars instead of points (TRUE/FALSE) NOT
  CURRENTLY FUNCTIONAL

- barwidth:

  width of bars in barplot, default method chooses based on quick and
  dirty formula also, current method of plot(...type='h') could be
  replaced with better approach

- ptscex:

  character expansion factor for points (default=1)

- ptscol:

  color for points/bars

- ptscol2:

  color for negative value points in bubble plots

- colvec:

  Vector of length 3 with colors for females, males, unsexed fish

- linescol:

  color for lines

- lty:

  line type

- lwd:

  Line width for plot elements.

- pch:

  point character type

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

- venusmars:

  Label females and males with venus and mars symbols?

- sampsizeline:

  show line for input sample sizes on top of conditional age-at-length
  plots (TRUE/FALSE/scalar, still in development)

- effNline:

  show line for effective sample sizes on top of conditional
  age-at-length plots (TRUE/FALSE/scalar, still in development)

- sampsizemean:

  mean input sample size value (used when sampsizeline=TRUE)

- effNmean:

  mean effective sample size value (used when effNline=TRUE)

- ipage:

  which page of plots when covering more than will fit within maxrows by
  maxcols.

- scalebins:

  Rescale expected and observed proportions by dividing by bin width for
  models where bins have different widths? Caution!: May not work
  correctly in all cases.

- sexvec:

  vector of sex codes if more than one present (otherwise NULL)

- multifig_colpolygon:

  vector of polygon fill colors of length 3 (for females, males, and
  unsexed fish). Can be input to SS_plots and will be passed to this
  function via the ... argument.

- multifig_oma:

  vector of outer margins. Can be input to SS_plots and will be passed
  to this function via the ... argument.

- ...:

  additional arguments passed to `par`.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),[`SSplotComps()`](https://r4ss.github.io/r4ss/reference/SSplotComps.md)

## Author

Ian Taylor
