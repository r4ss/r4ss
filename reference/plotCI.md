# Plot points with confidence intervals.

Given a set of x and y values and upper and lower bounds, this function
plots the points with error bars. This was Written by Venables and
modified to add access to ylim and contents.

## Usage

``` r
plotCI(
  x,
  y = NULL,
  uiw,
  liw = uiw,
  ylo = NULL,
  yhi = NULL,
  ...,
  sfrac = 0.01,
  ymax = NULL,
  add = FALSE,
  col = "black",
  log = ""
)
```

## Arguments

- x:

  The x coordinates of points in the plot

- y:

  The y coordinates of the points in the plot.

- uiw:

  The width of the upper portion of the confidence region.

- liw:

  The width of the lower portion of the confidence region.

- ylo:

  Lower limit of y range.

- yhi:

  Upper limit of y range.

- ...:

  Additional inputs that will be passed to the function
  `plot(x,y,ylim=ylim,...)`

- sfrac:

  Fraction of width of plot to be used for bar ends.

- ymax:

  Additional input for Upper limit of y range.

- add:

  Add points and intervals to existing plot? Default=FALSE.

- col:

  Color for the points and lines.

- log:

  Logarithmic scale for the y-axis? Should be "" or "y".

## Author

Bill Venables, Ian Stewart, Ian Taylor, John Wallace
