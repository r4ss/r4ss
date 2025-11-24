# Create a bubble plot.

Bubble plot based on function vaguely based on `bubble` by Edzer Pebesma
in gstat package. By default, positive values have closed bubbles and
negative values have open bubbles.

## Usage

``` r
bubble3(
  x,
  y,
  z,
  col = 1,
  cexZ1 = 5,
  maxsize = NULL,
  do.sqrt = TRUE,
  bg.open = gray(0.95, 0.3),
  legend = TRUE,
  legendloc = "top",
  legend.z = "default",
  legend.yadj = 1.1,
  main = "",
  cex.main = 1,
  xlab = "",
  ylab = "",
  minnbubble = 3,
  xlim = NULL,
  ylim = NULL,
  axis1 = TRUE,
  xlimextra = 1,
  add = FALSE,
  las = 1,
  allopen = TRUE
)
```

## Arguments

- x:

  Vector of x-values.

- y:

  Vector of y-values.

- z:

  Vector of bubble sizes, where positive sizes will be plotted as closed
  bubbles and negative as open unless `allopen==TRUE`.

- col:

  Color for bubbles. Should be either a single value or vector of length
  equal to x, y, and z vectors.

- cexZ1:

  Character expansion (cex) value for a proportion of 1.0.

- maxsize:

  Size of largest bubble. Preferred option is now an expansion factor
  for a bubble with z=1 (see `cexZ1` above).

- do.sqrt:

  Should size be based on the area? (Diameter proportional to sqrt(z)).
  Default=TRUE.

- bg.open:

  background color for open bubbles (border will equal 'col')

- legend:

  Add a legend?

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- legend.z:

  If a legend is added, what z values will be shown. Default is
  c(-3,-2,-1,.1,1,2,3) for Pearson-like quantities and a smaller range
  for proportions that are all less than 1.

- legend.yadj:

  If a legend is added, how much should the y-axis be expanded to make
  space for it.

- main:

  Title of plot. Default="".

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

- xlab:

  X-axis label.

- ylab:

  Y-axis label.

- minnbubble:

  Minimum number of unique x values below which extra space is added to
  horizontal axis (to make plot look better). Default = 8.

- xlim:

  Optional limits on x-range.

- ylim:

  Optional limits on y-range.

- axis1:

  Show the horizontal axis on plot? Option allows turning off for use in
  multi-figure plots.

- xlimextra:

  Extra space (see minnbubble above). Default = 1.

- add:

  Add bubbles to existing plot? Default=FALSE.

- las:

  Style of axis labels (see ?par for more info).

- allopen:

  Should all bubbles be open (instead of just negative values)?

## Author

Ian Stewart and Ian Taylor
