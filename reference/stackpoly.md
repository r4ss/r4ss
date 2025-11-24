# modified from "stackpoly" by Jim Lemon from "plotrix" package

Plot one or more columns of numeric values as the top edges of polygons
instead of lines.

## Usage

``` r
stackpoly(
  x,
  y,
  main = "",
  xlab = "",
  ylab = "",
  xat = NA,
  xaxlab = NA,
  xlim = NA,
  ylim = NA,
  lty = 1,
  border = NA,
  col = NA,
  axis4 = F,
  x.hash = NULL,
  density = 20,
  ...
)
```

## Arguments

- x:

  A numeric data frame or matrix with the 'x' values. If 'y' is NULL,
  these will become the 'y' values and the 'x' positions will be the
  integers from 1 to `dim(x)[1]`.

- y:

  The 'y' values.

- main:

  The title for the plot.

- xlab:

  x axis labels for the plot.

- ylab:

  y axis labels for the plot.

- xat:

  Where to put the optional xaxlabs.

- xaxlab:

  Optional labels for the x positions.

- xlim:

  Optional x limits.

- ylim:

  Optional y limits.

- lty:

  Line type for the polygon borders.

- border:

  Color for the polygon borders.

- col:

  Color to fill the polygons. If NULL, 'rainbow' will be called to
  generate the colors. If NA, the polygons will not be filled.

- axis4:

  option to add an axis on the right hand side.

- x.hash:

  values from x for which the bars have hash marks instead of solid fill

- density:

  density value for hashed areas

- ...:

  Additional arguments passed to 'plot'.

## References

<https://cran.r-project.org/package=plotrix>

## Author

Jim Lemon, Ian Taylor
