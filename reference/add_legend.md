# Add legend to plots

ss3diags function to add legend to plots

## Usage

``` r
add_legend(
  legendlabels,
  legendloc = "topleft",
  legendorder = NULL,
  legendncol = 1,
  legendcex = 1,
  legendsp = 0.9,
  col = NULL,
  pch = NULL,
  pt.cex = 1,
  lty = 1,
  lwd = 2,
  type = "o"
)
```

## Arguments

- legendlabels:

  Optional vector of labels to include in legend.

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- legendorder:

  Optional vector of model numbers that can be used to have the legend
  display the model names in an order that is different than that which
  is represented in the summary input object.

- legendncol:

  Number of columns for the legend.

- legendcex:

  Allows to adjust legend cex. Defaults to 1.

- legendsp:

  Space between legend labels

- col:

  Optional vector of colors to be used for lines. Input NULL

- pch:

  Optional vector of plot character values

- pt.cex:

  Adjust the cex of points.

- lty:

  Optional vector of line types

- lwd:

  Optional vector of line widths

- type:

  Type parameter passed to points (default 'o' overplots points on top
  of lines)
