# Make shaded polygons with a mountain-like appearance

Designed to replicate like the cool-looking Figure 7 in Butterworth et
al. (2003).

## Usage

``` r
mountains(
  zmat,
  xvec = NULL,
  yvec = NULL,
  zscale = 3,
  rev = TRUE,
  nshades = 100,
  axes = TRUE,
  xaxs = "i",
  yaxs = "i",
  xlab = "",
  ylab = "",
  las = 1,
  addbox = FALSE,
  ...
)
```

## Arguments

- zmat:

  A matrix where the rows represent the heights of each mountain range

- xvec:

  Optional input for the x variable

- yvec:

  Optional input for the y variable

- zscale:

  Controls the height of the mountains relative to the y-axis and
  max(zmat)

- rev:

  Reverse the order of the display of yvec values.

- nshades:

  Number of levels of shading

- axes:

  Add axes to the plot?

- xaxs:

  X-axis as internal or regular (see ?par for details)

- yaxs:

  Y-axis as internal or regular (see ?par for details)

- xlab:

  Optional label for x-axis

- ylab:

  Optional label for y-axis

- las:

  Xxis label style (see ?par for details). Default = 1 = horizontal axis
  labels.

- addbox:

  Puts a box around the whole plot

- ...:

  Extra inputs passed to the plot command

## References

Butterworth D.S., Ianelli J.N., Hilborn R. (2003) A statistical model
for stock assessment of southern bluefin tuna with temporal changes in
selectivity. South African Journal of Marine Science 25:331-362.

## Author

Ian Taylor
