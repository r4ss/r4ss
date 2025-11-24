# Show movement rates on a map.

Make a map with colored spatial cells and add arrows representing
movement rates between cells.

## Usage

``` r
SSplotMovementMap(
  replist = NULL,
  xlim,
  ylim,
  polygonlist,
  colvec,
  land = "grey",
  xytable = NULL,
  moveage = 5,
  moveseas = 1,
  lwdscale = 5,
  legend = TRUE,
  title = NULL,
  areanames = NULL,
  cex = 1
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- xlim:

  range of longitude values in the map

- ylim:

  range of latitude values in the map

- polygonlist:

  a list of data frames, each with two columns representing the
  longitude and latitude values of the colored polygons. The order of
  elements in the list should match the numbering of areas in the SS
  model.

- colvec:

  vector of colors for each polygon (if `replist` is provided)

- land:

  color of landmasses in the map

- xytable:

  data frame of latitude and longitude values which will be connected by
  the arrows representing movement rates. The order should match the
  order of areas in `polygonlist` and in the SS model. Not necessary if
  no arrows are shown on the map.

- moveage:

  age for which movement rates will be represented

- moveseas:

  season for which movement rates will be represented

- lwdscale:

  scaling factor for arrows in the plot. The largest rate of movement
  shown will be scaled to have a line width equal to this value.

- legend:

  add a legend to show the movement rate associated with the widest
  arrows

- title:

  optional title to be added above map

- areanames:

  optional vector of names to be shown on map at coordinates matching
  xytable values

- cex:

  character expansion to apply to text shown by areanames (if used)

## Note

Inspired by plots of MULTIFAN-CL movement patterns presented by Adam
Langley

## See also

[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md),
[`SSplotMovementRates()`](https://r4ss.github.io/r4ss/reference/SSplotMovementRates.md)

## Author

Ian Taylor
