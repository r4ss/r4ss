# Plot uncertainty around chosen selectivity ogive from MCMC.

Plot uncertainty in selectivity from an MCMC output for whichever
fleet/year was chosen in the optional extra "more stddev reporting"

## Usage

``` r
SSplotMCMC_ExtraSelex(
  post,
  add = FALSE,
  nsexes = 1,
  shift = 0,
  fleetname = "default",
  col = "blue"
)
```

## Arguments

- post:

  A data frame containing either derived_posteriors.sso or a good subset
  of it. This can be an element of the list created by the the
  [`SSgetMCMC()`](https://r4ss.github.io/r4ss/reference/SSgetMCMC.md)
  function.

- add:

  TRUE/FALSE option to add results to an existing plot.

- nsexes:

  Number of sexes in the model (should match model values but is only
  used in the title).

- shift:

  Optional adjustment to the x values to avoid overlap of intervals when
  overplotting on an existing plot.

- fleetname:

  Optional input to make the title better. Default will be something
  like "Fleet 1", using the numbering from the model.

- col:

  Color for points and lines.

## Author

Ian Taylor
