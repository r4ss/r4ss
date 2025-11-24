# Use 3.30 variance adjustments to create the 3.24 formatting

This functionality used to be in SS_readctl_3.30, but ware removed to
avoid confusion.

## Usage

``` r
translate_3.30_to_3.24_var_adjust(
  Variance_adjustment_list = NULL,
  Nfleets,
  fleetnames = seq_len(Nfleets)
)
```

## Arguments

- Variance_adjustment_list:

  The Variance_adjustments_list element in the control file r4ss list
  output generated from
  [SS_readctl](https://r4ss.github.io/r4ss/reference/SS_readctl.md).
  Defaults to NULL, which can be the case if no variance adjustments
  were included in the model.

- Nfleets:

  Number of fleets in the model

- fleetnames:

  Optional replacement for fleetnames used in data file.

## Value

A dataframe of 3.24 variance adjustments.
