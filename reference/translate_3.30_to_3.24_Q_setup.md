# Use 3.30 q options to create the 3.24 q setup

Use 3.30 q options to create the 3.24 q setup

## Usage

``` r
translate_3.30_to_3.24_Q_setup(
  Q_options,
  Nfleets,
  fleetnames = seq_len(Nfleets)
)
```

## Arguments

- Q_options:

  The Q options list element in the 3.30 control file r4ss list output
  generated from
  [SS_readctl](https://r4ss.github.io/r4ss/reference/SS_readctl.md).

- Nfleets:

  Number of fleets in the model

- fleetnames:

  Optional replacement for fleetnames used in data file.

## Value

A dataframe containing the 3.24 Q setup.
