# Get the tuning table

Get the tuning table

## Usage

``` r
get_tuning_table(
  replist,
  fleets,
  option,
  digits = 6,
  write = TRUE,
  verbose = TRUE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- fleets:

  A vector of fleet numbers

- option:

  Which type of tuning: 'none', 'Francis', 'MI', or 'DM'

- digits:

  Number of digits to round numbers to

- write:

  Write suggested tunings to a file 'suggested_tunings.ss'

- verbose:

  A logical value specifying if output should be printed to the screen.
