# Extract total catch, spawning output, and fraction unfished from forecast years

Values of total catch, spawning output, and fraction unfished are
extracted from the forecast years of a time series table for inclusion
in a decision table.

## Usage

``` r
SS_decision_table_stuff(
  replist,
  yrs = 2025:2036,
  digits = c(0, 0, 3),
  OFL = FALSE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- yrs:

  Range of years from which to extract values

- digits:

  Vector of number of digits to round to in table for

  - 1 catch

  - 2 spawning output

  - 3 fraction unfished (column is called "depl")

- OFL:

  Logical indicating whether to include the overfishing limit (OFL)
  instead of spawning output in the table. Defaults to `FALSE`.

## Value

A tibble with columns for year, total catch (dead biomass), spawning
output, and fraction unfished.

## See also

[`SS_ForeCatch()`](https://r4ss.github.io/r4ss/reference/SS_ForeCatch.md)

## Author

Ian G. Taylor
