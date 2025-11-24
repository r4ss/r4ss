# Function to pull values and calculate confidence intervals from model output called from `r4ss::table_exec_summary()`.

Function to pull values and calculate confidence intervals from model
output called from
[`r4ss::table_exec_summary()`](https://r4ss.github.io/r4ss/reference/table_exec_summary.md).

## Usage

``` r
get_values(replist, label, yrs, ci_value, single = FALSE)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- label:

  The parameter name to calculate confidence intervals for. The name is
  based on the names in the `replist` object.

- yrs:

  Vector of years to calculate confidence intervals for.

- ci_value:

  To calculate confidence intervals, the desired interval must be
  specified. The default is 0.95.

- single:

  Calculate the confidence interval for a single year or parameter. The
  default is FALSE.

## Value

data frame with point estimate and confidence interval low and high
values

## Author

Chantel R. Wetzel, Kelli F. Johnson, Ian G. Taylor
