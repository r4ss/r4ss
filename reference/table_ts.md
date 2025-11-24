# Table of estimated time series

Adapted from Lingcod 2021:
https://github.com/pfmc-assessments/lingcod/blob/main/R/table_ts.R

## Usage

``` r
table_ts(
  replist,
  dir = NULL,
  caption = "Time series of population estimates for the base model.",
  verbose = TRUE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- dir:

  Directory where the .rda files will be written. The default value is
  NULL where a table folder will be created where the Report.sso file is
  located associated with `replist`.

- caption:

  A character string for the caption.

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

Other table functions:
[`table_all()`](https://r4ss.github.io/r4ss/reference/table_all.md),
[`table_biology()`](https://r4ss.github.io/r4ss/reference/table_biology.md),
[`table_compweight()`](https://r4ss.github.io/r4ss/reference/table_compweight.md),
[`table_config()`](https://r4ss.github.io/r4ss/reference/table_config.md),
[`table_exec_summary()`](https://r4ss.github.io/r4ss/reference/table_exec_summary.md),
[`table_parcounts()`](https://r4ss.github.io/r4ss/reference/table_parcounts.md),
[`table_pars()`](https://r4ss.github.io/r4ss/reference/table_pars.md)

## Author

Kelli F. Johnson, Ian G. Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
table_ts(model)
} # }
```
