# Table of parameters

Table of parameters

## Usage

``` r
table_pars(
  replist,
  dir = NULL,
  rows = NULL,
  caption =
    "Parameter estimates, estimation phase, parameter bounds, estimation status, estimated standard deviation (SD), prior information [distribution(mean, SD)] used in the base model.",
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

- rows:

  Which rows to include from the `parameters` table created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).
  NULL will cause all rows to be included.

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
[`table_ts()`](https://r4ss.github.io/r4ss/reference/table_ts.md)

## Author

Kelli F. Johnson
