# Table of length and age comp Francis weights (doesn't yet include other data types)

Table of length and age comp Francis weights (doesn't yet include other
data types)

## Usage

``` r
table_compweight(
  replist,
  dir = NULL,
  caption = paste("Data weightings applied to compositions",
    "according to the `Francis` method. `Obs.` refers to the number of unique",
    "composition vectors included in the likelihood. `N input` and `N adj.`",
    "refer to the sample sizes of those vectors before and after being adjusted",
    "by the the weights."),
  caption_CAAL = "`CAAL` is conditional age-at-length data.",
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

- caption_CAAL:

  Additional text added to the caption for models with conditional age
  at length data.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

A list containing the table and caption.

## See also

Other table functions:
[`table_all()`](https://r4ss.github.io/r4ss/reference/table_all.md),
[`table_biology()`](https://r4ss.github.io/r4ss/reference/table_biology.md),
[`table_config()`](https://r4ss.github.io/r4ss/reference/table_config.md),
[`table_exec_summary()`](https://r4ss.github.io/r4ss/reference/table_exec_summary.md),
[`table_parcounts()`](https://r4ss.github.io/r4ss/reference/table_parcounts.md),
[`table_pars()`](https://r4ss.github.io/r4ss/reference/table_pars.md),
[`table_ts()`](https://r4ss.github.io/r4ss/reference/table_ts.md)

## Author

Kelli F. Johnson, Ian G. Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the model output
output <- r4ss::SS_output()
# Create the table
table_compweight(output)
} # }
```
