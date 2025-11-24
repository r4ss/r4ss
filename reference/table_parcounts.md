# Summarize the estimated parameters based on an SS3 control file

Requires either

## Usage

``` r
table_parcounts(
  replist,
  inputs = NULL,
  dir = NULL,
  caption = "Estimated parameters in the model.",
  verbose = TRUE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- inputs:

  An options list of inputs from the SS3 model created by
  [`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md). If
  NULL, then the function will run
  `SS_read(replist[["inputs"]][["dir"]])` to get the inputs.

- dir:

  Directory where the .rda files will be written. The default value is
  NULL where a table folder will be created where the Report.sso file is
  located associated with `replist`.

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

Other table functions:
[`table_all()`](https://r4ss.github.io/r4ss/reference/table_all.md),
[`table_biology()`](https://r4ss.github.io/r4ss/reference/table_biology.md),
[`table_compweight()`](https://r4ss.github.io/r4ss/reference/table_compweight.md),
[`table_config()`](https://r4ss.github.io/r4ss/reference/table_config.md),
[`table_exec_summary()`](https://r4ss.github.io/r4ss/reference/table_exec_summary.md),
[`table_pars()`](https://r4ss.github.io/r4ss/reference/table_pars.md),
[`table_ts()`](https://r4ss.github.io/r4ss/reference/table_ts.md)

## Author

Ian G. Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the model output
output <- r4ss::SS_output()
# Create the table
table_parcounts <- table_parcounts(output)
# filter for types with at least one estimated parameter
table_parcounts[["table"]] |> dplyr::filter(Count > 0)
} # }
```
