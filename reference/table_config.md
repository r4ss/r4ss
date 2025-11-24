# Create a table to summarize the configuration of the SS3 model

Create a table to summarize the configuration of the SS3 model

## Usage

``` r
table_config(replist, dir = NULL, verbose = TRUE)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- dir:

  Directory where the .rda files will be written. The default value is
  NULL where a table folder will be created where the Report.sso file is
  located associated with `replist`.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

Returns invisibly and optionally saves an .rda files containing a list
of table and caption

## See also

Other table functions:
[`table_all()`](https://r4ss.github.io/r4ss/reference/table_all.md),
[`table_biology()`](https://r4ss.github.io/r4ss/reference/table_biology.md),
[`table_compweight()`](https://r4ss.github.io/r4ss/reference/table_compweight.md),
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
table_config(output)

# compare configuration for multiple models
config1 <- table_config(output1)
config2 <- table_config(output2)
table_compare <- data.frame(
  new_model = config1[["table"]],
  old_model = config2[["table"]][["Configuration"]]
)
names(table_compare) <- c("Section", "New model", "Old model")
} # }
```
