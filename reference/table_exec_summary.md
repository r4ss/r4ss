# Create executive summary tables from an SS3 Report.sso file

Take the output from
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md) and
create executive summary .rda files as required by the current Terms of
Reference for U.S. West Coast groundfish assessments. Additionally, .rda
files of historical catches, time-series, and numbers-at-age are
created. A CSV file with captions is also created. This function is
modified from
[`SSexecutivesummary()`](https://r4ss.github.io/r4ss/reference/SSexecutivesummary.md)
associated with the adoption of the asar template.

## Usage

``` r
table_exec_summary(
  replist,
  dir = NULL,
  ci_value = 0.95,
  fleetnames = NULL,
  so_units = "biomass (mt)",
  endyr = NULL,
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

- ci_value:

  To calculate confidence intervals, the desired interval must be
  specified. The default is 0.95.

- fleetnames:

  String of fleet names. Default is NULL which will use the the model
  fleet names.

- so_units:

  A single character object specifying the unit of measurement that
  spawning output is reported in. The default is "millions of eggs".
  This text will be used in the table captions. If fecundity is equal to
  weight-at-length, then the units are hard-wired to `"mt"` regardless
  of what is used within this argument.

- endyr:

  Optional input to choose a different ending year for tables, which
  could be useful for catch-only updates. The default is `NULL`, which
  leads to using the ending year defined in Report.sso.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

Individual .rda files containing a list of table and caption

## See also

Other table functions:
[`table_all()`](https://r4ss.github.io/r4ss/reference/table_all.md),
[`table_biology()`](https://r4ss.github.io/r4ss/reference/table_biology.md),
[`table_compweight()`](https://r4ss.github.io/r4ss/reference/table_compweight.md),
[`table_config()`](https://r4ss.github.io/r4ss/reference/table_config.md),
[`table_parcounts()`](https://r4ss.github.io/r4ss/reference/table_parcounts.md),
[`table_pars()`](https://r4ss.github.io/r4ss/reference/table_pars.md),
[`table_ts()`](https://r4ss.github.io/r4ss/reference/table_ts.md)

## Author

Chantel R. Wetzel, Kelli F. Johnson, Ian G. Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
### running function
# read model output
model <- SS_output(...)
# run this function
tables <- table_exec_summary(model)
# load an individual table (list added to workspace includes
# the table and caption)
load(file.path(model[["inputs"]][["dir"]], "tables", "projections.rda"))

### modify projections table for model that falls below target
# get table from list saved above
tab <- tables[["projections"]][["table"]]
# get buffers from forecast file
buffers <- inputs[["fore"]][["Flimitfraction_m"]]
# which rows in the table are missing buffer and ABC values?
NA_rows <- is.na(tab[["Buffer"]])
# fill in Buffer column from forecast file values
tab[["Buffer"]][NA_rows] <- buffers[["fraction"]][match(tab[["Year"]][NA_rows], buffers[["year"]])]
# fill in ABC as product of Buffer and OFL
tab[NA_rows, "ABC (mt)"] <- tab[["Buffer"]][NA_rows] * tab$"OFL (mt)"[NA_rows]
} # }
```
