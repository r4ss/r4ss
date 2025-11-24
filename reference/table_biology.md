# Create a table of biology for assessment reporting: length, weight, % mature, fecundity, and selectivity

Takes the object created by
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md) to
create table for reporting biology at age. This function was formerly
known as `SStablebiology()` but that older version is now deprecated.

## Usage

``` r
table_biology(
  replist,
  dir = NULL,
  fleetnames = NULL,
  selexyr = NULL,
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

- fleetnames:

  Optional replacement for fleetnames used in data file.

- selexyr:

  The year to summarize selectivity, the default is the final model
  year.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

Individual .rda files containing a list of table and caption

## See also

Other table functions:
[`table_all()`](https://r4ss.github.io/r4ss/reference/table_all.md),
[`table_compweight()`](https://r4ss.github.io/r4ss/reference/table_compweight.md),
[`table_config()`](https://r4ss.github.io/r4ss/reference/table_config.md),
[`table_exec_summary()`](https://r4ss.github.io/r4ss/reference/table_exec_summary.md),
[`table_parcounts()`](https://r4ss.github.io/r4ss/reference/table_parcounts.md),
[`table_pars()`](https://r4ss.github.io/r4ss/reference/table_pars.md),
[`table_ts()`](https://r4ss.github.io/r4ss/reference/table_ts.md)

## Author

Chantel R. Wetzel
