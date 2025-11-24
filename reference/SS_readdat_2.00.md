# Deprecated: read data file from SS version 2.00

Read Stock Synthesis (version 2.00) data file into list object in R.
This function was formerly called SS_readdat. That name is now used for
a wrapper function that calls either SS_readdat_2.00 SS_readdat_3.00
SS_readdat_3.24 or SS_readdat_3.30 (and potentially additional functions
in the future).

## Usage

``` r
SS_readdat_2.00(
  file,
  verbose = TRUE,
  echoall = lifecycle::deprecated(),
  section = NULL
)
```

## Arguments

- file:

  Filename either with full path or relative to working directory.

  See the formal arguments for a possible default filename.

- verbose:

  A logical value specifying if output should be printed to the screen.

- echoall:

  Deprecated.

- section:

  Which data set to read. Only applies for a data.ss_new file created by
  Stock Synthesis. Allows the choice of either expected values
  (section=2) or bootstrap data (section=3+). Leaving default of
  section=NULL will read input data, (equivalent to section=1). \##
  needs to be added

## Details

Support for 3.24 models within the r4ss `SS_read*` and `SS_write*()`
functions is ending, so please update models to 3.30.

## See also

[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
[`SS_readdat_3.30()`](https://r4ss.github.io/r4ss/reference/SS_readdat_3.30.md)
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md)

## Author

Ian G. Taylor, Yukio Takeuchi, Z. Teresa A'mar, Neil L. Klaer
