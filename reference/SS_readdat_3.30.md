# read data file from SS version 3.30

Read Stock Synthesis (version 3.30) data file into list object in R.
This function was formerly called SS_readdat. That name is now used for
a wrapper function that calls either SS_readdat_3.24 or SS_readdat_3.30
(and potentially additional functions in the future).

## Usage

``` r
SS_readdat_3.30(
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
  section=NULL will read input data, (equivalent to section=1).

## See also

[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
`SS_readdat_3.30()`
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md)

## Author

Ian G. Taylor, Yukio Takeuchi, Z. Teresa A'mar, Chris J. Grandin, Kelli
F. Johnson, Chantel R. Wetzel, Kathryn L. Doering, Nathan R. Vaughan
