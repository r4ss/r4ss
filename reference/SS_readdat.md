# read Stock Synthesis data file

Read Stock Synthesis data file into list object in R. This function is a
wrapper which calls SS_readdat_3.30 (previously additional functions,
but they have been deprecated).

## Usage

``` r
SS_readdat(
  file,
  version = "3.30",
  verbose = TRUE,
  echoall = lifecycle::deprecated(),
  section = NULL
)
```

## Arguments

- file:

  Filename either with full path or relative to working directory.

  See the formal arguments for a possible default filename.

- version:

  SS version number. Currently "2.00", "3.00", "3.24" or "3.30" are
  supported, but all versions prior to "3.30" have been deprecated.
  either as character or numeric values (noting that numeric 3.30 =
  3.3). If version is NULL, the version (3.24 or 3.30) will be looked
  for on the first line of the file.

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

Other read/write functions:
[`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md),
[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md),
[`SS_writectl()`](https://r4ss.github.io/r4ss/reference/SS_writectl.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md)

## Author

Ian G. Taylor, Allan C. Hicks, Neil L. Klaer, Kelli F. Johnson, Chantel
R. Wetzel, Kathryn L. Doering, Nathan R. Vaughan
