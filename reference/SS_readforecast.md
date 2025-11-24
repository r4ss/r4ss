# read forecast file

read Stock Synthesis forecast file into list object in R

## Usage

``` r
SS_readforecast(
  file = "forecast.ss",
  Nfleets = NULL,
  Nareas = NULL,
  nseas = NULL,
  version = "3.30",
  readAll = FALSE,
  verbose = TRUE
)
```

## Arguments

- file:

  Filename either with full path or relative to working directory.

  See the formal arguments for a possible default filename.

- Nfleets:

  Number of fleets (not required in 3.30).

- Nareas:

  Number of areas (not required in 3.30).

- nseas:

  number of seasons (not required in 3.30).

- version:

  SS version number. Currently "3.24" or "3.30" are supported, either as
  character or numeric values (noting that numeric 3.30 = 3.3).
  `version = NULL` is no longer the default or an allowed entry. The
  default is `version = "3.30"`.

- readAll:

  Should the function continue even if Forecast = 0 or -1 (at which
  point SS stops reading)?

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

Other read/write functions:
[`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md),
[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md),
[`SS_writectl()`](https://r4ss.github.io/r4ss/reference/SS_writectl.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md)

## Author

Ian G. Taylor, Kelli F. Johnson, Kathryn L. Doering, Nathan R. Vaughan
