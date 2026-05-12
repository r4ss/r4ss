# Write Stock Synthesis control file

Write Stock Synthesis control file from list object in R which was
probably created using
[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md).
This function is a wrapper which calls SS_writectl_3.30() (previously
also SS_writectl_3.24, but that function has been deprecated).

## Usage

``` r
SS_writectl(
  ctllist,
  outfile,
  version = "3.30",
  overwrite = FALSE,
  verbose = FALSE
)
```

## Arguments

- ctllist:

  List object created by
  [`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md).

- outfile:

  Filename for where to write new control file.

- version:

  SS version number. Currently "3.24" or "3.30" are supported, either as
  character or numeric values (noting that numeric 3.30 = 3.3).
  `version = NULL` is no longer the default or an allowed entry. The
  default is `version = "3.30"`.

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

Other read/write functions:
[`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md),
[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md)

## Author

Ian G. Taylor, Yukio Takeuchi, Gwladys I. Lambert, Kathryn L. Doering,
Nathan R. Vaughan
