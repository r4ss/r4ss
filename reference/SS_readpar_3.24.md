# Deprecated: read ss.par file from SS version 3.24

Read Stock Synthesis (version 3.24) parameter file into list object in
R.

## Usage

``` r
SS_readpar_3.24(parfile, datsource, ctlsource, verbose = TRUE)
```

## Arguments

- parfile:

  Filename either with full path or relative to working directory.

- datsource:

  list or character. If list, should be a list produced from
  [`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md).
  If character, should be the full file location of an SS data file.

- ctlsource:

  list or character. If list, should be a list produced from
  [`SS_writectl()`](https://r4ss.github.io/r4ss/reference/SS_writectl.md).
  If character, should be the full file location of an SS control file.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Details

Support for 3.24 models within the r4ss `SS_read*` and `SS_write*()`
functions is ending, so please update models to 3.30.

## See also

[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md)
[`SS_readdat_3.24()`](https://r4ss.github.io/r4ss/reference/SS_readdat_3.24.md),[`SS_readdat_3.24()`](https://r4ss.github.io/r4ss/reference/SS_readdat_3.24.md)
[`SS_readctl_3.24()`](https://r4ss.github.io/r4ss/reference/SS_readctl_3.24.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md)

## Author

Nathan R. Vaughan
