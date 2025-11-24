# Deprecated: write data file for SS version 3.24

Write Stock Synthesis data file from list object in R which was probably
created using
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md)
(which would have called on
[`SS_readdat_3.24()`](https://r4ss.github.io/r4ss/reference/SS_readdat_3.24.md)).

## Usage

``` r
SS_writedat_3.24(
  datlist,
  outfile,
  overwrite = FALSE,
  faster = lifecycle::deprecated(),
  verbose = TRUE
)
```

## Arguments

- datlist:

  List object created by
  [`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md).

- outfile:

  Filename for where to write new data file.

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

- faster:

  Deprecated. Speed up writing by writing length and age comps without
  aligning the columns (by using write.table instead of
  print.data.frame)

- verbose:

  A logical value specifying if output should be printed to the screen.

## Details

Support for 3.24 models within the r4ss `SS_read*` and `SS_write*()`
functions is ending, so please update models to 3.30.

## See also

[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writedat_3.30()`](https://r4ss.github.io/r4ss/reference/SS_writedat_3.30.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md)

## Author

Ian G. Taylor, Yukio Takeuchi, Gwladys I. Lambert, Kelli F. Johnson,
Chantel R. Wetzel
