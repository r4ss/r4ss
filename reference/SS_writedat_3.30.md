# write data file for SS version 3.30

Write Stock Synthesis data file from list object in R which was probably
created using
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md)
(which would have called on
[`SS_readdat_3.30()`](https://r4ss.github.io/r4ss/reference/SS_readdat_3.30.md)).

## Usage

``` r
SS_writedat_3.30(
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

## See also

[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writedat_3.24()`](https://r4ss.github.io/r4ss/reference/SS_writedat_3.24.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md)

## Author

Ian G. Taylor, Yukio Takeuchi, Gwladys I. Lambert, Kelli F. Johnson,
Chantel R. Wetzel, Kathryn L. Doering, Nathan R. Vaughan
