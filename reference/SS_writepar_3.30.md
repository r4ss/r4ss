# write .par file from SS version 3.30

Write Stock Synthesis (version 3.30) parameter file from list object in
R to file.

## Usage

``` r
SS_writepar_3.30(parlist, outfile, overwrite = TRUE, verbose = FALSE)
```

## Arguments

- parlist:

  List object created by
  [`SS_readpar_3.30()`](https://r4ss.github.io/r4ss/reference/SS_readpar_3.30.md).

- outfile:

  Filename for where to write new parameter file.

- overwrite:

  Should existing files be overwritten? Default=TRUE.

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

[`SS_readpar_3.30()`](https://r4ss.github.io/r4ss/reference/SS_readpar_3.30.md),
[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md)
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_writectl()`](https://r4ss.github.io/r4ss/reference/SS_writectl.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md)

## Author

Nathan R. Vaughan
