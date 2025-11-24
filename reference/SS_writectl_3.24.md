# Deprecated: write 3.24 control file

write Stock Synthesis control file from list object in R which was
probably created using
[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md)

## Usage

``` r
SS_writectl_3.24(ctllist, outfile, overwrite = FALSE, verbose = FALSE)
```

## Arguments

- ctllist:

  List object created by
  [`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md).

- outfile:

  Filename for where to write new data file.

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Details

Support for 3.24 models within the r4ss `SS_read*` and `SS_write*()`
functions is ending, so please update models to 3.30.

## See also

[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readctl_3.24()`](https://r4ss.github.io/r4ss/reference/SS_readctl_3.24.md),[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),

## Author

Yukio Takeuchi, Kathryn L. Doering, Nathan R. Vaughan
