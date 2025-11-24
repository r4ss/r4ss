# write control file for SS3 version 3.30

write Stock Synthesis control file from list object in R which was
created using
[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md).This
function is designed to be called using
[`SS_writectl()`](https://r4ss.github.io/r4ss/reference/SS_writectl.md)
and should not be called directly.

## Usage

``` r
SS_writectl_3.30(ctllist, outfile, overwrite = FALSE, verbose = FALSE)
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

## See also

[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readctl_3.30()`](https://r4ss.github.io/r4ss/reference/SS_readctl_3.30.md),[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md)

## Author

Kathryn L. Doering, Yukio Takeuchi, Neil Klaer, Watal M. Iwasaki, Nathan
R. Vaughan
