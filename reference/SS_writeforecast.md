# write forecast file

write Stock Synthesis forecast file from list object in R which was
probably created using
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md)

## Usage

``` r
SS_writeforecast(
  mylist,
  dir = NULL,
  file = "forecast.ss",
  writeAll = FALSE,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- mylist:

  List object created by
  [`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md).

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- file:

  Filename for new forecast file. Default="forecast.ss".

- writeAll:

  Should the function continue even if Forecast=0 (at which point SS
  stops reading, and remaining elements in list may not be available,
  depending on settings used in SS_readforecast)

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
[`SS_writectl()`](https://r4ss.github.io/r4ss/reference/SS_writectl.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md)

## Author

Ian G. Taylor, Kelli F. Johnson, Kathryn L. Doering, Nathan R. Vaughan
