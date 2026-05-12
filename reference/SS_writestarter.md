# write starter file

write Stock Synthesis starter file from list object in R which was
probably created using
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md)

## Usage

``` r
SS_writestarter(
  mylist,
  dir = NULL,
  file = "starter.ss",
  overwrite = FALSE,
  verbose = TRUE,
  warn = lifecycle::deprecated()
)
```

## Arguments

- mylist:

  List object created by
  [`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md).

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- file:

  Filename for new starter file. Default="starter.ss".

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

- verbose:

  A logical value specifying if output should be printed to the screen.

- warn:

  Deprecated.

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
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md)

## Author

Ian G. Taylor, Kelli F. Johnson, Kathryn R. Doering
