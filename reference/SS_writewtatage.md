# Write weight-at-age file

Write Stock Synthesis weight-at-age file from R object that was probably
created using
[`SS_readwtatage()`](https://r4ss.github.io/r4ss/reference/SS_readwtatage.md)

## Usage

``` r
SS_writewtatage(
  mylist,
  dir = NULL,
  file = "wtatage.ss",
  overwrite = FALSE,
  verbose = TRUE,
  warn = lifecycle::deprecated()
)
```

## Arguments

- mylist:

  Object created by
  [`SS_readwtatage()`](https://r4ss.github.io/r4ss/reference/SS_readwtatage.md).

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- file:

  Filename for new weight-at-age file, which will be appended to `dir`
  to create a full file path. Default="wtatage.ss".

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

- verbose:

  A logical value specifying if output should be printed to the screen.

- warn:

  Deprecated.

## See also

[`SS_readwtatage()`](https://r4ss.github.io/r4ss/reference/SS_readwtatage.md)

## Author

Kelli F. Johnson
