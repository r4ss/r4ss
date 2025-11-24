# Read in a weight-at-age data file as a data frame

Read in a weight-at-age data file as a data frame

## Usage

``` r
SS_readwtatage(file = "wtatage.ss", verbose = TRUE)
```

## Arguments

- file:

  Filename either with full path or relative to working directory.

  See the formal arguments for a possible default filename.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

Returns a data frame with a variable number of columns based on the
number of ages that are included in the file. Though, the first columns
will always be `year`, `seas`, `sex`, `bio_pattern`, `birthSeas`, and
`fleet`. The seventh column will be age zero. The last or next to last
column will be the maximum age included in the weight-at-age data. For
Stock Synthesis versions 3.30 and greater, the last column will be a
column of comments.

`NULL` is returned if `file` does not exist or if the file does exist
but it is empty. This behavior is different than other `SS_read*`
functions that error if either of the previous checks are `TRUE`. Thus,
a complicated check involving
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) is used around
[`readLines()`](https://rdrr.io/r/base/readLines.html) to allow for
returning `NULL` rather than
[`stop()`](https://rdrr.io/r/base/stop.html). Additionally, this check
accommodates a URL for `file`.

## Author

Kelli F. Johnson, Ian G. Taylor
