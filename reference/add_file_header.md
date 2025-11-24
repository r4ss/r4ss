# Add header comments to the input files

Lines starting with \#C at the top of the file are carried over to the
\*.ss_new files by Stock Synthesis This function modifies any existing
header to add or replace lines written by r4ss that state the write time
of the file.

## Usage

``` r
add_file_header(filelist, con)
```

## Arguments

- filelist:

  An object created by one of the r4ss::SS_read\* functions.

- con:

  File to write to (passed to `con` input to
  [`writeLines()`](https://rdrr.io/r/base/writeLines.html))

## Author

Yukio Takeuchi, Ian G. Taylor
