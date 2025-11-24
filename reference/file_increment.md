# Rename key Stock Synthesis output files by adding integer value

Rename files found with `pattern` by adding `i` to their name before the
extension.

## Usage

``` r
file_increment(
  path,
  i,
  verbose = FALSE,
  pattern = "^[CcPRw][a-zA-Z]+\\.sso|summary\\.sso|\\.par$"
)
```

## Arguments

- path:

  Directory where model files are located.

- i:

  An integer value to append to the file name before the `.sso`
  extension.

- verbose:

  A logical value specifying if output should be printed to the screen.

- pattern:

  A character value specifying the file names to search for in
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

## Value

Invisibly returns a vector of logical values specifying whether or not
the file was successfully renamed.

## Details

The `.par` file, which is the only file extension searched for with the
default entry that does not end in `.sso`, is modified
differently.`_i.sso` is added to the file name.

## See also

[`jitter()`](https://r4ss.github.io/r4ss/reference/jitter.md)

## Author

Kelli F. Johnson
