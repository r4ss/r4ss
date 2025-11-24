# Read Stock Synthesis starter file as a list

Read Stock Synthesis starter file as a list

## Usage

``` r
SS_readstarter(file = "starter.ss", verbose = TRUE)
```

## Arguments

- file:

  Filename either with full path or relative to working directory.

  See the formal arguments for a possible default filename.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

A list with one element for each line of input values. List elements
containing the name of the control and data file are particularly
helpful, i.e., `ctlfile` and `datfile`, respectively.

## See also

Other read/write functions:
[`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md),
[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md),
[`SS_writectl()`](https://r4ss.github.io/r4ss/reference/SS_writectl.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md)

## Author

Ian G. Taylor, Kathryn L. Doering, Kelli F. Johnson

## Examples

``` r
starter_list <- SS_readstarter(
  system.file("extdata", "simple_small", "starter.ss",
    package = "r4ss"
  ),
  verbose = FALSE
)

# The following lines should be TRUE and demonstrate how you can know the
# names of the control and data file given information in the starter file.
starter_list[["ctlfile"]] == "simple_control.ss"
#> [1] FALSE
starter_list[["datfile"]] == "simple_data.ss"
#> [1] FALSE
```
