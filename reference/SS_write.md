# Write all Stock Synthesis input files for a model

Writes all the input files for a Stock Synthesis model using the list
created by
[`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md)
(presumably after modification of one or more elements) using the
`SS_write*()` functions for the four to six model input files.

## Usage

``` r
SS_write(inputlist, dir = "", overwrite = FALSE, verbose = FALSE)
```

## Arguments

- inputlist:

  list created by
  [`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md)

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

[`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md) creates
the list that is used by this function.

Other read/write functions:
[`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md),
[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_writectl()`](https://r4ss.github.io/r4ss/reference/SS_writectl.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md)

## Author

Ian G. Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
# read inputlist to modify the data file
inputlist <- SS_read(
  dir = system.file("extdata", "simple_small", package = "r4ss")
)

# modify the starter file (use the par file)
inputlist[["start"]][["init_values_src"]] <- 1

# modify the data file (remove age comps from years prior to 1990)
inputlist[["dat"]][["agecomp"]] <- inputlist[["dat"]][["agecomp"]] |>
  dplyr::filter(Yr >= 1990)

# modify the control file (turn off early recdevs and change range of yrs)
inputlist[["ctl"]][["recdev_early_phase"]] <-
  -abs(inputlist[["ctl"]][["recdev_early_phase"]])
inputlist[["ctl"]][["MainRdevYrFirst"]] <- 1980

# write the files to a new folder within the source directory
SS_write(
  inputlist = inputlist,
  dir = file.path(inputlist[["dir"]], "modified_inputs")
)
} # }
```
