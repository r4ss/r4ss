# Read all Stock Synthesis input files for a model

Read all the input files for a Stock Synthesis model into R as a list
object. These files will be in a single directory on your machine, i.e.,
`dir`. Functionality comes from the `r4ss::SS_read*()` functions. This
function simplifies the number of lines of code you need to write by
using all of the read functions to read in the starter, control, data,
and forecast files and if requested, the weight-at-age file. The starter
file is helpful because it provides names for the control and data
files.

## Usage

``` r
SS_read(dir = getwd(), ss_new = FALSE, verbose = FALSE)
```

## Arguments

- dir:

  A file path to the directory of interest or a raw github URL (see
  example). The default is the current working directory,
  `dir = getwd()`.

- ss_new:

  A logical that controls if the `.ss_new` files or the original input
  files are read in. The default is to read the original files.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

An invisible list is returned. The first element (`dir`) is the
directory that was provided in the argument `dir`. The second element
(`path`) is the result of `normalizePath(dir)`, which gives the full
path. The remaining four to six elements are list objects from reading
in the following input files:

- data

- control

- starter

- forecast

- wtatage (will be NULL if not required by the model)

- par (will be NULL if not required by model or if control and par do
  not match)

## See also

[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md) can be
used to write the input files using the list created by this function.

Other read/write functions:
[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md),
[`SS_writectl()`](https://r4ss.github.io/r4ss/reference/SS_writectl.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md)

## Author

Ian G. Taylor, Kelli F. Johnson

## Examples

``` r
# Read in the 'simple' example model stored in {r4ss}
inputs <- SS_read(
  dir = system.file("extdata", "simple_small", package = "r4ss")
)
# Read in an example from GitHub stored in ss3-user-examples,
# wrapped in `dontrun` because it requires an Internet connection
if (FALSE) { # \dontrun{
webexample <- SS_read(dir = file.path(
  "https://raw.githubusercontent.com",
  "nmfs-ost",
  "ss3-user-examples",
  "main",
  "model_files",
  "simple_long_wtatage"
))
} # }
```
