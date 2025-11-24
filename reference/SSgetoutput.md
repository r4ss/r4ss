# Get output from multiple Stock Synthesis models.

Apply the function
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)
multiple times and save output as individual objects or a list of lists.

## Usage

``` r
SSgetoutput(
  keyvec = NULL,
  dirvec = NULL,
  getcovar = TRUE,
  getcomp = TRUE,
  forecast = TRUE,
  verbose = TRUE,
  listlists = lifecycle::deprecated(),
  underscore = FALSE,
  save.lists = FALSE,
  SpawnOutputLabel = "Spawning output",
  modelnames = NULL
)
```

## Arguments

- keyvec:

  A vector of strings that are appended to the output files from each
  model if models are all in one directory. Default=NULL.

- dirvec:

  A vector of directories (full path or relative to working directory)
  in which model output is located. Default=NULL.

- getcovar:

  Choice to read or not read covar.sso output (saves time and memory).
  Default=TRUE.

- getcomp:

  Choice to read or not read CompReport.sso output (saves time and
  memory). Default=TRUE.

- forecast:

  Choice to read or not read forecast quantities. Default=FALSE.

- verbose:

  A logical value specifying if output should be printed to the screen.

- listlists:

  Deprecated argument that wasn't working.

- underscore:

  Add an underscore '\_' between any file names and any keys in keyvec.
  Default=FALSE.

- save.lists:

  Save each list of parsed output as a .Rdata file (with default
  filenaming convention based on iteration and date stamp.

- SpawnOutputLabel:

  An alternative to "Spawning output" for use in figure axis labels and
  table headers for models that include a fecundity relationship. This
  provides an option to provide the units, e.g.
  `SpawnOutputLabel = "Spawning output (trillions of eggs)"`. This needs
  to be a user input because the units depend on the choice of fecundity
  parameters which are calculated outside of the SS3 model.

## See also

[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

Other model comparison functions:
[`SSplotComparisons()`](https://r4ss.github.io/r4ss/reference/SSplotComparisons.md),
[`SSsummarize()`](https://r4ss.github.io/r4ss/reference/SSsummarize.md),
[`SStableComparisons()`](https://r4ss.github.io/r4ss/reference/SStableComparisons.md)

## Author

Ian Taylor

## Examples

``` r
# contrived example where the same model is read twice
mydir <- file.path(
  path.package("r4ss"),
  file.path("extdata", "simple_small")
)
models <- SSgetoutput(
  dirvec = c(mydir, mydir),
  modelnames = c("simple_small", "simple_small_again")
)
#> length(dirvec) as input to SSgetoutput: 2
#> reading output from /home/runner/work/_temp/Library/r4ss/extdata/simple_small/Report.sso
#> added element 'simple_small' to list
#> reading output from /home/runner/work/_temp/Library/r4ss/extdata/simple_small/Report.sso
#> added element 'simple_small_again' to list
```
