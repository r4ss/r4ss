# Populate multiple Stock Synthesis folders with input files

Creates a set of multiple folders and populates each with SS3 input
files such as for the purpose of running a new version of SS3 for an
existing set of test models.

## Usage

``` r
populate_multiple_folders(
  outerdir.old,
  outerdir.new,
  create.dir = TRUE,
  overwrite = FALSE,
  use_ss_new = FALSE,
  copy_par = FALSE,
  exe.dir = NULL,
  exe.file = "ss3",
  verbose = TRUE
)
```

## Arguments

- outerdir.old:

  Location of existing outer directory containing subdirectories for
  each model.

- outerdir.new:

  New outer directory into which the subfolders should be created.

- create.dir:

  Create new outer directory if it doesn't exist already?

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

- use_ss_new:

  Use .ss_new files instead of original inputs?

- copy_par:

  Copy any .par files found in the individual directories?

- exe.dir:

  Where to get executable to copy to each new subfolder. Options are

  - FALSE to not copy any executables,

  - TRUE to copy executables found in each existing subfolder to the
    corresponding new subfolder,

  - a path to a central location containing an executable to copy into
    each new subfolder.

- exe.file:

  Filename of executable to copy into all the subfolders.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

Returns a table of results indicating which directories were
successfully populated with the model input files and/or executables.

## See also

Other run functions:
[`copy_SS_inputs()`](https://r4ss.github.io/r4ss/reference/copy_SS_inputs.md),
[`jitter()`](https://r4ss.github.io/r4ss/reference/jitter.md),
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md),
[`retro()`](https://r4ss.github.io/r4ss/reference/retro.md),
[`run()`](https://r4ss.github.io/r4ss/reference/run.md),
[`tune_comps()`](https://r4ss.github.io/r4ss/reference/tune_comps.md)

## Author

Ian G. Taylor, Kelli F. Johnson

## Examples

``` r
if (FALSE) { # \dontrun{
populate_multiple_folders(
  outerdir.old = system.file("extdata", package = "r4ss"),
  outerdir.new = file.path(tempdir(), "test")
)
} # }
```
