# Copy a the Stock Synthesis input files from one directory to another

Reads the starter.ss file to figure out the names of the control and
data files, than copies those files along with starter.ss, forecast.ss,
and wtatage.ss (if present) to a new directory, as specified.

## Usage

``` r
copy_SS_inputs(
  dir.old = NULL,
  dir.new = NULL,
  create.dir = TRUE,
  overwrite = FALSE,
  recursive = FALSE,
  use_ss_new = FALSE,
  copy_exe = FALSE,
  copy_par = FALSE,
  dir.exe = NULL,
  verbose = TRUE
)
```

## Arguments

- dir.old:

  Location of model files to be copied, either an absolute path or
  relative to the working directory.

- dir.new:

  New location to which the files should be copied, either an absolute
  path or relative to the working directory.

- create.dir:

  Create `dir.new` directory if it doesn't exist already?

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

- recursive:

  A logical value passed to the `recursive` argument of
  [`dir.create()`](https://rdrr.io/r/base/files2.html) that specifies if
  elements of the path other than the last be created?

- use_ss_new:

  Use .ss_new files instead of original inputs?

- copy_exe:

  Copy any executables found in `dir.old` to `dir.new` or dir.exe (if
  provided)?

- copy_par:

  Copy any .par files found in `dir.old` to `dir.new`?

- dir.exe:

  Path to executable to copy instead of any in `dir.old`.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

A logical value is invisibly returned, indicating whether all input
files were copied successfully.

## See also

Other run functions:
[`jitter()`](https://r4ss.github.io/r4ss/reference/jitter.md),
[`populate_multiple_folders()`](https://r4ss.github.io/r4ss/reference/populate_multiple_folders.md),
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md),
[`retro()`](https://r4ss.github.io/r4ss/reference/retro.md),
[`run()`](https://r4ss.github.io/r4ss/reference/run.md),
[`tune_comps()`](https://r4ss.github.io/r4ss/reference/tune_comps.md)

## Author

Ian G. Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
# A theoretical example if "old_model" was present
# but expect an error
copy_SS_inputs(
  dir.old = "c:/SS/old_model",
  dir.new = "c:/SS/new_model"
)
# A working example using files stored in {r4ss}
copy_SS_inputs(
  dir.old = system.file("extdata", "simple_small", package = "r4ss"),
  dir.new = "test"
)
unlink(test, recursive = TRUE)
} # }
```
