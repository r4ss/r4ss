# Run a Stock Synthesis model

The `run()` function checks for the executable via
[`check_exe()`](https://r4ss.github.io/r4ss/reference/check_exe.md).
This involves first checking the specified directory `dir` for the
specified SS3 executable name. If it is not found in the specified
directory, then it checks the PATH. Linux systems may have an existing
executable utility `/usr/sbin/ss` in the path. If `exe = "ss3"` and this
file is found by
[`check_exe()`](https://r4ss.github.io/r4ss/reference/check_exe.md), it
will be ignored based on the smaller file size relative to the SS3
executable. Linux users who want to use the workflow of having SS3 in
their PATH should name the SS3 file something besides `ss`, such as
`ss3` or `ss_linux`.

## Usage

``` r
run(
  dir = getwd(),
  exe = "ss3",
  extras = "",
  skipfinished = TRUE,
  show_in_console = FALSE,
  console_output_file = "console.output.txt",
  verbose = TRUE
)
```

## Arguments

- dir:

  Directory containing the model input files.

- exe:

  Executable name. Can be just the name of the executable file if it is
  in the specified directory or in the user's PATH. Can also include the
  absolute path or a path relative to the specified directory. Needs to
  be a single character string, not a vector. On Windows, `exe` can
  optionally have the `.exe` extension appended; on Unix-based systems
  (i.e., Mac and Linux), no extension should be included.

- extras:

  Additional ADMB command line arguments passed to the executable, such
  as "-nohess"

- skipfinished:

  Skip any folders that already contain a Report.sso file. This can be
  helpful if the function is interrupted while running iteratively.

- show_in_console:

  Show output in the R console? If FALSE, then the console output is
  saved to a file (specified by `console_output_file`) at the end of the
  model run.

- console_output_file:

  File to store console output (if show_in_console = FALSE).

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

Returns one of five messages: "ran model", "model run failed", "unknown
run status", "not a directory", or "contained Report.sso".

## Details

Checks for presence of a Stock Synthesis executable and then runs the
model with any additional arguments specified by `extras`.

## See also

Other run functions:
[`copy_SS_inputs()`](https://r4ss.github.io/r4ss/reference/copy_SS_inputs.md),
[`jitter()`](https://r4ss.github.io/r4ss/reference/jitter.md),
[`populate_multiple_folders()`](https://r4ss.github.io/r4ss/reference/populate_multiple_folders.md),
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md),
[`retro()`](https://r4ss.github.io/r4ss/reference/retro.md),
[`tune_comps()`](https://r4ss.github.io/r4ss/reference/tune_comps.md)

## Author

Ian G. Taylor, Kathryn L. Doering, Kelli F. Johnson

## Examples

``` r
if (FALSE) { # \dontrun{
dir <- system.file("extdata", "simple_small", package = "r4ss")
r4ss::run(dir = dir)
} # }
```
