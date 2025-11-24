# Execute a single jittered model run

Execute a single jittered model run

## Usage

``` r
iterate_jitter(
  i,
  printlikes = TRUE,
  exe = "ss3",
  verbose = FALSE,
  init_values_src = 0,
  dir = NULL,
  extras = NULL,
  ...
)
```

## Arguments

- i:

  Index of the jitter iteration.

- printlikes:

  A logical value specifying if the likelihood values should be printed
  to the console.

- exe:

  Executable name. Can be just the name of the executable file if it is
  in the specified directory or in the user's PATH. Can also include the
  absolute path or a path relative to the specified directory. Needs to
  be a single character string, not a vector. On Windows, `exe` can
  optionally have the `.exe` extension appended; on Unix-based systems
  (i.e., Mac and Linux), no extension should be included.

- verbose:

  A logical value specifying if output should be printed to the screen.

- init_values_src:

  Either zero or one, specifying if the initial values to jitter should
  be read from the control file or from the par file, respectively.
  Cannot be `NULL`. Defaults to zero (initial values read from control
  file).

- dir:

  Directory where model files are located.

- extras:

  Additional ADMB command line arguments passed to the executable, such
  as "-nohess"

- ...:

  Additional arguments passed to
  [`run()`](https://r4ss.github.io/r4ss/reference/run.md)

## Value

Negative log-likelihood of one jittered model

## Author

James T. Thorson, Kelli F. Johnson, Ian G. Taylor, Kathryn L. Doering,
Kiva L. Oken
