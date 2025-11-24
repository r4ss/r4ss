# Find location of executable within path or specified directory

The `check_exe()` function first checks the specified directory `dir`
for the specified SS3 executable name and returns the file's location if
found. If it is not found in the specified directory, then it checks the
PATH. Linux systems may have an existing executable utility
`/usr/sbin/ss` in the path. If `exe = "ss3"` and this file is found by
``` check_exe()``, it will be ignored based on the smaller file size relative to the SS3 executable. Linux users who want to use the workflow of having SS3 in their PATH should name the SS3 file something besides  ```ss`, such as `ss3`or`ss_linux\`.

## Usage

``` r
check_exe(exe = "ss3", dir = getwd(), verbose = FALSE)
```

## Arguments

- exe:

  Executable name. Can be just the name of the executable file if it is
  in the specified directory or in the user's PATH. Can also include the
  absolute path or a path relative to the specified directory. Needs to
  be a single character string, not a vector. On Windows, `exe` can
  optionally have the `.exe` extension appended; on Unix-based systems
  (i.e., Mac and Linux), no extension should be included.

- dir:

  The directory where `exe` is located (if not in path). Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html) but can be an absolute
  path, a path relative to the working directory or a path relative to a
  directory that's in the PATH. Can also be a vector of directories.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

A list containing `$exe` and `$path`. `$exe` is the cleaned version of
the `exe` file name input. Windows systems will include ".exe" in the
returned value. On Linux and Mac systems, the returned `$exe` will
include "./" if the executable was found in the specified directory
`dir`. This will be a single character string, unlike `$path` which will
be a vector if the input `dir` is a vector. The `$path` element of the
list includes the normalized path (or paths if `dir` is a vector) where
the executable was found. If `dir` is a vector and the executable is
missing from a subset of those directories, NA is returned for those
elements of `$path`. If the specified `exe` input is not found in any of
the `dir` input values nor in the PATH, then the function stops with an
error.

## Details

Check that the Stock Synthesis executable name provided in `exe`, an
input argument to numerous `r4ss` functions is available in the location
specified by `dir` or in the path.

## See also

[`run()`](https://r4ss.github.io/r4ss/reference/run.md)

## Author

Kelli F. Johnson, Ian G. Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
# check for executable called "ss3" or "ss3.exe" in the PATH
check_exe()
# check for executable with a different name in the PATH
check_exe(exe = "ss_win")
# check for executable in a specific directory
check_exe(exe = "ss_linux", dir = "~/ss/ss_v3.30.19.01")
} # }
```
