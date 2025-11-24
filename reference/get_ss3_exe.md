# Download the Stock Synthesis (SS3) executable

The `get_ss3_exe()` function uses the gh package to get either the
latest release (if version = NULL) or the specified version of the Stock
Synthesis executable for the appropriate operating system to the
directory `dir` (if dir = NULL, then the executable is downloaded to the
working directory). To view the version tags available go to
https://github.com/nmfs-ost/ss3-source-code/tags

## Usage

``` r
get_ss3_exe(dir = NULL, version = NULL)
```

## Arguments

- dir:

  The directory that you would like the executable downloaded to.

- version:

  A character string of the executable version tag to download
  (e.g.'v3.30.20' or 'v3.30.18'). A list of tags is available at
  https://github.com/nmfs-ost/ss3-source-code/tags

## Value

A string of the file path to the downloaded executable

## Details

Downloads the SS3 executable according to specified version and the user
operating system.

## Author

Elizabeth F. Gugliotti

## Examples

``` r
if (FALSE) { # \dontrun{
get_ss3_exe()
get_ss3_exe(version = "v3.30.18")
} # }
```
