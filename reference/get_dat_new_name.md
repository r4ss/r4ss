# Get the name of the data .ss_new file in a directory

In previous versions of Stock Synthesis, the file new data file was
named `data.ss_new`. `_echo` was added to the name when the file was
parsed into three separate files.

## Usage

``` r
get_dat_new_name(dir)
```

## Arguments

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

## Value

A string with the name of the data .ss_new file. If not found, will be
NA. Both of strings are searched for using `dir(pattern = )` and if both
exist, then `data_echo.ss_new` is returned. If the `dir` input points to
github, then [`dir()`](https://rdrr.io/r/base/list.files.html) doesn't
work and `data_echo.ss_new` is always returned.

## See also

[get_par_name](https://r4ss.github.io/r4ss/reference/get_par_name.md)
