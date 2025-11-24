# Get the name of the .par file in a directory

In previous versions of Stock Synthesis,

## Usage

``` r
get_par_name(dir, verbose = TRUE)
```

## Arguments

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

A string with the name of the .par file. If not found, will be NA. If
multiple files exist, preference is given to ss3.par (default as of
3.30.22.1), followed by ss.par, followed by the most recently modified
file with a \*.par extension (choosing the first if two were modified at
the same time).

## See also

[get_dat_new_name](https://r4ss.github.io/r4ss/reference/get_dat_new_name.md)
