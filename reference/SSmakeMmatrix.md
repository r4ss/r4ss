# Convert a matrix of natural mortality values into inputs for Stock Synthesis

Inspired by Valerio Bartolino and North Sea herring

## Usage

``` r
SSmakeMmatrix(
  mat,
  startyr,
  outfile = NULL,
  overwrite = FALSE,
  yrs.in.columns = TRUE
)
```

## Arguments

- mat:

  a matrix of natural mortality by year and age, starting with age 0

- startyr:

  the first year of the natural mortality values (no missing years)

- outfile:

  optional file to which the results will be written

- overwrite:

  if 'outfile' is provided and exists, option to overwrite or not

- yrs.in.columns:

  an indicator of whether the matrix has years in columns or rows

## Value

Prints inputs with option to write to chosen file

## Author

Ian Taylor
