# Read ADMB .par and .cor files.

This function will parse the .par and .cor files to provide things like
parameter estimates, standard deviations, and correlations. Required for
Jim Thorson's Laplace Approximation but likely useful for other
purposes.

## Usage

``` r
read.admbFit(file)
```

## Arguments

- file:

  Name of ADMB executable such that files to read will have format
  file.par and file.cor.

## Value

List of various things from these files.

## See also

[`getADMBHessian()`](https://r4ss.github.io/r4ss/reference/getADMBHessian.md),
[`NegLogInt_Fn()`](https://r4ss.github.io/r4ss/reference/NegLogInt_Fn.md)

## Author

James Thorson
