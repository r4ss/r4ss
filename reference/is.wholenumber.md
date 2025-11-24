# Utility function to test if x is "numerically" integer wrt machine epsilon taken from example section of help of is.integer

Utility function to test if x is "numerically" integer wrt machine
epsilon taken from example section of help of is.integer

## Usage

``` r
is.wholenumber(x, tol = .Machine[["double.eps"]]^0.5)
```

## Arguments

- x:

  value to check if it is "integer"

- tol:

  tolerace
