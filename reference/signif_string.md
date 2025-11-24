# Convert a numeric vector to character with chosen significant digits

Convert a numeric vector to character with chosen significant digits

## Usage

``` r
signif_string(x, digits = 3)
```

## Arguments

- x:

  Vector of numeric values

- digits:

  Number of significant digits

## Details

The [`signif()`](https://rdrr.io/r/base/Round.html) function applies the
same number of decimal to all values in a vector, but parameter tables
often include very different scales, like Wtlen_1_Fem_GP_1 = 2e-06 and
Wtlen_2_Fem_GP_1 = 3, so this applies
[`signif()`](https://rdrr.io/r/base/Round.html) separately to each value
and then converts to a string.

## Author

Ian G. Taylor
