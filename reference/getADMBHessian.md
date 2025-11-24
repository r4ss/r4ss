# Read admodel.hes file

This function reads in all of the information contained in the .hes
file. Some is needed for relaxing the covariance matrix, while the rest
is recorded and rewritten to file as ADMB expects.

## Usage

``` r
getADMBHessian(
  hesfile = "admodel.hes",
  File = lifecycle::deprecated(),
  FileName = lifecycle::deprecated()
)
```

## Arguments

- hesfile:

  Name of .hes file, including the full path (can be relative to working
  directory).

- File:

  Deprecated. Add path to `hesfile` input instead.

- FileName:

  Deprecated. Use \`hesfile“ instead.

## Value

A list with elements num.pars, hes, hybrid_bounded_flag, and scale.

## Note

Explanation of the methods (in PDF form):
<https://github.com/admb-project/admb-examples/blob/master/admb-tricks/covariance-calculations/ADMB_Covariance_Calculations.pdf>

## See also

[`read.admbFit()`](https://r4ss.github.io/r4ss/reference/read.admbFit.md),
[`NegLogInt_Fn()`](https://r4ss.github.io/r4ss/reference/NegLogInt_Fn.md)

## Author

Cole Monnahan
