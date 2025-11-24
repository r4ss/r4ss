# tryCatch both warnings (with value) and errors

Catch *and* save both errors and warnings, and in the case of a warning,
also keep the computed result.

## Usage

``` r
tryCatch.W.E(expr)
```

## Arguments

- expr:

  an R expression to evaluate

## Value

a list with 'value' and 'warning', where 'value' may be an error caught.

## Details

Copied from
https://svn.r-project.org/R/trunk/src/library/base/demo/error.catching.R

## Author

Martin Maechler; Copyright (C) 2010-2023 The R Core Team
