# read ss_summary file

read Stock Synthesis ss_summary.sso file into list object in R

## Usage

``` r
SS_read_summary(file = "ss_summary.sso", verbose = FALSE)
```

## Arguments

- file:

  Filename either with full path or relative to working directory.

  See the formal arguments for a possible default filename.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

Output will be a list with four elements, `header`, `likelihoods`,
`parameters`, and `derived_quants`. Each is a data frame with rownames
indicating the quantity shown in each row.

## See also

[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md)

## Author

Ian Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
summary <- SS_read_summary(file = "c:/mymodel/ss_summary.sso")
} # }
```
