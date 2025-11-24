# Make html diagnostic tables

Creates html tables that show diagnostic outputs, including status
checks, gradients, and correlations.

## Usage

``` r
SS_makeHTMLdiagnostictable(
  replist,
  plotdir = NULL,
  gradmax = 0.001,
  ncor = 50,
  cormax = 0.95,
  cormin = 0.01
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- plotdir:

  Directory where PNG files will be written.

- gradmax:

  the largest gradient value for estimated parameter

- ncor:

  number of rows in tables of correlations

- cormax:

  threshold for highlighting high correlations

- cormin:

  threshold for highlighting low correlations

## Value

a three-element vector; the first element is the name of the html table
file, the second is the table caption, and the third is the category of
output type

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md),
[`SS_html()`](https://r4ss.github.io/r4ss/reference/SS_html.md)

## Author

Christine Stawitz
