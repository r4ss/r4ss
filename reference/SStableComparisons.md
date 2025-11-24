# make table comparing quantities across models

Creates a table comparing key quantities from multiple models, which is
a reduction of the full information in various parts of the list created
using the `SSsummarize` function.

## Usage

``` r
SStableComparisons(
  summaryoutput,
  models = "all",
  likenames = c("TOTAL", "Survey", "Length_comp", "Age_comp", "priors", "Size_at_age"),
  names = c("Recr_Virgin", "R0", "steep", "NatM", "L_at_Amax", "VonBert_K", "SSB_Virg",
    "Bratio_2025", "SPRratio_2024"),
  digits = NULL,
  modelnames = summaryoutput[["modelnames"]],
  csv = FALSE,
  csvdir = "workingdirectory",
  csvfile = "parameter_comparison_table.csv",
  verbose = TRUE,
  mcmc = FALSE
)
```

## Arguments

- summaryoutput:

  list created by `SSsummarize`

- models:

  optional subset of the models described in `summaryoutput`. Either
  "all" or a vector of numbers indicating columns in summary tables.

- likenames:

  Labels for likelihood values to include, should match substring of
  labels in `summaryoutput[["likelihoods"]]`.

- names:

  Labels for parameters or derived quantities to include, should match
  substring of labels in `summaryoutput[["pars"]]` or
  `summaryoutput[["quants"]]`.

- digits:

  Optional vector of the number of decimal digits to use in reporting
  each quantity.

- modelnames:

  optional vector of labels to use as column names. Default is
  `summaryoutput[["modelnames"]]`.

- csv:

  write resulting table to CSV file?

- csvdir:

  directory for optional CSV file

- csvfile:

  filename for CSV file

- verbose:

  A logical value specifying if output should be printed to the screen.

- mcmc:

  summarize MCMC output in table?

## See also

Other model comparison functions:
[`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md),
[`SSplotComparisons()`](https://r4ss.github.io/r4ss/reference/SSplotComparisons.md),
[`SSsummarize()`](https://r4ss.github.io/r4ss/reference/SSsummarize.md)

## Author

Ian Taylor
