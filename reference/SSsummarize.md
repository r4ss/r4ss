# Summarize the output from multiple Stock Synthesis models.

Summarize various quantities from the model output collected by
[`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md)
and return them in a list of tables and vectors. If the models have
incompatible dimensions, some quantities can't be compared via this
function, such as selectivity.

## Usage

``` r
SSsummarize(
  biglist,
  sizeselfactor = "Lsel",
  ageselfactor = "Asel",
  selfleet = NULL,
  selyr = "startyr",
  selgender = lifecycle::deprecated(),
  selsex = 1,
  SpawnOutputUnits = NULL,
  lowerCI = 0.025,
  upperCI = 0.975,
  verbose = TRUE
)
```

## Arguments

- biglist:

  A list of lists, one for each model. The individual lists can be
  created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md) or
  the list of lists can be created by
  [`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md)
  (which iteratively calls
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)).

- sizeselfactor:

  A string or vector of strings indicating which elements of the
  selectivity at length output to summarize. Default=c("Lsel").

- ageselfactor:

  A string or vector of strings indicating which elements of the
  selectivity at age output to summarize. Default=c("Asel").

- selfleet:

  Vector of fleets for which selectivity will be summarized. NULL=all
  fleets. Default=NULL.

- selyr:

  String or vector of years for which selectivity will be summarized.
  NOTE: NOT CURRENTLY WORKING. Options: NULL=all years, "startyr" =
  first year.

- selgender:

  Deprecated. Use selsex instead.

- selsex:

  Vector of sexes (1 and/or 2) for which selectivity will be summarized.
  NULL=all sexes. Default=NULL.

- SpawnOutputUnits:

  Optional single value or vector of "biomass" or "numbers" giving units
  of spawning for each model.

- lowerCI:

  Quantile for lower bound on calculated intervals. Default = 0.025 for
  95% intervals.

- upperCI:

  Quantile for upper bound on calculated intervals. Default = 0.975 for
  95% intervals.

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

Other model comparison functions:
[`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md),
[`SSplotComparisons()`](https://r4ss.github.io/r4ss/reference/SSplotComparisons.md),
[`SStableComparisons()`](https://r4ss.github.io/r4ss/reference/SStableComparisons.md)

## Author

Ian Taylor
