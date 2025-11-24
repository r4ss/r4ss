# Apply Francis composition weighting method TA1.8 for conditional age-at-length fits

Uses an extension of method TA1.8 (described in Appendix A of Francis,
2011) to do stage-2 weighting of conditional age at length composition
data from a Stock Synthesis model.

## Usage

``` r
SSMethod.Cond.TA1.8(
  fit,
  fleet,
  part = 0:2,
  seas = NULL,
  plotit = TRUE,
  printit = FALSE,
  datonly = FALSE,
  plotadj = !datonly,
  maxpanel = 1000,
  FullDiagOut = FALSE,
  ShowVersionB = FALSE,
  fleetnames = NULL,
  add = FALSE
)
```

## Arguments

- fit:

  Stock Synthesis output as read by r4SS function SS_output

- fleet:

  vector of one or more fleet numbers whose data are to be analysed
  simultaneously (the output N multiplier applies to all fleets
  combined)

- part:

  vector of one or more partition values; analysis is restricted to
  composition data with one of these partition values. Default is to
  include all partition values (0, 1, 2).

- seas:

  string indicating how to treat data from multiple seasons 'comb' -
  combine seasonal data for each year and plot against Yr 'sep' - treat
  seasons separately, plotting against Yr.S If is.null(seas) it is
  assumed that there is only one season in the selected data (a warning
  is output if this is not true) and option 'comb' is used.

- plotit:

  if TRUE, make an illustrative plot like one or more panels of Fig. 4
  in Francis (2011).

- printit:

  if TRUE, print results to R console.

- datonly:

  if TRUE, don't show the model expectations

- plotadj:

  if TRUE, plot the confidence intervals associated with the adjusted
  sample sizes (TRUE by default unless datonly = TRUE)

- maxpanel:

  maximum number of panels within a plot

- FullDiagOut:

  Print full diagnostics?

- ShowVersionB:

  Report the Version B value in addition to the default?

- fleetnames:

  Optional replacement for fleetnames used in data file.

- add:

  add to existing plot

## Details

The function outputs a multiplier, *w*, (with bootstrap 95% confidence
intervals) so that *N2i* = *w* x *N1i*, where *N1i* and *N2i* are the
stage-1 and stage-2 multinomial sample sizes for the *i*th composition.
Optionally makes a plot of observed and expected mean ages, with two
alternative sets of confidence limits - based on *N1i* (thin lines) and
*N2i* (thick lines) - for the observed values.

This function formerly reported two versions of w differ according to
whether the calculated mean ages are indexed by year (version A) or by
year and length bin (version B). However, research by Punt (2017) found
Version A to perform better and version B is no longer recommended and
is only reported if requested by the user.

CAUTIONARY/EXPLANATORY NOTE. The large number of options available in SS
makes it very difficult to be sure that what this function does is
appropriate for all combinations of options. The following notes (for
version A) might help anyone wanting to check or correct the code.

1.  The code first removes unneeded rows from database condbase.

2.  The remaining rows of the database are grouped (indexed by vector
    indx) and relevant statistics (e.g., observed and expected mean
    age), and ancillary data, are calculated for each group (these are
    stored in pldat - one row per group).

3.  If the data are to be plotted they are further grouped by fleet,
    with one panel of the plot per fleet.

4.  A single multiplier, *w*, is calculated to apply to all the selected
    data.

## References

Francis, R.I.C.C. (2011). Data weighting in statistical fisheries stock
assessment models. Can. J. Fish. Aquat. Sci. 68: 1124-1138.
https://doi.org/10.1139/f2011-025.

Punt, A.E. (2017). Some insights into data weighting in integrated stock
assessments. Fish. Res. 192:52-65.
https://doi.org/10.1016/j.fishres.2015.12.006.

## See also

Other tuning functions:
[`SSMethod.TA1.8()`](https://r4ss.github.io/r4ss/reference/SSMethod.TA1.8.md),
[`tune_comps()`](https://r4ss.github.io/r4ss/reference/tune_comps.md)

## Author

R.I.C Chris Francis, Andre E. Punt, Ian G. Taylor
