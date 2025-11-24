# Apply Francis composition weighting method TA1.8

Uses method TA1.8 (described in Appendix A of Francis 2011) to do
stage-2 weighting of composition data from a Stock Synthesis model.
Outputs a multiplier, *w* (with bootstrap 95% confidence interval), so
that *N2y* = *w* x *N1y*, where *N1y* and *N2y* are the stage-1 and
stage-2 multinomial sample sizes for the data set in year y. Optionally
makes a plot of observed (with confidence limits, based on *N1y*) and
expected mean lengths (or ages).  
  
CAUTIONARY/EXPLANATORY NOTE. The large number of options available in SS
makes it very difficult to be sure that what this function does is
appropriate for all combinations of options. The following notes might
help anyone wanting to check or correct the code.

1.  The code first takes the appropriate database (lendbase, sizedbase,
    agedbase, or condbase) and removes unneeded rows.

2.  The remaining rows of the database are grouped into individual comps
    (indexed by vector indx) and relevant statistics (e.g., observed and
    expected mean length or age), and ancillary data, are calculated for
    each comp (these are stored in pldat - one row per comp). If the
    data are to be plotted, the comps are grouped, with each group
    corresponding to a panel in the plot, and groups are indexed by
    plindx.

3.  A single multiplier is calculated to apply to all the comps.

## Usage

``` r
SSMethod.TA1.8(
  fit,
  type,
  fleet,
  part = 0:2,
  sexes = 0:3,
  seas = NULL,
  method = NULL,
  plotit = TRUE,
  printit = FALSE,
  datonly = FALSE,
  plotadj = !datonly,
  maxpanel = 1000,
  fleetnames = NULL,
  label.part = TRUE,
  label.sex = TRUE,
  set.pars = TRUE,
  add = FALSE
)
```

## Arguments

- fit:

  Stock Synthesis output as read by r4SS function SS_output

- type:

  either 'len' (for length composition data), 'size' (for generalized
  size composition data), 'age' (for age composition data), or 'con'
  (for conditional age at length data)

- fleet:

  vector of one or more fleet numbers whose data are to be analysed
  simultaneously (the output N multiplier applies to all fleets
  combined)

- part:

  vector of one or more partition values; analysis is restricted to
  composition data with one of these partition values. Default is to
  include all partition values (0, 1, 2).

- sexes:

  vector of one or more values for Sexes; analysis is restricted to
  composition data with one of these Sexes values. Ignored if
  type=='con'.

- seas:

  string indicating how to treat data from multiple seasons 'comb' -
  combine seasonal data for each year and plot against Yr 'sep' - treat
  seasons separately, plotting against Yr.S If is.null(seas) it is
  assumed that there is only one season in the selected data (a warning
  is output if this is not true) and option 'comb' is used.

- method:

  a vector of one or more size-frequency method numbers (ignored unless
  type = 'size'). If !is.null(method), analysis is restricted to
  size-frequency methods in this vector. NB comps are separated by
  method

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

- fleetnames:

  Optional replacement for fleetnames used in data file.

- label.part:

  Include labels indicating which partitions are included?

- label.sex:

  Include labels indicating which sexes are included?

- set.pars:

  Set the graphical parameters such as mar and mfrow. Can be set to
  FALSE in order to add plots form multiple calls to this function as
  separate panels in one larger figure.

- add:

  add to existing plot

## References

Francis, R.I.C.C. (2011). Data weighting in statistical fisheries stock
assessment models. Canadian Journal of Fisheries and Aquatic Sciences
68: 1124-1138.

## See also

Other tuning functions:
[`SSMethod.Cond.TA1.8()`](https://r4ss.github.io/r4ss/reference/SSMethod.Cond.TA1.8.md),
[`tune_comps()`](https://r4ss.github.io/r4ss/reference/tune_comps.md)

## Author

R.I.C Chris Francis, Andre E. Punt, Ian G. Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
Nfleet <- length(myreplist[["FleetNames"]])
for (Ifleet in 1:Nfleet) {
  SSMethod.TA1.8(myreplist, "len", fleet = Ifleet, maxpanel = maxpanel)
}
for (Ifleet in 1:Nfleet) {
  SSMethod.TA1.8(myreplist, "age", fleet = Ifleet, maxpanel = maxpanel)
}
for (Ifleet in 1:Nfleet) {
  SSMethod.TA1.8(myreplist, "size", fleet = Ifleet, maxpanel = maxpanel)
}
for (Ifleet in 1:Nfleet) {
  SSMethod.TA1.8(myreplist, "con", fleet = Ifleet, maxpanel = maxpanel)
}
for (Ifleet in 1:Nfleet) {
  SSMethod.Cond.TA1.8(myreplist, fleet = Ifleet, maxpanel = maxpanel)
}
} # }
```
