# Calculate Mohn's rho values for select quantities

Function calculates:

1.  a rho value for the ending year for each retrospective relative to
    the reference model as in Mohn (1999);

2.  a “Wood's Hole Mohn's rho”, which is a rho value averaged across all
    years for each retrospective relative to the reference model; and

3.  an Alaska Fisheries Science Center and Hurtado-Ferro et al. (2015)
    Mohn's rho, which is the average rho per retrospective “peel”.

## Usage

``` r
SSmohnsrho(summaryoutput, endyrvec, startyr, verbose = TRUE)
```

## Arguments

- summaryoutput:

  List created by
  [`SSsummarize()`](https://r4ss.github.io/r4ss/reference/SSsummarize.md).
  The expected order for the models are the full reference model, the
  retro -1, retro -2, and so forth. Order matters for the calculations.

- endyrvec:

  Integer vector of years that should be used as the final year for each
  model in `summaryoutput`. The default, which happens if `endyrvec` is
  missing, is based on information in `summaryoutput`, i.e.,
  `summaryoutput[["endyrs"]][summaryoutput[["n"]]]: (summaryoutput[["endyrs"]][summaryoutput[["n"]]] - summaryoutput[["n"]] + 1)`.
  This parameter will be used to extract estimates of fishing mortality
  for each year in `endyrvec` and estimates of biomass-based quantities
  for each year in `endyrvec + 1` because Stock Synthesis reports
  beginning of the year biomass, which we use here as a proxy for end of
  the year biomass.

- startyr:

  Single year used to calculate the start year for the calculation of
  the Wood's Hole Mohn's rho value, which is computed across the range
  of years in the model. If this parameter is missing, the default is to
  use the `startyr` of the reference model.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

A list with the following 12 entries:

- `"SSB"`

- `"Rec"`

- `"Bratio"`

- `"F"`

- `"WoodHole_SSB.all"`

- `"WoodHole_Rec.all"`

- `"WoodHole_Bratio.all"`

- `"WoodHole_F.all"`

- `"AFSC_Hurtado_SSB"`

- `"AFSC_Hurtado_Rec"`

- `"AFSC_Hurtado_F"`

- `"AFSC_Hurtado_Bratio"`

## References

- Hurtado-Ferro et al. 2015. Looking in the rear-view mirror: bias and
  retrospective patterns in integrated, age-structured stock assessment
  models. ICES J. Mar. Sci. 72(1), 99–110.
  https://doi.org/10.1093/icesjms/fsu198.

- Mohn, R. 1999. The retrospective problem in sequential population
  analysis: an investigation using cod fishery and simulated data.
  ICES J. Mar. Sci. 56, 473–488. https://doi.org/10.1006/jmsc.1999.0481.

## Author

Chantel R. Wetzel, Carey R. McGilliard, and Kelli F. Johnson
