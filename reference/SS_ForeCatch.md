# Create table of fixed forecast catches

Processing values of dead or retained biomass from timeseries output to
fit the format required at the bottom of the forecast file. This can be
used to map the catches resulting from forecasting with a particular
harvest control rule into a model representing a different state of
nature. This is a common task for US west coast groundfish but might be
useful elsewhere.

## Usage

``` r
SS_ForeCatch(
  replist,
  yrs = 2025:2036,
  average = FALSE,
  avg.yrs = 2020:2024,
  total = NULL,
  digits = 2,
  dead = TRUE,
  zeros = FALSE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- yrs:

  Range of years in which to fill in forecast catches from timeseries

- average:

  Use average catch over a range of years for forecast (as opposed to
  using forecast based on control rule)

- avg.yrs:

  Range of years to average over

- total:

  Either single value or vector of annual total forecast catch used to
  scale values (especially if values are from average catches). For west
  coast groundfish, total might be ACL for next 2 forecast years

- digits:

  Number of digits to round to in table

- dead:

  TRUE/FALSE switch to choose dead catch instead of retained catch.

- zeros:

  Include entries with zero catch (TRUE/FALSE)

## See also

[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md)

## Author

Ian G. Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
# create table based on average over past 5 years
SS_ForeCatch(base, # object created by SS_output
  yrs = 2021:2022, # years with fixed catch
  average = TRUE, # catch by fleet from average catch
  avg.yrs = 2014:2018
) # use average of catches over past 5 years

# create table with pre-defined totals where the first 2 years
# are based on current harvest specifications and the next 10 are set to some
# new value (with ratio among fleets based on average over past 5 years)
SS_ForeCatch(base, # object created by SS_output
  yrs = 2021:2022, # years with fixed catch
  average = TRUE, # catch by fleet from average catch
  avg.yrs = 2016:2020, # use average of catches over past 5 years
  total = c(rep(241.3, 2), rep(300, 10))
) # total

# create table based on harvest control rule projection in SS
# that can be mapped into an alternative state of nature
SS_ForeCatch(low_state, # object created by SS_output for low state
  yrs = 2021:2032, # forecast period after fixed ACL years
  average = FALSE
) # use values forecast in SS, not historic catch
} # }
```
