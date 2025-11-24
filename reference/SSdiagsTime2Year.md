# Convert Time-Steps

Function to convert non-annual into annual time-steps for retros and
cpue residuals

## Usage

``` r
SSdiagsTime2Year(ss3out, time.steps = 0.25, end.time)
```

## Arguments

- ss3out:

  outputs from
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md) or
  [`SSsummarize()`](https://r4ss.github.io/r4ss/reference/SSsummarize.md)

- time.steps:

  time steps behind yrs e.g. 0.25 for quarterly

- end.time:

  last time step e.g. 2018.75 with a cpue observation

## Value

Reformatted Rep file outputs
