# Calculate variance adjustments for discard or mean body weight data

Function developed for U.S. west coast Sablefish assessment in 2019 to
tune discard data or mean body weight data which are common inputs for
U.S. west coast groundfish assessments but as of 2023 have not often had
any data weighting method applied to them.

## Usage

``` r
calc_var_adjust(data, type = c("CV", "sd"))
```

## Arguments

- data:

  Either the "discard" or "mnwgt" elements of the list returned by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).
  Other data types might work here but haven't been tested.

- type:

  Either "CV" or "sd" specifying the type of control file variance
  adjustment, where the SS3 options are
  ``` 2=add_to_discard_stddev`` and  ```3=add_to_bodywt_CV`, so if `data`is discard data, type should be "CV" and if`data\`
  is mean body weight, type should be "sd".

## Value

A table of input and estimated uncertainty values in units of both CV
and sd including the following:

- `fleet` is the fleet number

- `mean_out` is the mean of the expected values

- `mean_in` is the mean of the observed values

- `CV_in` is the mean input CV

- `sd_in` is the mean input SD values (which may include variance
  adjustments already)

- `sd_out` is the SD of the observed relative to the expected values,
  calculated as described above

- `CV_out` is the CV of the observed relative to the expected,
  calculated as described above

- `added` is the value that could be added to any existing value in the
  "Input variance adjustments factors" section of the control file.

- `type` is the data type code used in "Input variance adjustments
  factors"

## Details

The calculation is based on sd_out = sqrt(mean(Obs - Exp)^2)). Added sd
is calculated as sd_out - sd_in where sd_in is the mean of the input
standard deviations (possibly including existing variance adjustments).
When a CV adjustment is required, the sd_out is converted to CV_out by
dividing by the mean of the expected values and with the added CV
calculated as CV_out - CV_in.

## Author

Kelli F. Johnson
