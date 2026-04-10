# Convert SS3 data into format required by FIMS

Uses output from
[`r4ss::SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md)
and does filtering, simplifying, and reformatting.

## Usage

``` r
ss3_data_to_fims(
  ss3_dir = NULL,
  ss_new = TRUE,
  ss3_inputs = NULL,
  ss3_output = NULL,
  fleets = NULL,
  maxage = NULL,
  lengths = NULL
)
```

## Arguments

- ss3_dir:

  Directory containing the SS3 input and output model files.
  Alternatively the SS3 input and output lists can be input directly
  using `ss3_inputs` and `ss3_output`

- ss_new:

  Logical indicating whether to read in the .ss_new files instead of the
  original input files. Beneficial for dealing with negative years in
  the wtatage file which can't yet be processed by this function.

- ss3_inputs:

  A list containing `dat` and `wtatage` such as that created by
  [`r4ss::SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md).
  Only required if `ss3_dir` is not provided.

- ss3_output:

  A list created by
  [`r4ss::SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).
  Only required if `ss3_dir` is not provided.

- fleets:

  Which fleets to include in the processed output. Note that the only
  start year population weight-at-age is read from the `wtatage` element
  (fleet = 0). NULL will default to including all fleets from the SS3
  model.

- maxage:

  Max age to include in the processed output. NULL will default to using
  age 0 up to the maximum age data bin in the SS3 model.

- lengths:

  Vector of lengths to include in the processed output. Needs to be a
  subset of the length bins in the SS3 model. NULL will default to using
  all length data bins from the SS3 model.

## Value

A data frame that can be passed to `FIMS::FIMSFrame()`

## See also

[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md) can be
used to write the input files using the list

## Author

Ian G. Taylor, Megumi Oshima, Kelli F. Johnson
