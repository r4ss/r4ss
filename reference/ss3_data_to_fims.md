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
  EWAA = NULL,
  fleets = NULL,
  maxage = NULL,
  lengths = NULL,
  ageing_error_matrices = NULL
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

  A list containing `dat` such as that created by
  [`r4ss::SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md).
  When `EWAA` is `TRUE`, the list must also contain `wtatage`. Only
  required if `ss3_dir` is not provided.

- ss3_output:

  A list created by
  [`r4ss::SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).
  Only required for age-to-length conversion if `ss3_dir` is not
  provided.

- EWAA:

  Logical indicating whether to use empirical weight-at-age and
  age_to_length conversion tables from the SS3 model or to rely on the
  FIMS growth model. If NULL then it will follow the setting in the SS3
  control file (`ctl[['EmpiricalWAA']]`)

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

- ageing_error_matrices:

  Optional vector of SS3 age-error definition IDs to convert. If NULL,
  all positive IDs referenced by retained age-composition data are
  converted. Definitions omitted from this vector are not applied.

- fleet_column:

  Name of the column used to identify the fleet.

- label:

  Label used in the row-count message.

- bin_pattern:

  Regular expression matching the composition-bin columns.

- data:

  Output rows for a single data type.

- definition_id:

  SS3 age-error definition number.

- age:

  Age bin to look up.

- length:

  Length bin to look up.

## Value

A data frame that can be passed to `FIMS::FIMSFrame()`

The filtered data frame. Normalize composition bins and reshape them
into one row per bin.

Long-format data with fleet, year, sample size, bin, and proportion.
Reject conflicting duplicate keys and remove exact repeated
observations.

Data with at most one row per identifying key. Calculate observed-age
probabilities from one SS3 mean/SD vector pair.

A matrix with true ages in rows and observed ages in columns. Look up
the SS3 age-to-length proportion for one age and length bin.

The aggregated proportion, or `NA` when either bin is out of range.

## See also

[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md) can be
used to write the input files using the list Filter observations to
selected fleets and report any rows removed.

## Author

Ian G. Taylor, Megumi Oshima, Kelli F. Johnson
