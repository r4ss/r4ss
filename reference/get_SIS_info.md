# Gather information for the NOAA Species Information System (SIS)

Processes model results contained in the list created by
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md) in a
format that is more convenient for submission to SIS. Currently the
results are returned invisibly as a list of two tables and written to a
CSV file from which results could be copied into SIS. In the future some
more direct link could be explored to avoid the manual copy step.

## Usage

``` r
get_SIS_info(
  model,
  dir = model[["inputs"]][["dir"]],
  writecsv = TRUE,
  stock = "StockName",
  assessment_type = "Operational",
  final_year = model[["endyr"]] + 1,
  data_year = model[["endyr"]],
  month,
  sciencecenter = "NWFSC",
  Mgt_Council = "PFMC",
  SpawnOutputLabel = model[["SpawnOutputLabel"]],
  contact = "first.last@noaa.gov",
  review_result = "XXXX",
  catch_input_data = "XXXX",
  abundance_input_data = "XXXX",
  bio_input_data = "XXXX",
  comp_input_data = "XXXX",
  ecosystem_linkage = "XXXX"
)
```

## Arguments

- model:

  Output from SS_output

- dir:

  Directory in which to write the CSV files.

- writecsv:

  Write results to a CSV file (where the name will have the format
  "\[stock\]\_2019_SIS_info.csv" where `stock` is an additional input

- stock:

  String to prepend id info to filename for CSV file

- assessment_type:

  "Operational" or "Stock Monitoring Updates" (or perhaps additional
  options as well)

- final_year:

  Year of assessment and reference points (typically will be
  `model[["endyr"]] + 1`)

- data_year:

  Last year of of timeseries data

- month:

  Month when assessment was conducted (within `final_year`)

- sciencecenter:

  Origin of assessment report

- Mgt_Council:

  Council jurisdiction. Currently only `"PFMC"` (Pacific Fishery
  Management Council) and `"GM"` (Gulf of Mexico) are the only options.

- SpawnOutputLabel:

  Units for spawning output if not in mt (e.g. "Millions of eggs"). In
  the future this can be included in the `model` list created by
  [`r4ss::SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

- contact:

  Name and/or email address for lead author.

- review_result:

  Something like "Full Acceptance"

- catch_input_data:

  Qualitative category for catch input data

- abundance_input_data:

  Qualitative category for abundance input data

- bio_input_data:

  Qualitative category for biological input data

- comp_input_data:

  Qualitative category for size/age composition input data

- ecosystem_linkage:

  Qualitative category for ecosystem linkage

## See also

[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian G. Taylor, Andi Stephens, LaTreese S. Denson

## Examples

``` r
if (FALSE) { # \dontrun{
# read the model output
model <- SS_output(
  dir = system.file("extdata/simple_small", package = "r4ss"),
  printstats = FALSE, verbose = FALSE
)
# run get_SIS_info:
info <- get_SIS_info(model, stock = "SimpleExample", month = 1)
} # }
```
