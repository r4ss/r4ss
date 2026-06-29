# Add parameter line

Add an additional parameter line to an existing parameter data frame in
a control list. The function will copy an existing line, change the
values in columns that the user specifies (e.g., PHASE, INIT), and place
the new line below an existing line. The user must also name the new
row. The name does not have to follow any naming conventions, but it
must be unique. Parameters can be added to either style of parameter
data frame (short or long). You can also use this function to add lines
to set-up data frames (e.g., `$ctl[["Q_options"]]`).

## Usage

``` r
SS_add_parameter_line(
  par_df,
  row_to_copy = 1,
  row_before,
  newval_df = data.frame(rowname = "new_parameter")
)
```

## Arguments

- par_df:

  Parameter data frame from the r4ss control list that you would like to
  add an additional parameter line to. The parameter table can by short
  or long.

- row_to_copy:

  Row number or row name to copy from the existing parameter data frame.
  Matching is done by [`grep()`](https://rdrr.io/r/base/grep.html) so
  only a uniquely identifiable string is necessary, not the full row
  name. This can be a string or an integer.

- row_before:

  Row number or row name you would like to place the new parameter line
  after. Matching is done by
  [`grep()`](https://rdrr.io/r/base/grep.html) so only a uniquely
  identifiable string is necessary, not the full row name. This can be a
  string or an integer. If you would like the new parameter to be placed
  in the first row, set this equal to 0.

- newval_df:

  A data frame of values you would like to change from the row that was
  copied. The only required column is "rowname". All other column names
  must match existing column names in `par_df`.

## Value

A parameter data frame with the new parameter line inserted in the
specified location.

## Author

Kiva L. Oken

## Examples

``` r

inputs <- SS_read(
dir = system.file("extdata", "simple_small", package = "r4ss")
)
# Details for new row
newval_df <- data.frame(
  rowname = "P_3_FISHERY",
  INIT = 5,
  PHASE = 4,
  Block = 1,
  Block_Fxn = 1
)

size_selex_new <- SS_add_parameter_line(
  par_df = inputs[["ctl"]][["size_selex_parms"]],
  row_to_copy = 1,
  row_before = "P_2_FISH",
  newval_df = newval_df
)

size_selex_new
#>                           LO HI     INIT PRIOR PR_SD PR_type PHASE env_var&link
#> SizeSel_P_1_FISHERY(1) 19.00 80 56.51380    50    99       0     2            0
#> SizeSel_P_2_FISHERY(1)  0.01 60 20.17930    15    99       0     3            0
#> P_3_FISHERY            19.00 80  5.00000    50    99       0     4            0
#> SizeSel_P_1_SURVEY1(2) 19.00 70 36.02330    30    99       0     2            0
#> SizeSel_P_2_SURVEY1(2)  0.01 60  5.33502    10    99       0     3            0
#>                        dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
#> SizeSel_P_1_FISHERY(1)        0         0         0      0     0         0
#> SizeSel_P_2_FISHERY(1)        0         0         0      0     0         0
#> P_3_FISHERY                   0         0         0      0     1         1
#> SizeSel_P_1_SURVEY1(2)        0         0         0      0     0         0
#> SizeSel_P_2_SURVEY1(2)        0         0         0      0     0         0
```
