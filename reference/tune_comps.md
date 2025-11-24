# Calculate new tunings for length and age compositions and (re)run models

Creates a table of values that can be copied into the SS3 control file
for SS3 3.30 models to adjust the input sample sizes for length and age
compositions based on either the Francis or McAllister-Ianelli tuning or
adds the Dirichlet-Multinomial parameters to the necessary files to tune
the model using an integrated method. Optionally, this function can
automatically add these tunings to the appropriate files and rerun the
model for the desired number of iterations.

## Usage

``` r
tune_comps(
  replist = NULL,
  fleets = "all",
  option = c("Francis", "MI", "none", "DM"),
  digits = 6,
  write = TRUE,
  niters_tuning = 0,
  init_run = FALSE,
  dir = getwd(),
  exe = "ss3",
  model = lifecycle::deprecated(),
  extras = "",
  allow_up_tuning = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- fleets:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing fleets or surveys to be included in the plot.

- option:

  Which type of tuning: 'none', 'Francis', 'MI', or 'DM'. The first
  option, `none`, will only return information about the Francis and MI
  weights that are suggested.

- digits:

  Number of digits to round numbers to.

- write:

  Write suggested tunings to a file saved to the disk called
  `suggested_tunings.ss`. This file name is currently hard coded and
  will be saved in `dir`.

- niters_tuning:

  The number of times to retune models. Defaults to 0, where only the
  tunings should be calculated and the model is not rerun. Note that for
  DM, it will be assumed that 0 means not to run the model and
  specifying 1 or greater will only run the model once (because DM is
  not an iterative retuning method).

- init_run:

  Should the model be run before calculating the tunings? Defaults to
  `FALSE`. This run is not counted as an iteration for `niters_tuning`
  and will not be used if `option = "DM"`.

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- exe:

  Executable name. Can be just the name of the executable file if it is
  in the specified directory or in the user's PATH. Can also include the
  absolute path or a path relative to the specified directory. Needs to
  be a single character string, not a vector. On Windows, `exe` can
  optionally have the `.exe` extension appended; on Unix-based systems
  (i.e., Mac and Linux), no extension should be included.

- model:

  Deprecated. Use `exe` instead.

- extras:

  Additional ADMB command line arguments passed to the executable, such
  as "-nohess"

- allow_up_tuning:

  Allow tuning values for Francis or MI \> 1? Defaults to FALSE, which
  caps tuning values at 1.

- verbose:

  A logical value specifying if output should be printed to the screen.

- ...:

  Additional arguments passed to
  [`run()`](https://r4ss.github.io/r4ss/reference/run.md), such as
  `show_in_console`.

## Value

Returns a table that can be copied into the control file. If
`write=TRUE` then will write the values to a file (currently hardwired
to go in the directory where the model was run and called
"suggested_tunings.ss").

## `option`

### Francis

The Francis approach to data weighting adjusts the input sample sizes
using a scalar such that the fit of the expected value is within the
uncertainty intervals based on the expected fit given adjusted sample
sizes.

### McAllister-Ianelli (MI)

Also known as the Harmonic-Mean approach to data weighting, the
McAllister-Ianelli weighting approach uses a scalar to adjust the input
sample size of composition data based matching the arithmetic mean of
the input sample size to the harmonic mean of the effective sample size.

### Dirichlet-Multinomial (DM)

The Dirichlet-Multinomial likelihood is an alternative approach that
allows the tuning data type to be estimated rather than iteratively
tuned. Note that for `option = "DM"` a table of tunings is not created
as the DM is not an iterative reweighting option. Instead, each of the
fleets with length- and age-composition data will be assigned a DM
parameter and the model will be rerun.

## SS3 versions

### 3.30.00-3.30.11

Recommended_var_adj and other columns were named differently in these
early version of SS3. Calculations are thus done internally based on
finding the correct column name.

### 3.30.12-3.30.16

Starting with SS3 version 3.30.12, the "Length_Comp_Fit_Summary" table
in Report.sso is already in the format required to paste into the
control file to apply the McAllister-Ianelli tuning. However, this
function provides the additional option of the Francis tuning and the
ability to compare the two approaches, as well as the functionality to
add tunings and rerun the model. The "Age_Comp_Fit_Summary" table in
Report.sso is formatted similarly though, though the Recommended_var_adj
was wrongly set to 1 for all fleets in SS3 versions 3.30.12 to 3.30.16.
Thus, the MI approach is not taken from this recommended column,
instead, it is calculated from the harmonic mean and input sample sizes.

### 3.30.20

Starting with SS3 version 3.30.20, the Dirichlet-multinomial likelihood
was made available for Generalized Size Comp data. As part of this
change, the column names were changed for all fit summary tables, to
both align the notation among them and also facilitate the future
addition of the Multivariate-Tweedie likelihood.

## References

Francis, R.I.C.C. (2011). Data weighting in statistical fisheries stock
assessment models. Can. J. Fish. Aquat. Sci. 68: 1124-1138.

## See also

Other tuning functions:
[`SSMethod.Cond.TA1.8()`](https://r4ss.github.io/r4ss/reference/SSMethod.Cond.TA1.8.md),
[`SSMethod.TA1.8()`](https://r4ss.github.io/r4ss/reference/SSMethod.TA1.8.md)

Other run functions:
[`copy_SS_inputs()`](https://r4ss.github.io/r4ss/reference/copy_SS_inputs.md),
[`jitter()`](https://r4ss.github.io/r4ss/reference/jitter.md),
[`populate_multiple_folders()`](https://r4ss.github.io/r4ss/reference/populate_multiple_folders.md),
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md),
[`retro()`](https://r4ss.github.io/r4ss/reference/retro.md),
[`run()`](https://r4ss.github.io/r4ss/reference/run.md)

## Author

Ian G. Taylor, Kathryn L. Doering

## Examples

``` r
if (FALSE) { # \dontrun{
# Set up the folders ----
# Create a temporary directory, feel free to change this location
mod_path <- file.path(tempdir(), "simple_mod")
# Path to simple model in r4ss and copy files to mod_path
example_path <- system.file("extdata", "simple_small", package = "r4ss")
# copy model input files
copy_SS_inputs(dir.old = example_path, dir.new = mod_path, verbose = FALSE)
# copy over the Report file
file.copy(
  from = file.path(example_path, "Report.sso"),
  to = file.path(mod_path, "Report.sso")
)
# copy comp report file
file.copy(
  from = file.path(example_path, "CompReport.sso"),
  to = file.path(mod_path, "CompReport.sso")
)
# Use the tune_comps function----

# Examples where a model is not run ----

# Just get the Francis and MI tables, without running the model. Note that the
# model in mod_path needs to already have been run with Stock Synthesis, so
# that a report file is available.

weight_table <- tune_comps(
  dir = mod_path,
  option = "none",
  verbose = FALSE
)
# view the weights. Note that the columns New_Francis and New_MI show the
# weights, but neither were added to the New_Var_adj column
weight_table

# Get the Francis and MI tables, but with the Francis weights in the
# New_Var_adj column. Note if option = "MI" were used, the output would be
# the same except that the New_Var_adj column would contain the MI weights.
weight_table_fran <- tune_comps(
  dir = mod_path,
  option = "Francis",
  verbose = FALSE
)
weight_table_fran

# Add Dirichlet-multinomial tuning parameters to the model,
# without running it.

DM_parm_info <- tune_comps(
  option = "DM",
  niters_tuning = 0, # 0 means the model will not be run.
  dir = mod_path,
  verbose = FALSE
)
# See the Dirichlet parameters added to the model.
DM_parm_info[["tuning_table_list"]]
# can also look in the data file to see which fleets of comp data now have
# DM parameters. The "ParmSelect" column of the len_info and age_info
# contains the dirichlet multinomial parameter numbers.
dat <- SS_readdat(file.path(mod_path, "simple_data.ss"), verbose = FALSE)
dat[["len_info"]]
dat[["age_info"]]

# Examples where models are run ----

# Run MI weighting and allow upweighting for 1 iteration. Assume that an ss
# executable called "ss or ss.exe" is available in the mod_path folder.
# If the executable is not available, then the call will exit on error.
# Note that the Dirichlet mulitnomial parameters will be removed, but any
# previous tunings will be retained.
tune_info <- tune_comps(
  option = "MI",
  niters_tuning = 1,
  dir = mod_path,
  allow_up_tuning = TRUE,
  exe = "ss3",
  verbose = FALSE
)
# see the tuning table, and the weights applied to the model.
tune_info

# Add Dirichlet multinomial paramters and rerun. The function will
# automatically remove the MI weighting and add in the DM parameters.
# Use extras = "-nohess" when running model to speed up run.
DM_parm_info <- tune_comps(
  option = "DM",
  niters_tuning = 1, # must be 1 or greater to run
  dir = mod_path,
  extras = "-nohess",
  verbose = FALSE
)
# see the DM parameter estimates
DM_parm_info[["tuning_table_list"]]

# cleanup ----
unlink(mod_path, recursive = TRUE)
} # }
```
