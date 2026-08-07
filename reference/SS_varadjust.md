# Modify variance and sample size adjustments in the control file

Function has not been fully tested yet

## Usage

``` r
SS_varadjust(
  dir = getwd(),
  ctlfile = "control.ss_new",
  newctlfile = "control_modified.ss",
  keyword = "variance adjustments",
  newtable = NULL,
  newrow = NULL,
  rownumber = NULL,
  maxcols = 100,
  maxrows = 100,
  overwrite = FALSE,
  version = "3.30",
  verbose = TRUE
)
```

## Arguments

- dir:

  A file path to the directory of interest. When omitted, functions
  typically default to the current working directory or the model
  directory stored in `replist`.

- ctlfile:

  Control file name. Default="control.ss_new".

- newctlfile:

  Name of new control file to be written. Default="control_modified.ss".

- keyword:

  Keyword to use as reference for start of section on variance
  adjustments

- newtable:

  Optional table of new variance adjustment values

- newrow:

  Optional vector of new variance adjustment values for a particular row

- rownumber:

  Which of the 6 rows to replace with 'newrow' if present?

- maxcols:

  Maximum number of columns to search among in 3.24 models (may need to
  increase from default if you have a huge number of fleets)

- maxrows:

  Maximum number of rows to search among in 3.30 models (may need to
  increase from default if you have a huge number of fleets)

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

- version:

  SS version number. Currently "3.24" or "3.30" are supported, either as
  character or numeric values (noting that numeric 3.30 = 3.3).
  `version = NULL` is no longer the default or an allowed entry. The
  default is `version = "3.30"`.

- verbose:

  A logical value specifying if output should be printed to the screen.

## See also

[`tune_comps()`](https://r4ss.github.io/r4ss/reference/tune_comps.md),
[`SS_parlines()`](https://r4ss.github.io/r4ss/reference/SS_parlines.md),
[`SS_changepars()`](https://r4ss.github.io/r4ss/reference/SS_changepars.md)

## Author

Ian G. Taylor, Gwladys I. Lambert

## Examples

``` r
if (FALSE) { # \dontrun{
# load model output into R
replist <- SS_output(dir = system.file("extdata", "simple_small", package = "r4ss"))

# get new variance adjustments (
varadjust <- tune_comps(replist, option = "Francis")
print(varadjust)

# write new table to file
SS_varadjust(
  dir = replist[["inputs"]][["dir"]], newctlfile = "new_control.ss",
  newtable = varadjust, overwrite = FALSE
)
} # }
```
