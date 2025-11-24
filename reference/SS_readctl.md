# Read control file from SS

Read control file from Stock Synthesis (SS3) into R as a list object.
This function acts as a wrapper for version-specific `SS_readctl_`
functions. But all version-specific functions prior to 3.30 have been
deprecated, so this function primarily calls
[`SS_readctl_3.30()`](https://r4ss.github.io/r4ss/reference/SS_readctl_3.30.md).
Input arguments that do not pertain to the version of your control file
can be left at their default values.

## Usage

``` r
SS_readctl(
  file,
  version = "3.30",
  verbose = FALSE,
  use_datlist = TRUE,
  datlist = file.path(dirname(file), "data_echo.ss_new"),
  nseas = NULL,
  N_areas = NULL,
  Nages = NULL,
  Nsexes = NULL,
  Npopbins = NA,
  Nfleets = NULL,
  Nfleet = NULL,
  Do_AgeKey = NULL,
  Nsurveys = NULL,
  N_tag_groups = NULL,
  N_CPUE_obs = NULL,
  catch_mult_fleets = NULL,
  predM_fleets = NULL,
  Ntag_fleets = NULL,
  N_rows_equil_catch = NULL,
  N_dirichlet_parms = NULL,
  ptype = lifecycle::deprecated()
)
```

## Arguments

- file:

  Filename either with full path or relative to working directory.

  See the formal arguments for a possible default filename.

- version:

  SS version number. Currently "3.24" or "3.30" are supported, either as
  character or numeric values (noting that numeric 3.30 = 3.3).
  `version = NULL` is no longer the default or an allowed entry. The
  default is `version = "3.30"`.

- verbose:

  A logical value specifying if output should be printed to the screen.

- use_datlist:

  LOGICAL. If TRUE, use datlist to derive parameters which can not be
  determined from control file. Defaults to TRUE.

- datlist:

  list or character. If list, should be a list produced from
  [`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md).
  If character, should be the file name of an SS data file.

- nseas:

  number of seasons in the model. This information is not explicitly
  available in control file and used only if `use_datlist = FALSE`.

- N_areas:

  number of spatial areas in the model. Default = 1. This information is
  not explicitly available in control file and used only if if
  `use_datlist = FALSE`.

- Nages:

  oldest age in the model. This information is also not explicitly
  available in control file and used only if `use_datlist = FALSE`.

- Nsexes:

  number of sexes in the model. This information is also not explicitly
  available in control file and used only if `use_datlist = FALSE`.

- Npopbins:

  number of population bins in the model. This information is also not
  explicitly available in control file and this information is only
  required if length based maturity vector is directly supplied
  (Maturity option of 6). and used only if `use_datlist = FALSE`.

- Nfleets:

  Number of fishing fleets and surveys, for 3.30 models.

- Nfleet:

  Number of fishing fleets, for 3.24 and lower version models.

- Do_AgeKey:

  Flag to indicate if 7 additional ageing error parameters to be read
  set 1 (but in fact any non zero numeric in R) or TRUE to enable to
  read them 0 or FALSE to disable them. This information is not
  explicitly available in control file and used only if
  `use_datlist = FALSE`.

- Nsurveys:

  Number of surveys, for 3.24 and lower version models.

- N_tag_groups:

  number of tag release group. Default =NA. This information is not
  explicitly available control file and used only if
  `use_datlist = FALSE`. This information is only required if custom tag
  parameters is enabled (TG_custom=1)

- N_CPUE_obs:

  Number of CPUE observations. Used only in control file 3.24 syntax if
  `use_datlist = FALSE`.

- catch_mult_fleets:

  Integer vector of fleets using the catch multiplier option. Defaults
  to NULL and should be left as such if 1) the catch multiplier option
  is not used for any fleet or 2) `use_datlist = TRUE` and datlist is
  specified. Used only in control file 3.30 syntax if
  `use_datlist = FALSE`.

- predM_fleets:

  integer vector of fleets with predator mortality included. Predator
  mortality fleets are only available in v3.30.18 and higher. Defaults
  to NULL and should be left as such if 1) predation mortality is not
  used for any fleets; 2) `use_datlist = TRUE` and `datlist` is
  specified; or 3) if comments in the control file should be used
  instead to determine the the predM_fleets. Used only in control file
  3.30 syntax if `use_datlist = FALSE`.

- Ntag_fleets:

  The number of catch fleets in the model (fleets of ) type 1 or 2; not
  surveys). Used to set the number of survey parameters. Only used in
  control file 3.30 reading if tagging data is in the model and
  `use_datlist = FALSE`.

- N_rows_equil_catch:

  Integer value of the number of parameter lines to read for equilibrium
  catch. Defaults to NULL, which means the function will attempt to
  figure out how many lines of equilibrium catch to read from the
  control file comments. Used only in control file 3.30 syntax if
  `use_datlist = FALSE`.

- N_dirichlet_parms:

  Integer value of the number of Dirichlet-Multinomial parameters.
  Defaults to 0. Used only in control file 3.30 syntax if
  `use_datlist = FALSE`.

- ptype:

  Deprecated.

## Value

A list structure where each element is a section of the control file.

## See also

Other read/write functions:
[`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md),
[`SS_writectl()`](https://r4ss.github.io/r4ss/reference/SS_writectl.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md)

## Author

Ian G. Taylor, Yukio Takeuchi, Neil L. Klaer, Kelli F. Johnson, Kathryn
L. Doering, Nathan R. Vaughan

## Examples

``` r
# Read in the 'simple' example SS model stored in r4ss
# Find the directory
dirsimple <- system.file("extdata", "simple_small", package = "r4ss")
# Read in the dat file to define the structure of the control file so that
# you don't have to specify things in the function call such as 'Nfleet'
datfilename <- dir(dirsimple, pattern = "data\\.ss", full.names = TRUE)
dat <- r4ss::SS_readdat(file = datfilename, verbose = FALSE)
# Read in the control file using a list object for datlist
ctl <- r4ss::SS_readctl(
  file = dir(dirsimple, pattern = "control\\.ss$", full.names = TRUE),
  verbose = FALSE,
  datlist = dat, use_datlist = TRUE
)
# Read in the control file using a file name for datlist
ctl <- r4ss::SS_readctl(
  file = dir(dirsimple, pattern = "control\\.ss$", full.names = TRUE),
  verbose = FALSE,
  datlist = datfilename, use_datlist = TRUE
)
```
