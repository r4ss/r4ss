# read control file from SS version 3.30

Read Stock Synthesis (version 3.30) control file into list object in R.
This function should be called from SS_readctl.

## Usage

``` r
SS_readctl_3.30(
  file,
  verbose = FALSE,
  use_datlist = TRUE,
  datlist = file.path(dirname(file), "data_echo.ss_new"),
  nseas = NULL,
  N_areas = NULL,
  Nages = NULL,
  Nsexes = NULL,
  Npopbins = NULL,
  Nfleets = NULL,
  Ntag_fleets = NULL,
  Do_AgeKey = NULL,
  N_tag_groups = NULL,
  catch_mult_fleets = NULL,
  predM_fleets = NULL,
  N_rows_equil_catch = NULL,
  N_dirichlet_parms = NULL
)
```

## Arguments

- file:

  Filename either with full path or relative to working directory.

  See the formal arguments for a possible default filename.

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

  number of fishery and survey fleets in the model. This information is
  also not explicitly available in control file

- Ntag_fleets:

  The number of catch fleets in the model (fleets of ) type 1 or 2; not
  surveys). Used to set the number of survey parameters. Only used if
  tagging data is in the model and `use_datlist` is FALSE.

- Do_AgeKey:

  Flag to indicate if 7 additional ageing error parameters to be read
  set 1 (but in fact any non zero numeric in R) or TRUE to enable to
  read them 0 or FALSE to disable them. This information is not
  explicitly available in control file and used only if
  `use_datlist = FALSE`.

- N_tag_groups:

  number of tag release group. Default =NA. This information is not
  explicitly available control file and used only if
  `use_datlist = FALSE`. This information is only required if custom tag
  parameters is enabled (TG_custom=1)

- catch_mult_fleets:

  integer vector of fleets using the catch multiplier option. Defaults
  to NULL and should be left as such if 1) the catch multiplier option
  is not used for any fleets or 2) use_datlist = TRUE and datlist is
  specified.

- predM_fleets:

  integer vector of fleets with predator mortality included. Predator
  mortality fleets are only available in v3.30.18 and higher. Defaults
  to NULL and should be left as such if 1) predation mortality is not
  used for any fleets; 2) use_datlist = TRUE and datlist is specified;
  or 3) if comments in the control file should be used instead to
  determine the the predM_fleets.

- N_rows_equil_catch:

  Integer value of the number of parameter lines to read for equilibrium
  catch. Defaults to NULL, which means the function will attempt to
  figure out how many lines of equilibrium catch to read from the
  control file comments.

- N_dirichlet_parms:

  Integer value of the number of Dirichlet multinomial parameters.
  Defaults to 0.

## See also

[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md)
[`SS_readdat_3.24()`](https://r4ss.github.io/r4ss/reference/SS_readdat_3.24.md),[`SS_readdat_3.30()`](https://r4ss.github.io/r4ss/reference/SS_readdat_3.30.md)
[`SS_readctl_3.24()`](https://r4ss.github.io/r4ss/reference/SS_readctl_3.24.md),
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md)

## Author

Neil Klaer, Yukio Takeuchi, Watal M. Iwasaki, Kathryn L. Doering, Nathan
R. Vaughan
