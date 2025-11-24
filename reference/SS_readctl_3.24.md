# Deprecated: read control file from SS version 3.24

Read Stock Synthesis (version 3.24) control file into list object in R.
This function comes with its wrapper function SS_readctl that calls
SS_readctl_3.24 (this function) or SS_readctl_3.30

## Usage

``` r
SS_readctl_3.24(
  file,
  verbose = FALSE,
  use_datlist = TRUE,
  datlist = "data.ss_new",
  nseas = NULL,
  N_areas = NULL,
  Nages = NULL,
  Nsexes = NULL,
  Npopbins = NA,
  Nfleet = NULL,
  Nsurveys = NULL,
  Do_AgeKey = NULL,
  N_tag_groups = NULL,
  N_CPUE_obs = NULL,
  ptype = lifecycle::deprecated()
)
```

## Arguments

- file:

  Filename either with full path or relative to working directory.

  See the formal arguments for a possible default filename.

- verbose:

  A logical value specifying if output should be printed to the screen.

- use_datlist:

  LOGICAL if TRUE, use datlist to derive parameters which can not be
  determined from control file. Defaults to TRUE

- datlist:

  list or character. if list : produced from SS_writedat or character :
  file name of dat file.

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

- Nfleet:

  number of fisheries in the model. This information is also not
  explicitly available in control file

- Nsurveys:

  number of survey fleets in the model. This information is also not
  explicitly available in control file

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

- N_CPUE_obs:

  numeric vector of length=Nfleet+Nsurveys containing number of data
  points of each CPUE time series

- ptype:

  deprecated.

## Details

Support for 3.24 models within the r4ss `SS_read*` and `SS_write*()`
functions is ending, so please update models to 3.30.

## See also

[`SS_readctl()`](https://r4ss.github.io/r4ss/reference/SS_readctl.md),
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md)
[`SS_readdat_3.24()`](https://r4ss.github.io/r4ss/reference/SS_readdat_3.24.md),[`SS_readdat_3.30()`](https://r4ss.github.io/r4ss/reference/SS_readdat_3.30.md)
[`SS_readstarter()`](https://r4ss.github.io/r4ss/reference/SS_readstarter.md),
[`SS_readforecast()`](https://r4ss.github.io/r4ss/reference/SS_readforecast.md),
[`SS_writestarter()`](https://r4ss.github.io/r4ss/reference/SS_writestarter.md),
[`SS_writeforecast()`](https://r4ss.github.io/r4ss/reference/SS_writeforecast.md),
[`SS_writedat()`](https://r4ss.github.io/r4ss/reference/SS_writedat.md)

## Author

Yukio Takeuchi, Neil Klaer, Iago Mosqueira, Kathryn L. Doering, Nathan
R. Vaughan
