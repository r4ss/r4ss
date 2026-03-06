# Shared parameter documentation

This function exists solely to provide shared parameter documentation
via `@inheritParams` for other functions in the package.

## Usage

``` r
r4ss_params(
  replist,
  verbose,
  plotdir,
  plot,
  print,
  res,
  pwidth,
  pheight,
  pheight_tall,
  punits,
  ptsize,
  cex.main,
  mainTitle,
  mar,
  labels,
  fleets,
  fleetnames,
  legendloc,
  legend,
  lwd,
  bioscale,
  overwrite,
  dir,
  version,
  extras,
  model,
  areacols,
  exe,
  file,
  use_datlist,
  datlist,
  nseas,
  N_areas,
  Nages,
  Nsexes,
  Npopbins,
  Do_AgeKey,
  N_tag_groups,
  ...
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- verbose:

  A logical value specifying if output should be printed to the screen.

- plotdir:

  Directory where PNG files will be written.

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- res:

  Resolution of plots printed to files. The default is `res = 300`.

- pwidth:

  Default width of plots printed to files in units of `punits`.

- pheight:

  Height of plots printed to png files in units of `punits`. Default is
  designed to allow two plots per page, with `pheight_tall` used for
  plots that work best with a taller format and a single plot per page.

- pheight_tall:

  Height of tall plots printed to png files in units of `punits`, where
  the tall plots are a subset of the plots which typically work best in
  a taller format.

- punits:

  Units for `pwidth` and `pheight`. Can be "px" (pixels), "in" (inches),
  "cm" (centimeters), or "mm" (millimeters). The default is
  `punits="in"`.

- ptsize:

  Point size for plotted text in plots printed to files (see
  [`help("png")`](https://rdrr.io/r/grDevices/png.html) in R for
  details).

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

- mainTitle:

  Logical indicating if a title should be included at the top (not yet
  implemented for all plots).

- mar:

  Either NULL to allow the default (which depends on whether the main
  title is included or not) or a numerical vector of the form c(bottom,
  left, top, right) which gives the number of lines of margin to be
  specified on the four sides of the plot, which is passed to
  [`par()`](https://rdrr.io/r/graphics/par.html).

- labels:

  Vector of labels for plots (titles and axis labels).

- fleets:

  Either the string "all", or a vector of numerical values, like c(1,3),
  listing fleets or surveys to be included in the plot.

- fleetnames:

  Optional replacement for fleetnames used in data file.

- legendloc:

  Location of legend. Either a string like "topleft" or a vector of two
  numeric values representing the fraction of the maximum in the x and y
  dimensions, respectively. See
  [`help("legend")`](https://rdrr.io/r/graphics/legend.html) for more
  info on the string options.

- legend:

  Add a legend?

- lwd:

  Line width for plot elements.

- bioscale:

  scaling for spawning biomass. Default = 1. Previously this was set to
  0.5 for single-sex models, and 1.0 for all others, but now single-sex
  models are assumed to use the -1 option for Nsexes in the data file so
  the scaling is done automatically by SS3.

- overwrite:

  A logical value specifying if the existing file(s) should be
  overwritten. The default value is `overwrite = FALSE`.

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- version:

  SS version number. Currently "3.24" or "3.30" are supported, either as
  character or numeric values (noting that numeric 3.30 = 3.3).
  `version = NULL` is no longer the default or an allowed entry. The
  default is `version = "3.30"`.

- extras:

  Additional ADMB command line arguments passed to the executable, such
  as "-nohess"

- model:

  Name of the Stock Synthesis model file (which has the .exe for on
  Windows) in `mydir` without the extension (if any), e.g., `"ss3"` or
  `"ss_win"`.

- areacols:

  Optional vector of colors for each area if model has multiple areas.
  NULL value will be replaced by a default set of areas.

- exe:

  Executable name. Can be just the name of the executable file if it is
  in the specified directory or in the user's PATH. Can also include the
  absolute path or a path relative to the specified directory. Needs to
  be a single character string, not a vector. On Windows, `exe` can
  optionally have the `.exe` extension appended; on Unix-based systems
  (i.e., Mac and Linux), no extension should be included.

- file:

  Filename either with full path or relative to working directory. See
  the formal arguments for a possible default filename.

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
  not explicitly available in control file and used only if
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
  (Maturity option of 6). Used only if `use_datlist = FALSE`.

- Do_AgeKey:

  Flag to indicate if 7 additional ageing error parameters to be read.
  Set 1 (but in fact any non zero numeric in R) or TRUE to enable
  reading them; 0 or FALSE to disable them. This information is not
  explicitly available in control file and used only if
  `use_datlist = FALSE`.

- N_tag_groups:

  number of tag release group. Default = NA. This information is not
  explicitly available in control file and used only if
  `use_datlist = FALSE`. This information is only required if custom tag
  parameters is enabled (TG_custom=1).

- ...:

  Additional arguments (unused, for compatibility only).
