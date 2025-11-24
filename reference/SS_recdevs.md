# Insert a vector of recruitment deviations into the control file.

A function to insert a vector of recruitment deviations into the control
file for simulation studies. This function was written in 2010, long
before the functions to read and write the input files were created. An
alternative approach would be to read the control file, add the recdevs,
and then write it again, but this function still works so there's no
immediate need to streamline that alternative approach.

## Usage

``` r
SS_recdevs(
  fyr,
  lyr,
  ctl = NULL,
  recdevs = NULL,
  rescale = TRUE,
  scaleyrs = NULL,
  dir = getwd(),
  ctlfile = "control.ss_new",
  newctlfile = "control_modified.ss",
  verbose = TRUE,
  writectl = TRUE,
  returnctl = FALSE,
  newmaxbias = NULL
)
```

## Arguments

- fyr:

  First year of the recdev vector.

- lyr:

  Last year of the recdev vector.

- ctl:

  Either NULL to read anew or an already read control file.
  Default=NULL.

- recdevs:

  Either NULL to generate anew or an already generated vector of
  recdevs. Default=NULL.

- rescale:

  Should the recdevs be rescaled to have mean = 0 and std. deviation =
  sigmaR? Default=TRUE.

- scaleyrs:

  Vector of years over which rescaling (if chosen) should occur.

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- ctlfile:

  Name of control file to modify. Default="control.ss_new".

- newctlfile:

  Name of new file to output modified control file.
  Default="control_modified.ss".

- verbose:

  A logical value specifying if output should be printed to the screen.

- writectl:

  Write new file? Default=TRUE.

- returnctl:

  Return contents ctl file as an object in the R workspace.
  Default=FALSE.

- newmaxbias:

  Replace the maximum bias adjustment fraction with any non-NULL value.
  Default=NULL.

## Author

Ian Taylor
