# A function to create a list object for the output from Stock Synthesis

Reads the Report.sso and (optionally) the covar.sso, CompReport.sso and
other files produced by Stock Synthesis and formats the important
content of these files into a list in the R workspace. A few statistics
unavailable elsewhere are taken from the .par file. Summary information
and statistics can be returned to the R console or just contained within
the list produced by this function.

## Usage

``` r
SS_output(
  dir = "C:/myfiles/mymodels/myrun/",
  dir.mcmc = NULL,
  repfile = "Report.sso",
  compfile = "CompReport.sso",
  covarfile = "covar.sso",
  forefile = "Forecast-report.sso",
  wtfile = "wtatage.ss_new",
  warnfile = "warning.sso",
  ncols = lifecycle::deprecated(),
  forecast = TRUE,
  warn = TRUE,
  covar = TRUE,
  readwt = TRUE,
  verbose = TRUE,
  printstats = TRUE,
  hidewarn = FALSE,
  NoCompOK = TRUE,
  aalmaxbinrange = 4,
  SpawnOutputLabel = "Spawning output"
)
```

## Arguments

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- dir.mcmc:

  Optional directory containing MCMC output. This can either be relative
  to `dir`, such that `file.path(dir, dir.mcmc)` will end up in the
  right place, or an absolute path.

- repfile:

  Name of the big report file (could be renamed by user).

- compfile:

  Name of the composition report file.

- covarfile:

  Name of the covariance output file.

- forefile:

  Name of the forecast file.

- wtfile:

  Name of the file containing weight at age data.

- warnfile:

  Name of the file containing warnings.

- ncols:

  Deprecated. This value is now calculated automatically.

- forecast:

  Read the forecast-report file?

- warn:

  Read the Warning.sso file?

- covar:

  Read covar.sso?

- readwt:

  Read the weight-at-age file?

- verbose:

  A logical value specifying if output should be printed to the screen.

- printstats:

  Print summary statistics about the output to the R GUI?

- hidewarn:

  Hides some warnings output from the R GUI.

- NoCompOK:

  Allow the function to work without a CompReport file.

- aalmaxbinrange:

  The largest length bin range allowed for composition data to be
  considered as conditional age-at-length data.

- SpawnOutputLabel:

  An alternative to "Spawning output" for use in figure axis labels and
  table headers for models that include a fecundity relationship. This
  provides an option to provide the units, e.g.
  `SpawnOutputLabel = "Spawning output (trillions of eggs)"`. This needs
  to be a user input because the units depend on the choice of fecundity
  parameters which are calculated outside of the SS3 model.

## Value

Many values are returned. Complete list would be quite long, but should
probably be created at some point in the future.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md)

## Author

Ian Stewart, Ian Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
# read model output
myreplist <- SS_output(dir = "c:/SS/Simple/")
# make a bunch of plots
SS_plots(myreplist)

# read model output and also read MCMC results (if run), which in
# this case would be stored in c:/SS/Simple/mcmc/
myreplist <- SS_output(dir = "c:/SS/Simple/", dir.mcmc = "mcmc")
} # }
```
