# Summarize, analyze and plot key MCMC output.

Makes four panel plot showing trace plots, moving average,
autocorrelations, and densities for chosen parameters from MCMC output.

## Usage

``` r
mcmc.out(
  directory = "c:/mydirectory/",
  run = "mymodel/",
  file = "keyposteriors.csv",
  namefile = "postplotnames.sso",
  names = FALSE,
  headernames = TRUE,
  numparams = 1,
  closeall = TRUE,
  burn = 0,
  thin = 1,
  scatter = FALSE,
  surface = FALSE,
  surf1 = 1,
  surf2 = 2,
  stats = FALSE,
  plots = TRUE,
  header = TRUE,
  sep = ",",
  print = FALSE,
  new = T,
  colNames = NULL
)
```

## Arguments

- directory:

  Directory where all results are located, one level above directory for
  particular run.

- run:

  Directory with files from a particular run.

- file:

  Filename either with full path or relative to working directory.

  Contents of the file that is referenced here should contain posterior
  samples for nuisance parameters, e.g., posteriors.sso or something
  written by
  [`SSgetMCMC`](https://r4ss.github.io/r4ss/reference/SSgetMCMC.md).

- namefile:

  The (optional) file name of the dimension and names of posteriors.

- names:

  Read in names file (T) or use generic naming (F).

- headernames:

  Use the names in the header of `file`?

- numparams:

  The number of parameters to analyze.

- closeall:

  By default close all open devices.

- burn:

  Optional burn-in value to apply on top of the option in the starter
  file and
  [`SSgetMCMC()`](https://r4ss.github.io/r4ss/reference/SSgetMCMC.md).

- thin:

  Optional thinning value to apply on top of the option in the starter
  file, in the `-mcsave` runtime command, and in
  [`SSgetMCMC()`](https://r4ss.github.io/r4ss/reference/SSgetMCMC.md).

- scatter:

  Can add a scatter-plot of all params at end, default is none.

- surface:

  Add a surface plot of 2-way correlations.

- surf1:

  The first parameter for the surface plot.

- surf2:

  The second parameter for the surface plot.

- stats:

  Print stats if desired.

- plots:

  Show plots or not.

- header:

  Data file with header?

- sep:

  Separator for data file passed to the `read.table` function.

- print:

  Send to screen unless asked to print.

- new:

  Logical whether or not to open a new plot window before plotting

- colNames:

  Specific names of the `file` to extract and work with. `NULL` keeps
  all columns

## Value

`directory`, because this function is used for its plotting side effects

## See also

[`mcmc.nuisance()`](https://r4ss.github.io/r4ss/reference/mcmc.nuisance.md),
[`SSgetMCMC()`](https://r4ss.github.io/r4ss/reference/SSgetMCMC.md)

## Author

Ian Stewart, Allan Hicks (modifications)

## Examples

``` r
if (FALSE) { # \dontrun{
mcmc.df <- SSgetMCMC(
  dir = "mcmcRun", writecsv = T,
  keystrings = c("NatM", "R0", "steep", "Q_extraSD"),
  nuisancestrings = c("Objective_function", "SSB_", "InitAge", "RecrDev")
)
mcmc.out("mcmcRun", run = "", numparams = 4, closeall = F)

# Or for more control
par(mar = c(5, 3.5, 0, 0.5), oma = c(0, 2.5, 0.2, 0))
mcmc.out("mcmcRun",
  run = "",
  numparams = 1,
  closeall = F,
  new = F,
  colNames = c("NatM_p_1_Fem_GP_1")
)
mtext("M (natural mortality)", side = 2, outer = T, line = 1.5, cex = 1.1)
} # }
```
