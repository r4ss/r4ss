# Read MCMC output.

Reads the MCMC output (in the posteriors.sso and derived_posteriors.sso
files) from a model.

## Usage

``` r
SSgetMCMC(
  dir = NULL,
  verbose = TRUE,
  writecsv = FALSE,
  postname = "posteriors.sso",
  derpostname = "derived_posteriors.sso",
  csv1 = "keyposteriors.csv",
  csv2 = "nuisanceposteriors.csv",
  keystrings = c("NatM", "R0", "steep", "RecrDev_2008", "Q_extraSD"),
  nuisancestrings = c("Objective_function", "SSB_", "InitAge", "RecrDev"),
  burnin = 0,
  thin = 1
)
```

## Arguments

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- verbose:

  A logical value specifying if output should be printed to the screen.

- writecsv:

  Write key parameters and certainty nuisance quantities to a CSV file.

- postname:

  Name of file with parameter posteriors (default matches
  "posteriors.sso" used by SS, but the user could change the name)

- derpostname:

  Name of file with parameter posteriors (default matches
  "derived_posteriors.sso" used by SS, but the user could change the
  name)

- csv1:

  First CSV file for key parameters.

- csv2:

  Second CSV file for nuisance quantities.

- keystrings:

  Vector of strings that partially match parameter names to write to the
  file csv1. This file intended to feed into
  [`mcmc.out()`](https://r4ss.github.io/r4ss/reference/mcmc.out.md).

- nuisancestrings:

  Vector of strings that partially match derived quantity names to write
  to the file csv2. This file intended to feed into
  [`mcmc.nuisance()`](https://r4ss.github.io/r4ss/reference/mcmc.nuisance.md).

- burnin:

  Optional burn-in value to apply on top of the option in the starter
  file.

- thin:

  Optional thinning value to apply on top of the option in the starter
  file and in the `-mcsave` runtime command.

## See also

[`mcmc.out()`](https://r4ss.github.io/r4ss/reference/mcmc.out.md),
[`mcmc.nuisance()`](https://r4ss.github.io/r4ss/reference/mcmc.nuisance.md),
[`SSplotPars()`](https://r4ss.github.io/r4ss/reference/SSplotPars.md)

## Author

Ian Taylor
