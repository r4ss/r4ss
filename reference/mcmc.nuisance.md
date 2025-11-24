# Summarize nuisance MCMC output

Summarize nuisance MCMC output (used in combination with
[`mcmc.out()`](https://r4ss.github.io/r4ss/reference/mcmc.out.md) for
key parameters).

## Usage

``` r
mcmc.nuisance(
  directory = "c:/mydirectory/",
  run = "mymodel/",
  file = "posteriors.sso",
  file2 = "derived_posteriors.sso",
  bothfiles = FALSE,
  printstats = FALSE,
  burn = 0,
  header = TRUE,
  thin = 1,
  trace = 0,
  labelstrings = "all",
  columnnumbers = "all",
  sep = ""
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

- file2:

  Optional second file containing posterior samples for nuisance
  parameters. This could be derived_posteriors.sso.

- bothfiles:

  TRUE/FALSE indicator on whether to read `file2` in addition to
  `file1`.

- printstats:

  Return all the statistics for a closer look.

- burn:

  Optional burn-in value to apply on top of the option in the starter
  file and
  [`SSgetMCMC()`](https://r4ss.github.io/r4ss/reference/SSgetMCMC.md).

- header:

  Data file with header?

- thin:

  Optional thinning value to apply on top of the option in the starter
  file, in the `mcsave` runtime command, and in
  [`SSgetMCMC()`](https://r4ss.github.io/r4ss/reference/SSgetMCMC.md).

- trace:

  Plot trace for param \# (to help sort out problem parameters).

- labelstrings:

  Vector of strings that partially match the labels of the parameters
  you want to consider.

- columnnumbers:

  Vector of column numbers indicating the columns you want to consider.

- sep:

  Separator for data file passed to the `read.table` function.

## See also

[`mcmc.out()`](https://r4ss.github.io/r4ss/reference/mcmc.out.md),
[`SSgetMCMC()`](https://r4ss.github.io/r4ss/reference/SSgetMCMC.md)

## Author

Ian Stewart
