# Iteratively run Stock Synthesis with jittered starting values

Iteratively run a Stock Synthesis model with different jittered starting
parameter values based on the jitter fraction. Output files are renamed
in the format Report1.sso, Report2.sso, etc.

## Usage

``` r
jitter(
  dir = NULL,
  mydir = lifecycle::deprecated(),
  Intern = lifecycle::deprecated(),
  Njitter,
  printlikes = TRUE,
  jitter_fraction = NULL,
  init_values_src = NULL,
  exe = "ss3",
  verbose = FALSE,
  extras = NULL,
  ...
)
```

## Arguments

- dir:

  Directory where model files are located.

- mydir:

  Deprecated. Use `dir` instead.

- Intern:

  Deprecated. Use `show_in_console` instead.

- Njitter:

  Number of jitters, or a vector of jitter iterations. If
  `length(Njitter) > 1` only the iterations specified will be run, else
  `1:Njitter` will be executed.

- printlikes:

  A logical value specifying if the likelihood values should be printed
  to the console.

- jitter_fraction:

  The value, typically 0.1, used to define a uniform distribution in
  cumulative normal space to generate new initial parameter values. The
  default of `NULL` forces the user to specify the jitter_fraction in
  the starter file, and this value must be greater than zero and will
  not be overwritten.

- init_values_src:

  Either zero or one, specifying if the initial values to jitter should
  be read from the control file or from the par file, respectively. The
  default is `NULL`, which will leave the starter file unchanged.

- exe:

  Executable name. Can be just the name of the executable file if it is
  in the specified directory or in the user's PATH. Can also include the
  absolute path or a path relative to the specified directory. Needs to
  be a single character string, not a vector. On Windows, `exe` can
  optionally have the `.exe` extension appended; on Unix-based systems
  (i.e., Mac and Linux), no extension should be included.

- verbose:

  A logical value specifying if output should be printed to the screen.

- extras:

  Additional ADMB command line arguments passed to the executable, such
  as "-nohess"

- ...:

  Additional arguments passed to
  [`run()`](https://r4ss.github.io/r4ss/reference/run.md), such as
  `show_in_console`, and `skipfinished`.

## Value

A vector of likelihoods for each jitter iteration.

## Details

This function will loop through models using the default strategy set by
the `future` package in the current working environment. In general,
this means models will run sequentially. To run multiple models
simultaneously using parallel computing, see
[`future::plan()`](https://future.futureverse.org/reference/plan.html)

Note that random number generation occurs outside of R directly in stock
synthesis. When running jitters in parallel (i.e. `future` strategy is
not `sequential`), no formal steps are taken to ensure independence of
random numbers generated across cores. However, when running in
parallel, there is a pause of i seconds before the ith jitter starts.
The stock synthesis random number seed is based on the system time down
to the second, so this is intended to produce random numbers from
different seeds for each jitter. Random numbers may still not
technically be considered statistically independent. If jitter results
are only used as a general heuristic for model convergence, this mild
lack of independence should not matter much.

When running models in parallel, the transfer of large files leads to
expensive overheads and parallel processing may not be faster.
Covariance files are especially expensive to transfer, so the option
`extras = '-nohess'` is recommended when using parallel processing.

## See also

Other run functions:
[`copy_SS_inputs()`](https://r4ss.github.io/r4ss/reference/copy_SS_inputs.md),
[`populate_multiple_folders()`](https://r4ss.github.io/r4ss/reference/populate_multiple_folders.md),
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md),
[`retro()`](https://r4ss.github.io/r4ss/reference/retro.md),
[`run()`](https://r4ss.github.io/r4ss/reference/run.md),
[`tune_comps()`](https://r4ss.github.io/r4ss/reference/tune_comps.md)

## Author

James T. Thorson, Kelli F. Johnson, Ian G. Taylor, Kathryn L. Doering,
Kiva L. Oken, Elizabeth F. Perl

## Examples

``` r
if (FALSE) { # \dontrun{
#### Run jitter from par file with arbitrary, but common, choice of 0.1
modeldir <- tail(dir(system.file("extdata", package = "r4ss"), full.names = TRUE), 1)
numjitter <- 25
jit.likes <- jitter(
  dir = modeldir, Njitter = numjitter,
  jitter_fraction = 0.1, init_values_src = 1
)

#### Run same jitter in parallel
ncores <- parallelly::availableCores(omit = 1)
future::plan(future::multisession, workers = ncores)
jit.likes <- jitter(
  dir = modeldir, Njitter = numjitter,
  jitter_fraction = 0.1, init_values_src = 1
)
future::plan(future::sequential)

#### Read in results using other r4ss functions
# (note that un-jittered model can be read using keyvec=0:numjitter)
profilemodels <- SSgetoutput(dirvec = modeldir, keyvec = 1:numjitter, getcovar = FALSE)
# summarize output
profilesummary <- SSsummarize(profilemodels)
# Likelihoods
profilesummary[["likelihoods"]][1, ]
# Parameters
profilesummary[["pars"]]
} # }
```
