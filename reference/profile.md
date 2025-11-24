# Run a likelihood profile in Stock Synthesis.

Iteratively changes the control file for the chosen parameter. This
function was formerly called
[`SS_profile()`](https://r4ss.github.io/r4ss/reference/SS_profile.md).

## Usage

``` r
profile(
  dir,
  oldctlfile = "control.ss_new",
  masterctlfile = lifecycle::deprecated(),
  newctlfile = "control_modified.ss",
  linenum = NULL,
  string = NULL,
  profilevec = NULL,
  usepar = FALSE,
  globalpar = FALSE,
  parlinenum = NULL,
  parstring = NULL,
  saveoutput = TRUE,
  overwrite = TRUE,
  whichruns = NULL,
  prior_check = TRUE,
  read_like = lifecycle::deprecated(),
  exe = "ss3",
  verbose = TRUE,
  conv_criteria = 0.01,
  ...
)
```

## Arguments

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- oldctlfile:

  Source control file. Default = "control.ss_new"

- masterctlfile:

  Deprecated. Use `oldctlfile` instead.

- newctlfile:

  Destination for new control files (must match entry in starter file).
  Default = "control_modified.ss".

- linenum:

  Line number of parameter to be changed. Can be used instead of
  `string` or left as NULL. Can be a vector if you are profiling
  multiple parameters at the same time.

- string:

  String partially matching name of parameter to be changed. Can be used
  instead of `linenum` or left as NULL. Can be a vector if you are
  profiling multiple parameters at the same time.

- profilevec:

  Vector of values to profile over. If you are profileing over multiple
  parameters at the same time this should be a data.frame or matrix with
  a column for each parameter.

- usepar:

  Use PAR file from previous profile step for starting values?

- globalpar:

  Use global par file (`parfile_original_backup.sso`, which is
  automatically copied from original `ss.par` or `ss3.par` depending on
  SS3 version) for all runs instead of the par file from each successive
  run

- parlinenum:

  Line number in par file to change (if usepar = TRUE). Can be a vector
  if you are profiling multiple parameters at the same time.

- parstring:

  String in par file preceding line number to change as an alternative
  to parlinenum (only needed if usepar = TRUE). Can be a vector if you
  are profiling multiple parameters at the same time.

- saveoutput:

  Copy output .sso files to unique names. Default = TRUE.

- overwrite:

  Overwrite any existing .sso files. Default = TRUE. If FALSE, then some
  runs may be skipped.

- whichruns:

  Optional vector of run indices to do. This can be used to re-run a
  subset of the cases in situations where the function was interrupted
  or some runs fail to converge. Must be a subset of 1:n, where n is the
  length of profilevec.

- prior_check:

  Check to make sure the starter file is set to include the prior
  likelihood contribution in the total likelihood. Default = TRUE.

- read_like:

  Deprecated.

- exe:

  Executable name. Can be just the name of the executable file if it is
  in the specified directory or in the user's PATH. Can also include the
  absolute path or a path relative to the specified directory. Needs to
  be a single character string, not a vector. On Windows, `exe` can
  optionally have the `.exe` extension appended; on Unix-based systems
  (i.e., Mac and Linux), no extension should be included.

- verbose:

  A logical value specifying if output should be printed to the screen.

- conv_criteria:

  Maximum gradient for a model to be considered converged. Defaults to
  0.01.

- ...:

  Additional arguments passed to
  [`run()`](https://r4ss.github.io/r4ss/reference/run.md), such as
  `extras`, `show_in_console`, and `skipfinished`.

## Note

The starting values used in this profile are not ideal and some models
may not converge. Care should be taken in using an automated tool like
this, and some models are likely to require rerunning with alternate
starting values.

To run multiple models simultaneously using parallel computing, see
[`future::plan()`](https://future.futureverse.org/reference/plan.html).
However, when running models in parallel, you cannot iteratively adapt
the starting values using `usepar = TRUE` and `globalpar = FALSE`. This
increases the chances that some of your models do not converge.

Also, someday this function will be improved to work directly with the
plotting function
[`SSplotProfile()`](https://r4ss.github.io/r4ss/reference/SSplotProfile.md),
but they don't yet work well together. Thus, even if `profile()` is
used, the output should be read using
[`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md)
or by multiple calls to
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)
before sending to
[`SSplotProfile()`](https://r4ss.github.io/r4ss/reference/SSplotProfile.md).

## See also

[`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md),
[`SS_changepars()`](https://r4ss.github.io/r4ss/reference/SS_changepars.md),
[`SS_parlines()`](https://r4ss.github.io/r4ss/reference/SS_parlines.md)

Other run functions:
[`copy_SS_inputs()`](https://r4ss.github.io/r4ss/reference/copy_SS_inputs.md),
[`jitter()`](https://r4ss.github.io/r4ss/reference/jitter.md),
[`populate_multiple_folders()`](https://r4ss.github.io/r4ss/reference/populate_multiple_folders.md),
[`retro()`](https://r4ss.github.io/r4ss/reference/retro.md),
[`run()`](https://r4ss.github.io/r4ss/reference/run.md),
[`tune_comps()`](https://r4ss.github.io/r4ss/reference/tune_comps.md)

Other profile functions:
[`PinerPlot()`](https://r4ss.github.io/r4ss/reference/PinerPlot.md),
[`SSplotProfile()`](https://r4ss.github.io/r4ss/reference/SSplotProfile.md)

## Author

Ian G. Taylor, Kathryn L. Doering, Kelli F. Johnson, Chantel R. Wetzel,
James T. Thorson, Kiva L. Oken

## Examples

``` r
if (FALSE) { # \dontrun{

###########################################################################
# example profile
# (assumes you have an SS3 exe called "ss3.exe" or "ss3" in your PATH)
###########################################################################

# directory for "simple_small" example model included with r4ss
dir_simple_small <- file.path(
  path.package("r4ss"),
  file.path("extdata", "simple_small")
)

# create temporary directory and copy files into it
dir_prof <- file.path(tempdir(), "profile")
copy_SS_inputs(
  dir.old = dir_simple_small,
  dir.new = dir_prof,
  create.dir = TRUE,
  overwrite = TRUE,
  copy_par = TRUE,
  verbose = TRUE
)

# the following commands related to starter.ss could be done by hand
# read starter file
starter <- SS_readstarter(file.path(dir_prof, "starter.ss"))
# change control file name in the starter file
starter[["ctlfile"]] <- "control_modified.ss"
# make sure the prior likelihood is calculated
# for non-estimated quantities
starter[["prior_like"]] <- 1
# write modified starter file
SS_writestarter(starter, dir = dir_prof, overwrite = TRUE)
# vector of values to profile over
h.vec <- seq(0.3, 0.9, .1)
Nprofile <- length(h.vec)
# run profile command
prof.table <- profile(
  dir = dir_prof,
  oldctlfile = "control.ss",
  newctlfile = "control_modified.ss",
  string = "steep", # subset of parameter label
  profilevec = h.vec
)
# read the output files (with names like Report1.sso, Report2.sso, etc.)
profilemodels <- SSgetoutput(dirvec = dir_prof, keyvec = 1:Nprofile)
# summarize output
profilesummary <- SSsummarize(profilemodels)

# OPTIONAL COMMANDS TO ADD MODEL WITH PROFILE PARAMETER ESTIMATED
# (in the "simple_small" example, steepness is fixed so it doesn't
# have any impact)
MLEmodel <- SS_output(dir_simple_small, verbose = FALSE, printstats = FALSE)
profilemodels[["MLE"]] <- MLEmodel
profilesummary <- SSsummarize(profilemodels)
# END OPTIONAL COMMANDS

# plot profile using summary created above
results <- SSplotProfile(profilesummary, # summary object
  profile.string = "steep", # substring of profile parameter
  profile.label = "Stock-recruit steepness (h)"
) # axis label

# make timeseries plots comparing models in profile
SSplotComparisons(profilesummary, legendlabels = paste("h =", h.vec))

# run same profile in parallel
ncores <- parallelly::availableCores(omit = 1)
future::plan(future::multisession, workers = ncores)
prof.table <- profile(
  dir = dir_prof,
  oldctlfile = "control.ss",
  newctlfile = "control_modified.ss",
  string = "steep", # subset of parameter label
  profilevec = h.vec
)
future::plan(future::sequential)

###########################################################################
# example two-dimensional profile
# (assumes you have an SS3 exe called "ss3.exe" or "ss3" in your PATH)
###########################################################################

dir_simple_small <- file.path(
  path.package("r4ss"),
  file.path("extdata", "simple_small")
)

# create temporary directory and copy files into it
dir_prof <- file.path(tempdir(), "profile_2D")
copy_SS_inputs(
  dir.old = dir_simple_small,
  dir.new = dir_prof,
  create.dir = TRUE,
  overwrite = TRUE,
  copy_par = TRUE,
  verbose = TRUE
)


# create table of M values for females and males
par_table <- expand.grid(
  M1vec = c(0.05, 0.10, 0.15),
  M2vec = c(0.05, 0.10, 0.15)
)

# run model once to create control.ss_new with
# good starting parameter values
# exe is assumed to be in PATH, add "exe" argument if needed
run(dir_prof, extras = "-nohess")

# run profile using ss_new file as parameter source and
# overwriting original control file with new values
prof.table <- profile(
  dir = dir_prof,
  oldctlfile = "control.ss_new",
  newctlfile = "control.ss",
  string = c("NatM_uniform_Fem_GP_1", "NatM_uniform_Mal_GP_1"),
  profilevec = par_table,
  extras = "-nohess"
)

# get model output
profilemodels <- SSgetoutput(
  dirvec = dir_prof,
  keyvec = 1:nrow(par_table), getcovar = FALSE
)
n <- length(profilemodels)
profilesummary <- SSsummarize(profilemodels)

# add total likelihood (row 1) to table created above
par_table[["like"]] <- as.numeric(profilesummary[["likelihoods"]][1, 1:n])

# reshape data frame into a matrix for use with contour
like_matrix <- reshape2::acast(
  data = par_table,
  formula = M1vec ~ M2vec,
  value.var = "like"
)

# look at change relative to the minimum
# (shows small change when female and male M are equal,
# big change when they are different)
like_matrix - min(like_matrix)
#         0.05    0.1    0.15
# 0.05   6.938 32.710 121.959
# 0.1   49.706  0.000  27.678
# 0.15 154.897 44.768   5.366
} # }
```
