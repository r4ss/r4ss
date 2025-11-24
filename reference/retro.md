# Run a retrospective analyses

Do retrospective analyses by creating new directories, copying model
files, and iteratively changing the starter file to set the number of
years of data to exclude. Note that there was a bug for retrospectives
in 3.30.01; the user should update their model to a newer version of
Stock Synthesis to run retrospectives. To run retrospective models in
parallel, use
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
before running `retro()`.

## Usage

``` r
retro(
  dir = getwd(),
  masterdir = lifecycle::deprecated(),
  oldsubdir = "",
  newsubdir = "retrospectives",
  subdirstart = "retro",
  years = 0:-5,
  overwrite = TRUE,
  RemoveBlocks = FALSE,
  verbose = FALSE,
  exe = "ss3",
  ...
)
```

## Arguments

- dir:

  Directory where everything takes place.

- masterdir:

  Deprecated. Use `dir` instead.

- oldsubdir:

  Subdirectory within `dir` with existing model files.

- newsubdir:

  Subdirectory within `dir` where retrospectives will be run. Default is
  'retrospectives'.

- subdirstart:

  First part of the pattern of names for the directories in which the
  models will actually be run.

- years:

  Vector of values to iteratively enter into the starter file for
  retrospective year. Should be zero or negative values.

- overwrite:

  Overwrite any input files with matching names in the subdirectories
  where models will be run.

- RemoveBlocks:

  Logical switch determining whether specifications of blocks is removed
  from top of control file. Blocks can cause problems for retrospective
  analyses, but the method for removing them is overly simplistic and
  probably won't work in most cases. Default=FALSE.

- verbose:

  A logical value specifying if output should be printed to the screen.

- exe:

  Executable name. Can be just the name of the executable file if it is
  in the specified directory or in the user's PATH. Can also include the
  absolute path or a path relative to the specified directory. Needs to
  be a single character string, not a vector. On Windows, `exe` can
  optionally have the `.exe` extension appended; on Unix-based systems
  (i.e., Mac and Linux), no extension should be included.

- ...:

  Additional arguments passed to
  [`run()`](https://r4ss.github.io/r4ss/reference/run.md), such as
  `extras`, `show_in_console`, and `skipfinished`.

## See also

[`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md)

Other run functions:
[`copy_SS_inputs()`](https://r4ss.github.io/r4ss/reference/copy_SS_inputs.md),
[`jitter()`](https://r4ss.github.io/r4ss/reference/jitter.md),
[`populate_multiple_folders()`](https://r4ss.github.io/r4ss/reference/populate_multiple_folders.md),
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md),
[`run()`](https://r4ss.github.io/r4ss/reference/run.md),
[`tune_comps()`](https://r4ss.github.io/r4ss/reference/tune_comps.md)

## Author

Ian G. Taylor, James T. Thorson, Kathryn L. Doering, Kiva L. Oken

## Examples

``` r
if (FALSE) { # \dontrun{
# note: don't run this in your main directory--make a copy in case something
# goes wrong
mydir <- "C:/Simple"

## retrospective analyses
retro(
  dir = mydir,
  years = 0:-5
)

retroModels <- SSgetoutput(
  dirvec = file.path(mydir, "retrospectives", paste("retro", 0:-5, sep = ""))
)
retroSummary <- SSsummarize(retroModels)
endyrvec <- retroSummary[["endyrs"]] + 0:-5
SSplotComparisons(retroSummary,
  endyrvec = endyrvec,
  legendlabels = paste("Data", 0:-5, "years")
)

## run retrospectives in parallel
ncores <- parallelly::availableCores(omit = 1)
future::plan(future::multisession, workers = ncores)
retro(
  dir = mydir,
  years = 0:-5
)
future::plan(future::sequential)
} # }
```
