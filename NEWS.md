# r4ss 1.47.0 (XX February 2023)
* Support new SS3 option to estimate separate Dirichlet-multinomial parameters 
  for retained vs discards
  * SS3 version 3.30.21 models will also have all info on D-M
    parameters read directly from Report.sso instead of the data file 
* Numerous minor bug fixes
* Adds dependency on 'dplyr' and 'magrittr'

# r4ss 1.46.1 (29 July 2022)
* Major revamp of all functions that run the Stock Synthesis executable
  https://github.com/r4ss/r4ss/pull/722
  * Functions that run the executable have been renamed:
    https://github.com/r4ss/r4ss/issues/723:
    * `run_SS_models()` -> `run()` (now has defaults which allow you to
      simply call `run()` in the current working directory as well as
      and better support for having the Stock Synthesis executable in
      your path)
    * `SS_profile()` -> `profile()`
    * `SS_doRetro()` -> `retro()`
    * `SS_RunJitter()` -> `jitter()`
    * `SS_tune_comps()` -> `tune_comps()`
  * Functions have more consistent inputs (e.g., always `dir` instead of
    `mydir`, `File`, or `masterdir`, and now defaulting to the current
    working directory) 
  * Input `show_in_console = FALSE` can be used with all these functions
    and will pipe output to a text file keeping R console cleaner while
    models run
* Deprecates functions that relate to 3.24 models, like
  `SS_readdat_3.24()` (although `SS_output()` is still compatible back
  to version 3.24) https://github.com/r4ss/r4ss/pull/718 
* Introduces new "simple_small" example model but removes older
  examples, reducing total package size https://github.com/r4ss/r4ss/pull/700
* Uses new columns names in BIOLOGY output https://github.com/r4ss/r4ss/pull/711
* Numerous bug fixes
* Adds dependency on 'tidyr'
* Removes never-completed function `SSbootstrap()`

# r4ss 1.44.0 (23 May 2022)
* Minor improvements and bug fixes
* Compatibility with SS3 version 3.30.19
* Submitted to CRAN

# r4ss 1.43.2 (04 April 2022)
* Standardize version = "3.30" in read and write functions and
  deprecate NULL as an option

# r4ss 1.43.1 (18 February 2022)
* Added Common Utility Functions

# r4ss 1.39.1 (18 August 2020)
* Numerous bug fixes and minor improvements
* Compatibility with SS version 3.30.16

# r4ss 1.38.0 (25 March 2020)
* Numerous bug fixes and minor improvements
* Compatibility with SS version 3.30.15
* Expansion of capabilities for functions to read and write SS input files
* Dependent on fewer packages but now depends on R version 3.5.0 or newer

# r4ss 1.36.1 (16 October 2019)
* Numerous minor bug fixes and minor improvements
* Compatibility with SS versions 3.24 through 3.30.14
* Submitted to CRAN

# r4ss 1.33.1 (5 November 2018)
* Numerous minor bug fixes and minor improvements
* Compatibility with SS versions 3.24 through 3.30.13-beta

# r4ss 1.30.1 (February 2, 2018)
* Fix bugs in new stock-recruit plot, and cohort lines added to bubbles

# r4ss 1.30.0 (January 25, 2018)
* Limited support for models that use Dirichlet-Multinomial likelihood
(currently only works for age compositions, not length comps)

# r4ss 1.29.1 (January 11, 2018)
* Support for SS version 3.30.10.00 available on VLab today
* Some changes in column names and other variables to match
cleanup of terminology in latest Stock Synthesis
* New Vignette (very limited but provides a start to be improved on)

# r4ss 1.25.0 - 1.28.0
* Slacked off on updating this NEWS file

# r4ss 1.24.3
* Implemented automated R package check on Travis-CI:
https://travis-ci.org/r4ss/r4ss

# r4ss 1.24.2
* SS_varadjust now works for use in automating variance adjustments
at the bottom of the control file
* Improvements related to testing SS version 3.30 including converting
SS_readdat into a wrapper function that calls separate functions for
SS v3.24 and v3.30

# r4ss 1.24.1
* Minor bug fixes and other small edits

# r4ss 1.24.0
* First CRAN update since r4ss 1.22.1 in July 2014.
* Variety of new defaults in plots
* Help from various new contributors
* See https://github.com/r4ss/r4ss for more detail
on changes and new features