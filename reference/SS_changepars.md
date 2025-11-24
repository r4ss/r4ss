# Change parameters, bounds, or phases in the control file.

Loops over a subset of control file to change parameter lines. Current
initial value, lower and upper bounds, and phase can be modified, but
function could be expanded to control other columns. Depends on
[`SS_parlines()`](https://r4ss.github.io/r4ss/reference/SS_parlines.md).
Used by [`profile()`](https://r4ss.github.io/r4ss/reference/profile.md)
and the ss3sim package.

## Usage

``` r
SS_changepars(
  dir = NULL,
  ctlfile = "control.ss_new",
  newctlfile = "control_modified.ss",
  linenums = NULL,
  strings = NULL,
  newvals = NULL,
  repeat.vals = FALSE,
  newlos = NULL,
  newhis = NULL,
  newprior = NULL,
  newprsd = NULL,
  newprtype = NULL,
  estimate = NULL,
  verbose = TRUE,
  newphs = NULL
)
```

## Arguments

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- ctlfile:

  Control file name. Default="control.ss_new".

- newctlfile:

  Name of new control file to be written. Default="control_modified.ss".

- linenums:

  Line numbers of control file to be modified. Either this or the
  `strings` argument are needed. Default=NULL.

- strings:

  Strings (with optional partial matching) indicating which parameters
  to be modified. This is an alternative to `linenums`. `strings`
  correspond to the commented parameter names included in
  `control.ss_new`, or whatever is written as comment at the end of the
  14 number parameter lines. If `strings` is an exact match to a
  parameter name in `control.ss_new`, this parameter will be modified,
  otherwise the function will check for parameters that are a partial
  match to `strings.` Default=NULL.

- newvals:

  Vector of new parameter values. Default=NULL. The vector can contain
  `NA` values, which will assign the original value to the given
  parameter but change the remainder parameters, where the vector of
  values needs to be in the same order as either `linenums` or
  `strings`.

- repeat.vals:

  If multiple parameter lines match criteria, repeat the `newvals` input
  for each line.

- newlos:

  Vector of new lower bounds. Default=NULL. The vector can contain `NA`
  values, which will assign the original value to the given parameter
  but change the remainder parameters, where the vector of values needs
  to be in the same order as either `linenums` or `strings`.

- newhis:

  Vector of new high bounds. Must be the same length as newhis
  Default=NULL. The vector can contain `NA` values, which will assign
  the original value to the given parameter but change the remainder
  parameters, where the vector of values needs to be in the same order
  as either `linenums` or `strings`.

- newprior:

  Vector of new prior values. Default=NULL. The vector can contain `NA`
  values, which will assign the original value to the given parameter
  but change the remainder parameters, where the vector of values needs
  to be in the same order as either `linenums` or `strings`.

- newprsd:

  Vector of new prior sd values. Default=NULL. The vector can contain
  `NA` values, which will assign the original value to the given
  parameter but change the remainder parameters, where the vector of
  values needs to be in the same order as either `linenums` or
  `strings`.

- newprtype:

  Vector of new prior type. Default=NULL. The vector can contain `NA`
  values, which will assign the original value to the given parameter
  but change the remainder parameters, where the vector of values needs
  to be in the same order as either `linenums` or `strings`.

- estimate:

  Optional vector or single value of TRUE/FALSE for which parameters are
  to be estimated. Changes sign of phase to be positive or negative.
  Default `NULL` causes no change to phase.

- verbose:

  A logical value specifying if output should be printed to the screen.

- newphs:

  Vector of new phases. Can be a single value, which will be repeated
  for each parameter, the same length as newvals, where each value
  corresponds to a single parameter, or `NULL`, where the phases will
  not be changed. If one wants to strictly turn parameters on or off and
  not change the phase in which they are estimated use `estimate = TRUE`
  or `estimate = FALSE`, respectively. The vector can contain `NA`
  values, which will assign the original value to the given parameter
  but change the remaining parameters, where the vector of values needs
  to be in the same order as either `linenums` or `strings`.

## See also

[`SS_parlines()`](https://r4ss.github.io/r4ss/reference/SS_parlines.md),
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md)

## Author

Ian Taylor, Christine Stawitz, Chantel Wetzel, Kiva L. Oken

## Examples

``` r
if (FALSE) { # \dontrun{
SS_changepars(
  dir = "C:/ss/SSv3.30.03.05_May11/Simple - Copy",
  strings = c("steep", "sigmaR"), newvals = c(.4, .6)
)
## parameter names in control file matching input vector 'strings' (n=2):
## [1] "SR_BH_steep" "SR_sigmaR"
## These are the ctl file lines as they currently exist:
##     LO HI     INIT PRIOR PR_type SD PHASE env_var&link dev_link dev_minyr dev_maxyr
## 95 0.2  1 0.613717   0.7    0.05  1     4       0       0         0         0
## 96 0.0  2 0.600000   0.8    0.80  0    -4       0       0         0         0
##        dev_PH Block Block_Fxn       Label Linenum
## 95          0     0         0 SR_BH_steep      95
## 96          0     0         0   SR_sigmaR      96
## line numbers in control file (n=2):
## [1] 95 96
##
## wrote new file to control_modified.ss with the following changes:
##    oldvals newvals oldphase newphase oldlos newlos oldhis newhis       comment
## 1 0.613717     0.4        4       -4    0.2    0.2      1      1 # SR_BH_steep
## 2 0.600000     0.6       -4       -4    0.0    0.0      2      2   # SR_sigmaR
} # }
```
