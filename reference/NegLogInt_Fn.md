# Perform SS implementation of Laplace Approximation

(Attempt to) perform the SS implementation of the Laplace Approximation
from Thorson, Hicks and Methot (2014) ICES J. Mar. Sci.

## Usage

``` r
NegLogInt_Fn(
  dir = getwd(),
  File = lifecycle::deprecated(),
  Input_SD_Group_Vec,
  CTL_linenum_List,
  ESTPAR_num_List,
  PAR_num_Vec,
  Int_Group_List = list(1),
  StartFromPar = TRUE,
  Intern = lifecycle::deprecated(),
  ReDoBiasRamp = FALSE,
  BiasRamp_linenum_Vec = NULL,
  CTL_linenum_Type = NULL,
  exe = "ss3",
  verbose = FALSE,
  ...
)
```

## Arguments

- dir:

  Directory containing Stock Synthesis files.

- File:

  Deprecated. Use `dir` instead.

- Input_SD_Group_Vec:

  Vector where each element is the standard deviation for a group of
  random effects (e.g., a model with a single group of random effects
  will have Input_SD_Group_Vec be a vector of length one)

- CTL_linenum_List:

  List (same length as `Input_SD_Group_Vec`), where each element is a
  vector giving the line number(s) for the random effect standard
  deviation parameter or penalty in the CTL file (and where each line
  will correspond to a 7-parameter or 14-parameter line).

- ESTPAR_num_List:

  List (same length as `Input_SD_Group_Vec`), where each element is a
  vector giving the parameter number for the random effect coefficients
  in that group of random effects. These "parameter numbers" correspond
  to the number of these parameters in the list of parameters in the
  ".cor" output file.

- PAR_num_Vec:

  Vector giving the number in the ".par" vector for each random effect
  coefficient.

- Int_Group_List:

  List where each element is a vector, providing a way of grouping
  different random effect groups into a single category. Although this
  input is still required, it is no has the former input Version has
  been hardwired to Version = 1.

- StartFromPar:

  Logical flag (TRUE or FALSE) saying whether to start each round of
  optimization from a ".par" file (I recommend TRUE)

- Intern:

  Deprecated. Use `show_in_console` instead. See
  [`run()`](https://r4ss.github.io/r4ss/reference/run.md) for details.

- ReDoBiasRamp:

  Logical flag saying whether to re-do the bias ramp (using
  [`SS_fitbiasramp()`](https://r4ss.github.io/r4ss/reference/SS_fitbiasramp.md))
  each time Stock Synthesis is run.

- BiasRamp_linenum_Vec:

  Vector giving the line numbers from the CTL file that contain the
  information about the bias ramp.

- CTL_linenum_Type:

  Character vector (same length as `Input_SD_Group_Vec`), where each
  element is either "Short_Param", "Long_Penalty", "Long_Penalty".
  Default is NULL, and if not explicitly specified the program will
  attempt to detect these automatically based on the length of relevant
  lines from the CTL file.

- exe:

  Executable name. Can be just the name of the executable file if it is
  in the specified directory or in the user's PATH. Can also include the
  absolute path or a path relative to the specified directory. Needs to
  be a single character string, not a vector. On Windows, `exe` can
  optionally have the `.exe` extension appended; on Unix-based systems
  (i.e., Mac and Linux), no extension should be included.

- verbose:

  A logical value specifying if output should be printed to the screen.

- ...:

  Additional arguments passed to
  [`run()`](https://r4ss.github.io/r4ss/reference/run.md), such as
  `extras` and `show_in_console`.

## References

Thorson, J.T., Hicks, A.C., and Methot, R.D. 2014. Random effect
estimation of time-varying factors in Stock Synthesis. ICES J. Mar. Sci.

## See also

[`read.admbFit()`](https://r4ss.github.io/r4ss/reference/read.admbFit.md),
[`getADMBHessian()`](https://r4ss.github.io/r4ss/reference/getADMBHessian.md)

## Author

James Thorson

## Examples

``` r
if (FALSE) { # \dontrun{
# need the full path because wd is changed in function
direc <- "C:/Models/LaplaceApprox/base"
if ("Optimization_record.txt" %in% list.files(direc)) {
  file.remove(file.path(direc, "Optimization_record.txt"))
}
Opt <- optimize(
  f = NegLogInt_Fn,
  interval = c(0.001, 0.12),
  maximum = FALSE,
  dir = direc,
  Input_SD_Group_Vec = 1,
  CTL_linenum_List = list(127:131),
  ESTPAR_num_List = list(86:205),
  Int_Group_List = 1,
  PAR_num_Vec = NA,
  Intern = TRUE
)
} # }
```
