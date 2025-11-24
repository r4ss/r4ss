# Create a plot for the TSC report

Creates a plot of catch and spawning biomass from the output of
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md) for
the NOAA TSC report.

## Usage

``` r
TSCplot(
  SSout,
  yrs = "default",
  ylimBar = "default",
  ylimDepl = c(0, 1.025),
  colBar = "yellow",
  cexBarLabels = 1.1,
  cex.axis = 1.1,
  space = 0,
  pchDepl = 19,
  colDepl = "red",
  lwdDepl = 3,
  shiftDepl = 0.25,
  pchSpace = 5,
  ht = 4,
  wd = 7,
  labelLines = 2.8,
  makePDF = NULL,
  makePNG = NULL,
  MCMC = FALSE
)
```

## Arguments

- SSout:

  The output from
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

- yrs:

  The vector of years to plot

- ylimBar:

  y-axis limits for catch barplot

- ylimDepl:

  y-axis limits for depletion line

- colBar:

  colors of the bars

- cexBarLabels:

  character expansion for the labels underneath the bars (years)

- cex.axis:

  character expansion for the axis labels

- space:

  space between bars (see space argument of `barplot`)

- pchDepl:

  character type for points on the depletion line

- colDepl:

  color of the points on the depletion line

- lwdDepl:

  width of the depletion line

- shiftDepl:

  shift from beginning of the year for the points on the depletion line.
  Helps to guide the eye for exactly which year it corresponds to.

- pchSpace:

  number of years between points on the depletion line. Higher numbers
  help tidy up the plot when plotting many years.

- ht:

  Height of the plot in inches

- wd:

  Width of the plot in inches

- labelLines:

  line argument for `mtext` to move the axis labels

- makePDF:

  filename for a pdf file. If NULL it does not make a pdf. Can specify a
  pdf filename or a png filename. Not both at the same time.

- makePNG:

  filename for a png image. If NULL it does not make a png. Can specify
  a pdf filename or a png filename. Not both at the same time.

- MCMC:

  If TRUE, will use mcmc results. It needs a list element called 'mcmc'
  on SSout.

## Value

Returns a data frame with the years, spawning biomass, depletion, and
total dead catch.

## Details

It creates a plot on the current graphics device, in a pdf file, or as a
png image of the figure used in the TSC report produced by the NWFSC. It
expects the SS results read in by
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md). If
MCMC results are to be plotted, a 'mcmc' list element should be added
using the
[`SSgetMCMC()`](https://r4ss.github.io/r4ss/reference/SSgetMCMC.md)
function. See the examples below.

## See also

[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)
[`SSgetMCMC()`](https://r4ss.github.io/r4ss/reference/SSgetMCMC.md)

## Author

Allan Hicks

## Examples

``` r
if (FALSE) { # \dontrun{

# define directory
directory <- "C:\\NOAA2011\\Dover\\Models\\base_20110701"
# read model output
base <- SS_output(dir = directory, covar = FALSE, verbose = FALSE)

# show the plot in R
TSCplot(base)
TSCplot(base, yrs = 2000:2011, pchSpace = 1)

# Create the plot as a PNG file
TSCplot(base, makePNG = "C:\\NOAA2012\\Assessments\\TSCdover.png")
# Create the plot as a PDF file
TSCplot(base, makePDF = "C:\\NOAA2012\\Assessment\\TSCdover.pdf")

# Model with MCMC results
directory <- "C:/Models"
base <- SS_output(dir = directory, dir.mcmc = "mcmc")
TSCplot(base, ylimDepl = c(0, 1.25), pchSpace = 1, MCMC = TRUE)
} # }
```
