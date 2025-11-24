# Plot a two-panel comparison using SSplotComparisons

Plot a two-panel time series comparison of spawning biomass and fraction
of unfished spawning output (or any other subplots, like recruitment and
recdevs). By sharing the same horizontal axis label and requiring only a
single caption, this makes it easier to show two quantities on a single
page of a report.

## Usage

``` r
plot_twopanel_comparison(
  mods,
  legendlabels = NULL,
  filename = NULL,
  dir = NULL,
  subplot1 = NULL,
  subplot2 = NULL,
  hessian = TRUE,
  endyrvec = 2023,
  ylimAdj1 = 1.05,
  ylimAdj2 = 1.05,
  verbose = TRUE,
  ...
)
```

## Arguments

- mods:

  A list of model objects as created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)
  grouped together as a list(), or by
  [`SSgetoutput()`](https://r4ss.github.io/r4ss/reference/SSgetoutput.md)

- legendlabels:

  Labels for use in the legend. NULL value will use the lingcod model id

- filename:

  file to write to (relative to the "figures" directory) NULL value will
  use a filename like `compare_[id1]_[id2].png`

- dir:

  Where to put the PNG file. If NULL then will send to the current
  graphics device.

- subplot1:

  Which of the
  [`SSplotComparisons()`](https://r4ss.github.io/r4ss/reference/SSplotComparisons.md)
  plots to use for the top panel. By default will be either spawning
  biomass without or with uncertainty (1 or 2) depending on whether
  input `hessian` is TRUE or FALSE.

- subplot2:

  Which of the
  [`SSplotComparisons()`](https://r4ss.github.io/r4ss/reference/SSplotComparisons.md)
  plots to use for the bottom panel. By default will be either fraction
  unfished without or with uncertainty (3 or 4) depending on whether
  input `hessian` is TRUE or FALSE.

- hessian:

  TRUE/FALSE whether to include hessian or not.

- endyrvec:

  final year to include in the figure passed to
  [`SSplotComparisons()`](https://r4ss.github.io/r4ss/reference/SSplotComparisons.md)

- ylimAdj1:

  adjustment to y-axis limit for the first plot, relative to highest
  point among all models shown

- ylimAdj2:

  adjustment to y-axis limit for the second plot, relative to highest
  point among all models shown

- verbose:

  A logical value specifying if output should be printed to the screen.

- dots:

  additional arguments that will get passed to
  [`SSplotComparisons()`](https://r4ss.github.io/r4ss/reference/SSplotComparisons.md)

## Author

Ian G. Taylor

## Examples

``` r
if (FALSE) { # \dontrun{
plot_twopanel_comparison(
  mods = list(
    model1,
    model2
  ),
  legendlabels = c("Model 1", "Model 2"),
)

plot_twopanel_comparison(
  mods = list(
    model1,
    model2
  ),
  legendlabels = c("Model 1", "Model 2"),
  endyrvec = c(2025, 2023), # models might have different ending years
  subplot1 = 10, # recruitment
  subplot2 = 12 # recdevs
)
} # }
```
