# Customizing {r4ss} output and posting to github pages

## Overview

The html output from
[`r4ss::SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md)
can be posted to [GitHub Pages](https://pages.github.com/). This is an
easy way to share model results publicly. For example, [html output for
a big skate assessment](https://r4ss.github.io/r4ss/BigSkate/) is hosted
on Github Pages. In this vignette, we demonstrate how to add custom
plots created outside of {r4ss} to the html output and provide links to
documentation for hosting on Github.

## Creating plots

See the documentation details by calling
[`?r4ss::SS_plots`](https://r4ss.github.io/r4ss/reference/SS_plots.md)
in the R console. This provides information on how
[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md) works
and how the html output can be modified to customize the html output.

## Example: Creating plots from simple and adding a custom tab

### read model output

``` r

example_path <- system.file("extdata", package = "r4ss")
simple_small <- SS_output(file.path(example_path, "simple_small"),
  verbose = FALSE, printstats = FALSE
)
```

### make the default plots and associated HTML files

``` r

SS_plots(simple_small)
#> Finished defining objects
#> Plots will be written to PNG files in the directory:
#> /home/runner/work/_temp/Library/r4ss/extdata/simple_small/plots
#> Starting biology plots (group 1)
#> Starting selectivity and retention plots (group 2)
#> Starting timeseries plots (group 3)
#> Plotting Dynamic B0
#> Starting recruitment deviation plots (group 4)
#> Starting estimation of recruitment bias adjustment and associated plots (group
#> 5)
#> Starting spawner-recruit curve plot (group 6)
#> Starting catch plots (group 7)
#> Starting SPR plots (group 8)
#> Skipping discard plot (group 9) because no discard data
#> Skipping mean weight plot (group 10) because no mean weight data
#> Starting index plots (group 11)
#> Starting numbers at age plots (group 12)
#> skipped sex ratio contour plot because females=males for all ages and years
#> skipped sex ratio contour plot because females=males for all lengths and years
#> Starting length comp data plots (group 13)
#> Starting age comp data plots (group 14)
#> Starting conditional comp data plots (group 15)
#> Starting fit to length comp plots (group 16)
#> Starting fit to age comp plots (group 17)
#> Starting fit to conditional age-at-length comp plots (group 18)
#> Skipping conditional A@L plots (group 19) because no such data in model
#> Starting mean length-at-age and mean weight-at-age plots (group 20)
#> Skipping tag plots (group 21) because no tag data in model
#> Starting yield plots (group 22)
#> Skipping movement plots (group 23) because no movement in model
#> Starting data range plots (group 24)
#> Starting parameter distribution plots (group 25)
#> Excluding 22 deviation parameters because input 'showdev' = FALSE
#> Plotting distributions for 10 estimated parameters (deviations not included).
#> Finished all requested plots in SS_plots function
#> Starting diagnostic tables (group 26)
#> Wrote table of info on PNG files to:
#> /home/runner/work/_temp/Library/r4ss/extdata/simple_small/plots/plotInfoTable_27-05-2026_16.23.37.1432.csv
#> Running 'SS_html': By default, this function will look in the directory where
#> PNG files were created for CSV files with the name 'plotInfoTable...' written
#> by 'SS_plots.' HTML files are written to link to these plots and put in the
#> same directory.
#> Removing duplicate rows in combined plotInfoTable based on multiple CSV files
#> Home HTML file with output will be:
#> /home/runner/work/_temp/Library/r4ss/extdata/simple_small/plots/_SS_output.html
#> Opening HTML file in your default web-browser.
```

### Make custom plots and write CSV file with info about them

Note: all files need to be in the same directory.
[`SS_html()`](https://r4ss.github.io/r4ss/reference/SS_html.md) uses
[`basename()`](https://rdrr.io/r/base/basename.html) and
[`dirname()`](https://rdrr.io/r/base/basename.html) which prevent the
use of subdirectories to organize the files.

``` r

plotdir <- file.path(example_path, "simple_small/plots/")

SSplotComparisons(
  SSsummarize(list(simple_small, simple_small)),
  print = TRUE, plot = FALSE, plotdir = plotdir
)
#> Summarizing 2 models:
#> imodel=1/2
#> N active pars = 32
#> imodel=2/2
#> N active pars = 32
#> Summary finished. To avoid printing details above, use 'verbose = FALSE'.
#> showing uncertainty for all models
#> subplot 1: spawning biomass
#> subplot 2: spawning biomass with uncertainty intervals
#> subplot 3: biomass ratio (hopefully equal to fraction of unfished)
#> subplot 4: biomass ratio with uncertainty
#> subplot 18: summary biomass
#> skipping subplot 19 summary biomass with uncertainty because no models include
#> summary biomass as a derived quantity
#> subplot 5: SPR ratio
#> subplot 6: SPR ratio with uncertainty
#> subplot 7: F value
#> subplot 8: F value with uncertainty
#> subplot 9: recruits
#> subplot 10: recruits with uncertainty
#> subplot 11: recruit devs
#> subplot 12: recruit devs with uncertainty
#> subplot 13: index fits
#> subplot 14: index fits on a log scale
#> subplot 15: phase plot
#> subplots 16 and 17: densities
#> Parameter/quantity names matching 'densitynames' input: SSB_Virgin, SR_LN(R0)
#> x-axis for SSB_Virgin in density plot has been divided by 1000 (so may be in
#> units of '1000 t)
#> x-axis for SSB_Virgin in density plot has been divided by 1000 (so may be in
#> units of '1000 t)

data.frame(
  file = dir(plotdir) |> grep(pattern = "^compare[0-9]", value = TRUE),
  caption = "add caption",
  alt_text = "add alt-text",
  category = "comparisons",
  png_time = NA,
  StartTime = simple_small$StartTime
) |>
  write.csv(
    file = file.path(plotdir, "plotInfoTable_comparisons.csv"),
    row.names = FALSE
  )
```

### Add custom plots to the .csv

``` r

SS_html(simple_small,
  filenotes = "default plots plus comparisons",
  plotdir = file.path(example_path, "simple_small/plots"),
  verbose = TRUE
)
#> Running 'SS_html': By default, this function will look in the directory where
#> PNG files were created for CSV files with the name 'plotInfoTable...' written
#> by 'SS_plots.' HTML files are written to link to these plots and put in the
#> same directory.
#> Removing duplicate rows in combined plotInfoTable based on multiple CSV files
#> Home HTML file with output will be:
#> /home/runner/work/_temp/Library/r4ss/extdata/simple_small/plots/_SS_output.html
#> Opening HTML file in your default web-browser.
```

## Getting the plots onto github

Instructions for using Github Pages are thoroughly documented in the
[Github Documentation](https://docs.github.com/en/pages/quickstart).
