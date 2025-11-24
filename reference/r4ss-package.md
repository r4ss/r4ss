# r4ss: R Code for Stock Synthesis

A collection of R functions for use with Stock Synthesis, a fisheries
stock assessment modeling platform written in ADMB by Dr. Richard D.
Methot at the NOAA Northwest Fisheries Science Center. The functions
include tools for summarizing and plotting results, manipulating files,
visualizing model parameterizations, and various other common stock
assessment tasks. This version of 'r4ss' is compatible with Stock
Synthesis versions 3.24 through 3.30 (specifically version
3.30.24.00-prerel, from July 2025). Support for 3.24 models is only
through the core functions for reading output and plotting.

## See also

Useful links:

- <https://github.com/r4ss/r4ss>

- <https://r4ss.github.io/r4ss/>

- Report bugs at <https://github.com/r4ss/r4ss/issues>

## Author

**Maintainer**: Ian G. Taylor <Ian.Taylor@noaa.gov>

Authors:

- Ian J. Stewart

- Allan C. Hicks

- Tommy M. Garrison

- Andre E. Punt

- R.I.C. Chris Francis

- John R. Wallace

- Chantel R. Wetzel

- James T. Thorson

- Yukio Takeuchi

- Kotaro Ono

- Cole C. Monnahan

- Christine C. Stawitz

- Z. Teresa A'mar

- Athol R. Whitten

- Kelli F. Johnson

- Robbie L. Emmet

- Sean C. Anderson

- Gwladys I. Lambert

- Megan M. Stachura

- Andrew B. Cooper

- Andi Stephens

- Neil L. Klaer

- Carey R. McGilliard

- Iago Mosqueira

- Watal M. Iwasaki

- Kathryn L. Doering

- Andrea M. Havron

- Nathan R. Vaughan

- LaTreese S. Denson

- Ashleigh J. Novak

- Henning Winker

- Lee Qi

- Megumi Oshima

- Eric Fletcher

- Elizabeth F. Gugliotti

- Kiva L. Oken

- Arni Magnusson

## Examples

``` r
if (FALSE) { # \dontrun{
# it's useful to create a variable for the directory with the model output
mydir <- file.path(
  path.package("r4ss"),
  file.path("extdata", "simple_small")
)

# read the model output and print diagnostic messages
replist <- SS_output(
  dir = mydir,
  verbose = TRUE,
  printstats = TRUE
)

# plots the results
SS_plots(replist)
} # }
```
