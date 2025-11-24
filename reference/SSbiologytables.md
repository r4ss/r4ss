# Deprecated function to create a table of biology for assessment reporting: length, weight, % mature, fecundity, and selectivity

Replaced by
[`table_biology()`](https://r4ss.github.io/r4ss/reference/table_biology.md).
Takes the object created by SS_output to create table for reporting for
West Coast groundfish. Works with Stock Synthesis versions 3.30.12 and
later.

## Usage

``` r
SSbiologytables(
  replist = NULL,
  printfolder = "tables",
  dir = "default",
  fleetnames = "default",
  selexyr = "default"
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- printfolder:

  The sub-directory under 'dir' (see below) in which the PNG files will
  be located. The default sub-directory is "plots". The directory will
  be created if it doesn not exist. If 'printfolder' is set to "", it is
  ignored and the PNG files will be located in the directory specified
  by 'dir'.

- dir:

  A file path to the directory of interest. The default value is
  `dir = NULL`, which leads to using the current working directory.

- fleetnames:

  Optional replacement for fleetnames used in data file.

- selexyr:

  The year to summarize selectivity, the default is the final model yr
  strings to use for each fleet name. Default="default".

## Value

A csv files containing biology and selectivity tables

## Author

Chantel Wetzel
