# Create HTML files to view figures in browser.

Writes a set of HTML files with tabbed navigation between them. Depends
on [`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md)
with settings in place to write figures to PNG files. Should open main
file in default browser automatically.

## Usage

``` r
SS_html(
  replist = NULL,
  plotdir = NULL,
  plotInfoTable = NULL,
  title = "SS Output",
  width = 500,
  openfile = TRUE,
  multimodel = FALSE,
  filenotes = NULL,
  verbose = TRUE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- plotdir:

  Directory where PNG files will be written.

- plotInfoTable:

  CSV file with info on PNG files. By default, the `plotdir` directory
  will be searched for files with name beginning 'plotInfoTable\*'

- title:

  Title for HTML page.

- width:

  Width of plots (in pixels).

- openfile:

  Automatically open index.html in default browser?

- multimodel:

  Override errors associated with plots from multiple model runs. Only
  do this if you know what you're doing.

- filenotes:

  Add additional notes to home page.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Note

By default, this function will look in the directory where PNG files
were created for CSV files with the name 'plotInfoTable...' written by
'SS_plots. HTML files are written to link to these plots and put in the
same directory. Please provide feedback on any bugs, annoyances, or
suggestions for improvement.

## See also

[`SS_plots()`](https://r4ss.github.io/r4ss/reference/SS_plots.md),
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md)

## Author

Ian Taylor
