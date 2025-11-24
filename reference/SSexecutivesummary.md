# Deprecated function to create executive summary tables from an SS3 Report.sso file

Note: this function has been replaced by
[`table_exec_summary()`](https://r4ss.github.io/r4ss/reference/table_exec_summary.md)
Take the output from
[`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md) and
create executive summary .csv files as required by the current Terms of
Reference for U.S. West Coast groundfish assessments. Additionally, .csv
files of historical catches, time-series, and numbers-at-age are
created.

## Usage

``` r
SSexecutivesummary(
  replist,
  plotfolder = "default",
  ci_value = 0.95,
  es_only = FALSE,
  fleetnames = NULL,
  add_text = "model area",
  so_units = "millions of eggs",
  tables = lifecycle::deprecated(),
  divide_by_2 = FALSE,
  endyr = NULL,
  adopted_ofl = NULL,
  adopted_abc = NULL,
  adopted_acl = NULL,
  forecast_ofl = NULL,
  forecast_abc = NULL,
  format = lifecycle::deprecated(),
  match_digits = lifecycle::deprecated(),
  verbose = TRUE
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- plotfolder:

  Directory where a new `tables` directory will be created, which will
  be used to store the output from this function. The default is the dir
  location where the Report.sso file is located.

- ci_value:

  To calculate confidence intervals, the desired interval must be
  specified. The default is 0.95.

- es_only:

  A logical that specifies if only the executive summary tables should
  be produced. The default is `FALSE`, which leads to all executive
  summary and auxiliary tables being produced (see Return).

- fleetnames:

  Optional replacement for fleetnames used in data file.

- add_text:

  A single character object, where the default is `"model area"`. The
  text will be added to some of the table captions to indicate what the
  results apply to. Besides the default, one could use `"base model"`,
  `"sub-area model South of Point Conception."`, etc. Just know that the
  text will be appended to `"for the"`, and thus, the default text leads
  to `"for the model area."`. Another thing to note is that a full stop
  is not needed but can be used because a full stop is appended to the
  end of the caption if it does not already exist.

- so_units:

  A single character object specifying the unit of measurement that
  spawning output is reported in. The default is "millions of eggs".
  This text will be used in the table captions. If fecundity is equal to
  weight-at-length, then the units are hard-wired to `"mt"` regardless
  of what is used within this argument.

- tables:

  Deprecated as of version 1.49.1 because this function only takes 15
  seconds to run and overwriting old tables should not be a problem if
  users are modifying the .csv files in a programmatic way. The function
  behavior is the same as the previous default behavior where all tables
  will be created.

- divide_by_2:

  A logical allowing the output to be based on single sex values based
  on the new sex specification (-1) in SS3 for single sex models.
  Default value is `FALSE`. `TRUE` will lead to dividing values by 2.

- endyr:

  Optional input to choose a different ending year for tables, which
  could be useful for catch-only updates. The default is `NULL`, which
  leads to using the ending year defined in Report.sso.

- adopted_ofl, adopted_abc, adopted_acl:

  Vectors of adopted overfishing limits (OFL), acceptable biological
  catch (ABC), and annual catch limits (ACL) values to be printed in the
  management performance table. These vectors *MUST BE* be vectors of
  length 10. The default of `NULL` leads to the table being filled in
  with notes that the values need to be changed.

- forecast_ofl, forecast_abc:

  Optional input vectors for management adopted OFL and ABC values for
  table g. These values will overwrite the OFL and ABC values in the
  projection table, rather than the model estimated OFL values. As an
  example, `c(1500, 1300)` would be viable input.

- format:

  Deprecated as of version 1.49.1 because most users are now using LaTeX
  instead of microsoft word so formatting can be done inside
  `sa4ss::es_table_tex()` rather than here. From now on, only .csv files
  will be available. The default was `TRUE` but is now essentially
  `FALSE`.

- match_digits:

  Deprecated as of version 1.49.1 because this function just returns an
  unformatted csv file now.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Value

Individual .csv files for each executive summary table and additional
tables (catch, timeseries, numbers-at-age).

## Author

Chantel R. Wetzel, Kelli F. Johnson, Ian G. Taylor
