# read forecast file

read Stock Synthesis forecast file into list object in R

## Usage

``` r
SS_readforecast(
  file = "forecast.ss",
  Nfleets = NULL,
  Nareas = NULL,
  nseas = NULL,
  version = "3.30",
  readAll = FALSE,
  verbose = TRUE
)
```

## Arguments

- file:

  Filename either with full path or relative to working directory. See
  the formal arguments for a possible default filename.

- Nfleets:

  Number of fleets (not required in 3.30).

- Nareas:

  Number of areas (not required in 3.30).

- nseas:

  number of seasons (not required in 3.30).

- version:

  SS version number. Currently "3.24" or "3.30" are supported, either as
  character or numeric values (noting that numeric 3.30 = 3.3).
  `version = NULL` is no longer the default or an allowed entry. The
  default is `version = "3.30"`.

- readAll:

  Should the function continue even if Forecast = 0 or -1 (at which
  point SS stops reading)?

- verbose:

  A logical value specifying if output should be printed to the screen.

- forelist:

  list to which the data frame should be added

- nrows:

  number of rows in the data frame; if NULL, will be determined by
  finding the next line with str in it

- ncol:

  number of columns in the data frame

- col.names:

  column names for the data frame

- name:

  name of the data frame to be added to forelist

- comments:

  optional vector of comments to be used as row names; if NULL, row
  names will be generated as \#\_name_1, \#\_name_2, etc.

- str:

  string to be used to find the end of the data frame if nrows is NULL;
  default is "-9999" with partial matching so str = "-999" will also
  match "-9999".

## Author

Ian G. Taylor, Kelli F. Johnson, Kathryn L. Doering, Nathan R. Vaughan
