# Function to write formatted table similar to table written by gdata::write.fwf from data.frame or matrix This function does not accept columns or logical with factor

Function to write formatted table similar to table written by
gdata::write.fwf from data.frame or matrix This function does not accept
columns or logical with factor

## Usage

``` r
write_fwf4(
  x,
  file = "",
  append = FALSE,
  quote = FALSE,
  sep = " ",
  na = "NA",
  rownames = FALSE,
  colnames = TRUE,
  rowCol = NULL,
  justify = "left",
  width = NULL,
  eol = "\n",
  qmethod = c("escape", "double"),
  digits = 8,
  checkNA = TRUE,
  checkInfty = TRUE,
  checkError = TRUE
)
```

## Arguments

- x:

  data.frame or matrix the object to be written

- file:

  either a character string naming a file or a connection open for
  writing. "" indicates output to the console.

- append:

  logical, append to existing data in `file`

- quote:

  logical, quote data in output

- sep:

  character, separator between columns in output

- na:

  character, the string to use for missing values i.e. `NA` in the
  output

- rownames:

  logical, print row names

- colnames:

  logical, print column names

- rowCol:

  character, rownames column name

- justify:

  character, alignment of character columns; see
  [`format()`](https://rdrr.io/r/base/format.html)

- width:

  numeric, width of the columns in the output

- eol:

  the character(s) to print at the end of each line (row). For example,
  'eol="\r\n"' will produce Windows' line endings on a Unix-alike OS,
  and 'eol="\r"' will produce files as expected by Mac OS Excel 2004.

- qmethod:

  a character string specifying how to deal with embedded double quote
  characters when quoting strings. Must be one of '"escape"' (default),
  in which case the quote character is escaped in C style by a
  backslash, or '"double"', in which case it is doubled. You can specify
  just the initial letter.

- digits:

  Used for signif

- checkNA:

  logical if TRUE, function will stop when NA is found

- checkInfty:

  logical if TRUE, function will stop when Infinity is found

- checkError:

  logical if TRUE both, set checkNA and checkInftr TRUE

## Author

Yukio Takeuchi
