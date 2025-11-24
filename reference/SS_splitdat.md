# Split apart bootstrap data to make input file.

**\[deprecated\]**

`SS_splitdat()` is being deprecated because it is no longer needed in as
of Stock Synthesis 3.30.19, where the files are already split. If
needing to split older version of SS3, this could be done by reading in
the file to
[`SS_readdat()`](https://r4ss.github.io/r4ss/reference/SS_readdat.md),
specifying the section argument to grab the desired part of the file,
and then writing out the file using. \`SS_writedat()'.

## Usage

``` r
SS_splitdat(
  inpath = "working_directory",
  outpath = "working_directory",
  inname = "data.ss_new",
  outpattern = "BootData",
  number = FALSE,
  verbose = TRUE,
  fillblank = TRUE,
  MLE = TRUE,
  inputs = FALSE,
  notes = ""
)
```

## Arguments

- inpath:

  Directory containing the input file. By default the working directory
  given by getwd() is used. Default="working_directory".

- outpath:

  Directory into which the output file will be written.
  Default="working_directory".

- inname:

  File name of input data file to be split. Default="Data.SS_New".

- outpattern:

  File name of output data file. Default="BootData".

- number:

  Append bootstrap number to the file name chosen in `outpattern`?
  Default=F.

- verbose:

  A logical value specifying if output should be printed to the screen.

- fillblank:

  Replace blank lines with "#". Helps with running on linux.
  Default=TRUE.

- MLE:

  Grab the maximum likelihood values from the second block in
  Data.SS_New (instead of bootstrap values or copies of inputs)?
  Default=TRUE.

- inputs:

  Grab the copy of the input values values from the first block in
  Data.SS_New (instead of MLE or bootstrap values)? Default=F.

- notes:

  Notes to the top of the new file (comment indicator "#C" will be
  added). Default="".

## Author

Ian Taylor
