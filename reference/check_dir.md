# Directory check

Directory check

## Usage

``` r
check_dir(dir, verbose = TRUE)
```

## Arguments

- dir:

  A file path to the directory of interest. When omitted, functions
  typically default to the current working directory or the model
  directory stored in `replist`.

- verbose:

  A logical value specifying if output should be printed to the screen.

## Details

Check that

1.  The user knows that the data will not be saved if `dir = NULL`.

2.  The directory exists if it can be created.

3.  The function fails if the directory cannot be created.

Note: this function was copied from the nwfscSurvey package:
https://github.com/pfmc-assessments/nwfscSurvey/blob/main/R/check_dir.R
rather than adding an additional dependency on that package.

## Author

Chantel R. Wetzel

## Examples

``` r
check_dir(getwd(), verbose = FALSE)
# See more output
check_dir(getwd())
```
