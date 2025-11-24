# Convert composition data to long format

Convert age, length, or generalized size composition data from wide to
long format.

## Usage

``` r
comp2long(x, ...)

age2long(x, expand = FALSE, zero = TRUE)

size2long(x, measure = NULL, zero = TRUE)
```

## Arguments

- x:

  A data frame containing age, length, generalized size composition
  data, or a list containing such a data frame.

- ...:

  Passed to `age2long` or `size2long`.

- expand:

  Whether to repeat recurring entries in the resulting data frame, so
  that the `freq` column is `1` for every entry.

- zero:

  Whether zero frequencies should be included in the output.

- measure:

  An optional string indicating the type of measurement, e.g.,
  `"weight"`.

## Value

Data frame containing the data in long format.

## Details

The `age2long` function converts age compositions and `size2long`
converts length or generalized size compositions. Alternatively, the
wrapper function `comp2long` converts any composition data and will call
either `age2long` or `size2long`, depending on the data type of `x`.

If `x` is not a data frame, the function will look for a data frame
inside the list: first `lencomp`, then `agecomp`, and then
`sizefreq_data_list[[1]]`.

The `expand = TRUE` option is mainly useful for accessing conditional
age-at-length, e.g., to produce a data frame that has the same number of
rows as the number of otoliths.

## Note

In r4ss input data objects, age composition data are stored as
`agecomp`, length composition data as `lencomp`, and generalized size
composition data as `sizefreq_data_list[[i]]`.

## See also

[`SS_read`](https://r4ss.github.io/r4ss/reference/SS_read.md) and
[`SS_readdat`](https://r4ss.github.io/r4ss/reference/SS_readdat.md) read
composition data from files into r4ss list objects.

## Author

Arni Magnusson

## Examples

``` r
# Read composition data from example model
path <- system.file("extdata", "simple_small", package = "r4ss")
inputs <- SS_read(path)
dat <- SS_readdat(file.path(path, "data.ss"), verbose = FALSE)
x <- dat[["lencomp"]]
y <- dat[["agecomp"]]

# Main argument can be a list or data frame
head(comp2long(inputs))
#>   year month fleet sex part Nsamp length freq
#> 1 2011     7     1   1    0    50     26    0
#> 2 2011     7     1   1    0    50     28    0
#> 3 2011     7     1   1    0    50     30    0
#> 4 2011     7     1   1    0    50     32    0
#> 5 2011     7     1   1    0    50     34    0
#> 6 2011     7     1   1    0    50     36    0
head(comp2long(dat))
#>   year month fleet sex part Nsamp length freq
#> 1 2011     7     1   1    0    50     26    0
#> 2 2011     7     1   1    0    50     28    0
#> 3 2011     7     1   1    0    50     30    0
#> 4 2011     7     1   1    0    50     32    0
#> 5 2011     7     1   1    0    50     34    0
#> 6 2011     7     1   1    0    50     36    0
head(comp2long(x))
#>   year month fleet sex part Nsamp length freq
#> 1 2011     7     1   1    0    50     26    0
#> 2 2011     7     1   1    0    50     28    0
#> 3 2011     7     1   1    0    50     30    0
#> 4 2011     7     1   1    0    50     32    0
#> 5 2011     7     1   1    0    50     34    0
#> 6 2011     7     1   1    0    50     36    0
head(comp2long(y))
#>   year month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp age freq
#> 1 2011     7     1   1    0      2       1      -1    25   1    0
#> 2 2011     7     1   1    0      2       1      -1    25   2    1
#> 3 2011     7     1   1    0      2       1      -1    25   3    0
#> 4 2011     7     1   1    0      2       1      -1    25   4    0
#> 5 2011     7     1   1    0      2       1      -1    25   5    0
#> 6 2011     7     1   1    0      2       1      -1    25   6    2

# Rename second to last column
head(comp2long(x, measure = "weight"))
#>   year month fleet sex part Nsamp weight freq
#> 1 2011     7     1   1    0    50     26    0
#> 2 2011     7     1   1    0    50     28    0
#> 3 2011     7     1   1    0    50     30    0
#> 4 2011     7     1   1    0    50     32    0
#> 5 2011     7     1   1    0    50     34    0
#> 6 2011     7     1   1    0    50     36    0

# Shrink output by omitting zero frequencies
nrow(comp2long(x))
#> [1] 800
nrow(comp2long(x, zero = FALSE))
#> [1] 418
nrow(comp2long(y))
#> [1] 480
nrow(comp2long(y, zero = FALSE))
#> [1] 256

# Expand output by repeating recurring entries
nrow(comp2long(y, expand = TRUE))
#> [1] 400

# Aggregate by sex
aggregate(freq ~ sex, comp2long(x), sum)
#>   sex freq
#> 1   1  408
#> 2   2  392
aggregate(freq ~ sex, comp2long(y), sum)
#>   sex freq
#> 1   1  216
#> 2   2  184
```
