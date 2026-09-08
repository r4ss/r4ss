# Clean unnecessary Stock Synthesis output files

Lists or deletes temporary and output files produced by ADMB and Stock
Synthesis. Only files directly within `dir` are considered;
subdirectories are not searched or removed.

## Usage

``` r
clean_files(dir, action = c("list", "delete"))
```

## Arguments

- dir:

  Directory containing the Stock Synthesis output files.

- action:

  One or both of `"list"` and `"delete"`. If `"list"` is included,
  matching file names are returned. If `"delete"` is included, matching
  files are deleted.

## Value

A character vector containing the names of matching files. The result is
returned invisibly when `action` does not include `"list"`.

## See also

Other run functions:
[`copy_SS_inputs()`](https://r4ss.github.io/r4ss/reference/copy_SS_inputs.md),
[`jitter()`](https://r4ss.github.io/r4ss/reference/jitter.md),
[`populate_multiple_folders()`](https://r4ss.github.io/r4ss/reference/populate_multiple_folders.md),
[`profile()`](https://r4ss.github.io/r4ss/reference/profile.md),
[`retro()`](https://r4ss.github.io/r4ss/reference/retro.md),
[`run()`](https://r4ss.github.io/r4ss/reference/run.md),
[`tune_comps()`](https://r4ss.github.io/r4ss/reference/tune_comps.md)

## Examples

``` r
model_dir <- tempdir()
clean_files(model_dir, action = "list")
#> ℹ No matching files found to delete.
```
