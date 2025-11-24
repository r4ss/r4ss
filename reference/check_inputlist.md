# Check input argument `inputlist`

Check the elements of the `inputlist` list used as an argument in
[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md)
function.

## Usage

``` r
check_inputlist(inputlist)
```

## Arguments

- inputlist:

  List created by the
  [`SS_read()`](https://r4ss.github.io/r4ss/reference/SS_read.md)
  function with elements "dat", "ctl", "start", "fore", and (optionally)
  "wtatage" and "par".

## Value

Either TRUE if the input list is valid, or FALSE if not, with a warning
about which elements are missing.

## See also

[`SS_write()`](https://r4ss.github.io/r4ss/reference/SS_write.md)

## Author

Kelli F. Johnson, Ian G. Taylor
