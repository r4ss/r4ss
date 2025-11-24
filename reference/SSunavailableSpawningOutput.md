# Plot unavailable spawning output

Calculate and plot the unavailable spawning output- separating out ones
that are unavailable because they're too small to be selected from ones
that are too big to be selected

## Usage

``` r
SSunavailableSpawningOutput(
  replist,
  plot = TRUE,
  print = FALSE,
  plotdir = "default",
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1
)
```

## Arguments

- replist:

  A list object created by
  [`SS_output()`](https://r4ss.github.io/r4ss/reference/SS_output.md).

- plot:

  Plot to active plot device?

- print:

  Print to PNG files?

- plotdir:

  Directory where PNG files will be written.

- pwidth:

  Default width of plots printed to files in units of `punits`.

- pheight:

  Height of plots printed to png files in units of `punits`. Default is
  designed to allow two plots per page, with `pheight_tall` used for
  plots that work best with a taller format and a single plot per page.

- punits:

  Units for `pwidth` and `pheight`. Can be "px" (pixels), "in" (inches),
  "cm" (centimeters), or "mm" (millimeters). The default is
  `punits="in"`.

- res:

  Resolution of plots printed to files. The default is `res = 300`.

- ptsize:

  Point size for plotted text in plots printed to files (see
  [`help("png")`](https://rdrr.io/r/grDevices/png.html) in R for
  details).

- cex.main:

  Character expansion for plot titles. The default is `cex.main=1`.

## Author

Megan Stachura, Andrew Cooper, Andi Stephens, Neil Klaer, Ian G. Taylor
