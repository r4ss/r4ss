# Open png device and return info on the file being created

this was previously contained within each of the SSplotXXX() functions.
It (1) translates the not-quite-matching specifications for the image to
the values needed by png(), then (2) returns the plotinfo data.frame
(which exists within each function which calls this) after adding a row
with the filename and caption for each new plot Note: this just opens
the png device which needs to be closed via dev.off() outside this
function.

## Usage

``` r
save_png(
  plotinfo,
  file,
  plotdir,
  pwidth,
  pheight,
  punits,
  res,
  ptsize,
  caption = NA,
  alt_text = NA,
  filenameprefix = NA
)
```

## Arguments

- plotinfo:

  table of information about all plots

- file:

  filename to write to (including .png extension)

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

- caption:

  caption for the image

- alt_text:

  alternative text for screen readers (if left as NA then will be set by
  SS_html() based on the caption)

- filenameprefix:

  Additional text to append to PNG or PDF file names. It will be
  separated from default name by an underscore.

## Author

Ian G. Taylor
