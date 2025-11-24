# Allow Multi-Plots Set the par() to options suitable for ss3diags multi plots.

See [par](https://rdrr.io/r/graphics/par.html) for more details on each
parameter.

## Usage

``` r
sspar(
  mfrow = c(1, 1),
  plot.cex = 1,
  mai = c(0.55, 0.6, 0.1, 0.1),
  omi = c(0, 0, 0, 0) + 0.1,
  labs = TRUE
)
```

## Arguments

- mfrow:

  determines plot frame set up

- plot.cex:

  cex graphic option

- mai:

  graphical par for plot margins

- omi:

  Outer margins in lines of text.

- labs:

  if TRUE margins are narrow
