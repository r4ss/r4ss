r4ss
====

### R code for Stock Synthesis

The r4ss package version 1.22.1 is on CRAN and can be installed in R using a command like

```S
install.packages("r4ss")
```
To get notifications about r4ss, you can watch this GitHub project and/or join the r4ss email list: <https://groups.google.com/forum/#!forum/r4ss>

Additional information about r4ss at the old Google Code page, <https://code.google.com/p/r4ss/>, will be migrated over to GitHub in the future.

The latest development version of r4ss can be installed directly from Github at any time via the `devtools` package in R and with the following commands:

```S
install.packages("devtools")
devtools::install_github("r4ss/r4ss")
```

Once you have installed the r4ss package, it can be loaded in the regular manner:

```S
library(r4ss)
````

From version 1.22 onwards, it should also be possible to install previous versions of the r4ss package using, for example:

```S
devtools::install_github("r4ss/r4ss", ref="v1.22.1")
````