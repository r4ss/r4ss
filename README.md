# r4ss: R code for Stock Synthesis
[![Build Status](https://travis-ci.org/r4ss/r4ss.png?branch=master)](https://travis-ci.org/r4ss/r4ss)

Stock Synthesis is a fisheries stock assessment model written in ADMB by Dr. Rick Methot at the [NOAA Northwest Fisheries Science Center](http://www.nwfsc.noaa.gov/). The R code in the r4ss package is based on the original work of Dr. Ian Stewart begun around 2005 and released as an open source R package on CRAN in 2009. The package has benefited from contributions, suggestions, and bug reports from many collaborators and users.

*Code available on this website comes with no warranty or guarantee of accuracy. It merely represents an ongoing attempt to integrate output plotting, statistics and diagnostics. It is absolutely necessary that prior to use with a new application, the user checks the output manually to verify that there are no plotting or statistical bugs which could incorrectly represent the output files being analyzed.*

## Installation

The latest r4ss version on CRAN can be installed using a command like

```S
install.packages("r4ss")
```
To get notifications about r4ss, you can watch this GitHub project and/or join the r4ss email list: <https://groups.google.com/forum/#!forum/r4ss>

Additional information about r4ss at the old Google Code page, <https://code.google.com/p/r4ss/>, will be migrated over to GitHub in the future.

More frequent enhancements and bug fixes are posted to this GitHub project. The latest version of r4ss can be installed directly from Github at any time via the `devtools` package in R with the following commands:

```S
install.packages("devtools")
devtools::install_github("r4ss/r4ss")
```

Note: devtools will give this message: "*WARNING: Rtools is required to build R packages, but is not currently installed.*" However, Rtools is NOT required for installing r4ss via devtools, so ignore the warning.

Once you have installed the r4ss package, it can be loaded in the regular manner:

```S
library(r4ss)
````

There is now a basic Vignette, which can be viewed at <https://github.com/r4ss/r4ss/blob/master/vignettes/r4ss-intro-vignette.Rmd> or built locally on your computer using this command to install the package:
```S
devtools::install_github("r4ss/r4ss", build_vignettes = TRUE)
```


## Changes

See NEWS.md for (not very complete) log of changes, starting with v1.24.0.
