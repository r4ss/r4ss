# r4ss: R code for Stock Synthesis
[![Build Status](https://travis-ci.org/r4ss/r4ss.png?branch=master)](https://travis-ci.org/r4ss/r4ss)

Stock Synthesis is a fisheries stock assessment model written in ADMB by Dr. Rick Methot at the [NOAA Northwest Fisheries Science Center](http://www.nwfsc.noaa.gov/). The R code in the r4ss package is based on the original work of Dr. Ian Stewart begun around 2005 and released as an open source R package on CRAN in 2009. The package has benefited from contributions, suggestions, and bug reports from many collaborators and users.

*Code available on this website comes with no warranty or guarantee of accuracy. It merely represents an ongoing attempt to integrate output plotting, statistics and diagnostics. It is absolutely necessary that prior to use with a new application, the user checks the output manually to verify that there are no plotting or statistical bugs which could incorrectly represent the output files being analyzed.*

## Installation

The version of r4ss on CRAN is currently out of date. Therefore, installing directly from GitHub is recommended. This requires installing the `devtools` package first.

```S
install.packages("devtools")
devtools::install_github("r4ss/r4ss")
```

Note: devtools may give this message: "*WARNING: Rtools is required to build R packages, but is not currently installed.*" However, Rtools is NOT required for installing r4ss via devtools, so ignore the warning.

Ongoing development of r4ss has been mostly taking place in the "Development" branch on GitHub. That branch may be less stable, but can provide fixes, especially to issues related to beta releases of new SS versions. The development branch can be installed using the command:

```S
devtools::install_github("r4ss/r4ss", ref="development")
```



Once you have installed the r4ss package, it can be loaded using:

```S
library(r4ss)
````

To get notifications about r4ss, you can watch this GitHub project or follow messages on the forums on Stock Synthesis VLab (account required).

There is now a basic Vignette, which can be viewed at <https://github.com/r4ss/r4ss/blob/master/vignettes/r4ss-intro-vignette.Rmd> or built locally on your computer using this command to install the package:
```S
devtools::install_github("r4ss/r4ss", build_vignettes = TRUE)
```


## Changes

See NEWS.md for (not very complete) log of changes to r4ss, starting with r4ss v1.24.0 from 2014. The list of commits at <https://github.com/r4ss/r4ss/commits/master> provides a much more detailed list.


## Reporting problems

Please report any issues with this package by posting a new github issue at <https://github.com/r4ss/r4ss/issues>. You can also write to Ian.Taylor@noaa.gov.
