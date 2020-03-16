# r4ss: R code for Stock Synthesis
[![Build Status](https://travis-ci.org/r4ss/r4ss.png?branch=master)](https://travis-ci.org/r4ss/r4ss) (master)
[![Build Status](https://travis-ci.org/r4ss/r4ss.svg?branch=development)](https://travis-ci.org/r4ss/r4ss) (development)
[![codecov](https://codecov.io/gh/r4ss/r4ss/branch/development/graph/badge.svg)](https://codecov.io/gh/r4ss/r4ss) (development)

Stock Synthesis is a fisheries stock assessment model written in ADMB by Rick Methot. The Stock Synthesis software and many other associated materials are available on the NOAA Virtual Laboratory at [https://vlab.ncep.noaa.gov/web/stock-synthesis/home](https://vlab.ncep.noaa.gov/web/stock-synthesis/home). The r4ss package is a collection of R functions for interacting with Stock Synthesis. It is based on the original work of Ian Stewart begun around 2005 and released as an open source R package in 2009. The package has a long list of authors and has benefited from a large community of users making suggestions and reporting issues.

*Code available on this website comes with no warranty or guarantee of accuracy. It merely represents an ongoing attempt to integrate output plotting, statistics and diagnostics. It is absolutely necessary that prior to use with a new application, the user checks the output manually to verify that there are no plotting or statistical bugs which could incorrectly represent the output files being analyzed.*

## Installation

r4ss can be downloaded from CRAN using:
```S
install.packages("r4ss")
```

This is equivalent to installing the master branch directly from GitHub:

```S
install.packages("remotes")
remotes::install_github("r4ss/r4ss")
```

Ongoing development of r4ss has been mostly taking place in the "development" branch on GitHub. The development branch may be less stable, but can provide the most recent features and bug fixes. The development branch can be installed using the command:

```S
remotes::install_github("r4ss/r4ss", ref="development")
```

Once you have installed the r4ss package, it can be loaded using:

```S
library(r4ss)
````

To get notifications about r4ss, you can watch this GitHub project or follow messages on the [forums on Stock Synthesis VLab](https://vlab.ncep.noaa.gov/web/stock-synthesis/public-forums). Note that to subscribe to the Stock Synthesis VLab forums, an account is required, but those without an account may still view and post forum messages.

A basic vignette can be viewed at <https://cran.r-project.org/web/packages/r4ss/vignettes/r4ss-intro-vignette.html> or built locally on your computer using this command to install the package:
```S
remotes::install_github("r4ss/r4ss", build_vignettes = TRUE)
```


## Changes

See [NEWS.md](https://github.com/r4ss/r4ss/blob/master/NEWS.md) for a (not very complete) log of changes to r4ss, starting with r4ss v1.24.0 from 2014. The list of commits at <https://github.com/r4ss/r4ss/commits/master> provides a much more detailed list.


## Reporting problems

Please report any issues with this package by posting a new github issue at <https://github.com/r4ss/r4ss/issues>. You can also write to Ian.Taylor@noaa.gov.
