# r4ss: R code for Stock Synthesis

[![call-r-cmd-check](https://github.com/r4ss/r4ss/actions/workflows/call-r-cmd-check.yml/badge.svg)](https://github.com/r4ss/r4ss/actions/workflows/call-r-cmd-check.yml) [![codecov](https://codecov.io/gh/r4ss/r4ss/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r4ss/r4ss)

Stock Synthesis is a fisheries stock assessment model written in ADMB by Rick Methot. The Stock Synthesis software and many other associated materials are available on the NOAA Virtual Laboratory at [https://vlab.noaa.gov/web/stock-synthesis/home](https://vlab.noaa.gov/web/stock-synthesis/home). The r4ss package is a collection of R functions for interacting with Stock Synthesis. It is based on the original work of Ian Stewart begun around 2005 and released as an open source R package in 2009. The package has a long list of authors and has benefited from a large community of users making suggestions and reporting issues.

*This code comes with no warranty or guarantee of accuracy. It merely represents an ongoing attempt to integrate output plotting, statistics and diagnostics. It is absolutely necessary that prior to use with a new application, the user checks the output manually to verify that there are no plotting or statistical bugs which could incorrectly represent the output files being analyzed.*

## Installation

The CRAN version of r4ss is out of date. For now, it is necessary to install the main branch directly from GitHub:

```S
install.packages("remotes")
remotes::install_github("r4ss/r4ss")
```

Ongoing development of r4ss used to take place in the "development" branch, but now is in the "main" branch. Thus, you should no longer need to reference a branch when installing from github unless you are seeking a specific feature that hasn't yet been merged into the main branch.

Once you have installed the r4ss package, it can be loaded using:

```S
library(r4ss)
```

To get notifications about r4ss, you can watch this GitHub project or follow messages on the [forums on Stock Synthesis VLab](https://vlab.noaa.gov/web/stock-synthesis/public-forums). Note that to subscribe to the Stock Synthesis VLab forums, an account is required, but those without an account may still view and post forum messages.

A basic vignette can be viewed at <https://r4ss.github.io/r4ss/vignettes/r4ss-intro-vignette.html> or built locally on your computer using this command to install the package:

```S
remotes::install_github("r4ss/r4ss", build_vignettes = TRUE)
```

## Citing r4ss

Please cite r4ss as:

Ian G. Taylor, Kathryn L. Doering, Kelli F. Johnson, Chantel R. Wetzel, Ian J. Stewart, 2021. Beyond visualizing catch-at-age models: Lessons learned from the r4ss package about software to support stock assessments, Fisheries Research, 239:105924. <https://doi.org/10.1016/j.fishres.2021.105924>.

## Changes

See [NEWS.md](https://github.com/r4ss/r4ss/blob/main/NEWS.md) for a (not very complete) log of changes to r4ss, starting with r4ss v1.24.0 from 2014. The list of commits at <https://github.com/r4ss/r4ss/commits/main> provides a much more detailed list.

## Contributing to r4ss

Interested in contributing to r4ss? We recognize contributions come in many forms, including but not limited to code, reporting issues, creating examples and/or documentation.

We strive to follow the [NMFS Fisheries Toolbox Contribution Guide](https://github.com/nmfs-fish-tools/Resources/blob/master/CONTRIBUTING.md). We also have included r4ss-specific code contribution information in the [Git workflow page of the r4ss wiki](https://github.com/r4ss/r4ss/wiki/Git-Workflow). Note that these are guidelines, not rules, and we are open to collaborations in other ways that may work better for you. Please feel free to reach out to us by opening an issue in this repository or by emailing the maintainer (call `maintainer("r4ss")` in R to view the current maintainer's name and email address).

Note that by contributing, you are expect to uphold the [code of conduct](#code-of-conduct).

## Reporting problems

Please report any issues with this package by posting a new github issue at <https://github.com/r4ss/r4ss/issues>. You can also write to Ian.Taylor@noaa.gov.

## Code of conduct

This project and everyone participating in it is governed by the [NMFS Fisheries Toolbox Code of Conduct](https://github.com/nmfs-fish-tools/Resources/blob/master/CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to [fisheries.toolbox@noaa.gov](mailto:fisheries.toolbox@noaa.gov). Note that the maintainers of r4ss do not have access to this email account, so unacceptable behavior of maintainers can also be reported here.

The NMFS Fisheries Toolbox Code of Conduct is adapted from the [Contributor Covenant][homepage], version 1.4,
available at <https://www.contributor-covenant.org/version/1/4/code-of-conduct.html>

[homepage]: <https://www.contributor-covenant.org>

For answers to common questions about this code of conduct, see
<https://www.contributor-covenant.org/faq>
