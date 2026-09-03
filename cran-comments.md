## Release summary

The r4ss package on CRAN (1.44.0, published in 2022) is no longer maintained
there. Active development has continued at <https://github.com/r4ss/r4ss>,
where the current package is substantially newer.

This submission replaces the outdated CRAN implementation with a minimal
compatibility package. On attachment it directs users to the maintained GitHub
repository, and `r4ss_repository()` returns the canonical repository URL. The
CRAN package has no third-party dependencies.

This intentionally removes the historical API from the CRAN distribution.
Reverse dependencies that require the analysis API should install the current
r4ss release from GitHub.

## R CMD check results

The package was checked on Ubuntu 24.04.3 LTS with R 4.5.1:

* `R CMD check --as-cran`: 0 errors | 0 warnings | 0 notes.

## Reverse dependencies

CRAN currently lists ss3sim as a reverse import and MSEtool as a reverse
suggestion. The actively maintained GitHub version of ss3sim already
points to the GitHub version of this package in the `Remotes` field the
DESCRIPTION file. An issue has been posted to MSEtool to suggest a 
similar change there.