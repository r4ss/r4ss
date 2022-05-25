## Release summary

This is a resubmission. In this version I have:

* Added single quotes around the package name in the Description section
  of DESCRIPTION to avoid a false positive spell check error.

* Removed the VignetteBuilder field from DESCRIPTION

* Removed the URLs which caused a NOTE about "(possibly) invalid URLs".
  The URLs worked fine for me, so I'm not sure the source of the error.

* Removed the URLs which caused a NOTE about "should use \doi" because I
  can't get \doi to work and don't see it documented anywhere.

Checking with winbuilder, I get "Status: OK"

## R CMD check results
0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
